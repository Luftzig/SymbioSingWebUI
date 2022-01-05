module Notation exposing
    ( ConversionParameters
    , HapticNote(..)
    , HapticPart
    , HapticScore
    , IntermediateAction(..)
    , IntermediateActionForce(..)
    , IntermediateRepr
    , Measure
    , NotationParserConfiguration
    , PartID
    , PartialMeasure
    , Signature
    , configurationToMap
    , defaultConfiguration
    , noteToNumber
    , parseMusicXml
    , parseMusicXmlWith
    , resultMapList
    , scoreToSchedule
    , tagPath
    , toTuple2
    , forceToPwmVal)

import Array exposing (Array)
import Dict exposing (Dict)
import Dict.Extra as Dict
import Extra.TypedTime as TypedTime exposing (TypedTime)
import FlowIO exposing (FlowIOAction, FlowIOCommand, PortState, portToIndex)
import List.Extra
import Result.Extra as Result
import Scheduler
import Set
import Xml exposing (Value(..))
import Xml.Decode exposing (decode)
import Xml.Query exposing (int, string, tag, tags)


type alias HapticScore =
    Dict PartID HapticPart


type alias PartID =
    String


type alias HapticPart =
    { name : String
    , measures : List Measure
    }


type alias Measure =
    { number : Int
    , signature : Signature
    , divisionsPerQuarter : Int
    , notes : List HapticNote
    }


type alias PartialMeasure =
    { number : Int
    , signature : Maybe Signature
    , divisionsPerQuarter : Maybe Int
    , notes : List HapticNote
    }


type alias Signature =
    { beats : Int
    , beatType : Timing
    }


type HapticNote
    = Rest Timing
    | FullInflate Timing
    | FastInflate Timing
    | SlowInflate Timing
    | SlowestInflate Timing
    | FullRelease Timing
    | FastRelease Timing
    | SlowRelease Timing
    | SlowestRelease Timing


type alias Timing =
    Int


type alias NotationParserConfiguration =
    { fullInflatePitch : Int
    , fastInflatePitch : Int
    , slowInflatePitch : Int
    , slowestInflatePitch : Int
    , fullReleasePitch : Int
    , fastReleasePitch : Int
    , slowReleasePitch : Int
    , slowestReleasePitch : Int
    }


noteToNumber step octave alter =
    let
        stepToNumber s =
            case String.trim <| String.toUpper s of
                "C" ->
                    Just 0

                "D" ->
                    Just 2

                "E" ->
                    Just 4

                "F" ->
                    Just 5

                "G" ->
                    Just 7

                "A" ->
                    Just 9

                "B" ->
                    Just 11

                _ ->
                    Nothing
    in
    stepToNumber step
        |> Maybe.map (\stepNum -> (12 * (octave + 1)) + stepNum + alter)


defaultConfiguration : NotationParserConfiguration
defaultConfiguration =
    { fullInflatePitch = 48 -- noteToNumber "C" 3 0
    , fastInflatePitch = 47 -- noteToNumber "B" 2 0
    , slowInflatePitch = 46 -- noteToNumber "B" 2 -1
    , slowestInflatePitch = 45 -- noteToNumber "A" 2 0
    , slowestReleasePitch = 44 -- noteToNumber "A" 2 -1
    , slowReleasePitch = 43 -- noteToNumber "G" 2 0
    , fastReleasePitch = 42 -- noteToNumber "G" 2 -1
    , fullReleasePitch = 41 -- noteToNumber "F" 2 0
    }


configurationToMap : NotationParserConfiguration -> Dict Int (Timing -> HapticNote)
configurationToMap conf =
    Dict.fromList
        [ ( conf.fullInflatePitch, FullInflate )
        , ( conf.fastInflatePitch, FastInflate )
        , ( conf.slowInflatePitch, SlowInflate )
        , ( conf.slowestInflatePitch, SlowestInflate )
        , ( conf.slowestReleasePitch, SlowestRelease )
        , ( conf.slowReleasePitch, SlowRelease )
        , ( conf.fastReleasePitch, FastRelease )
        , ( conf.fullReleasePitch, FullRelease )
        ]


toTuple2 : (a -> b) -> (a -> c) -> a -> ( b, c )
toTuple2 f g v =
    ( f v, g v )


resultMapList : (a -> b) -> Result e (List a) -> Result e (List b)
resultMapList f =
    Result.map (List.map f)


tagPath : List String -> (Value -> Result String a) -> Value -> Result String a
tagPath path decoder value =
    case path of
        [] ->
            decoder value

        p :: rest ->
            tag p (tagPath rest decoder) value


parseMusicXml : String -> Result String HapticScore
parseMusicXml =
    parseMusicXmlWith defaultConfiguration


parseMusicXmlWith : NotationParserConfiguration -> String -> Result String HapticScore
parseMusicXmlWith configuration input =
    let
        pitchToNoteMap =
            configurationToMap configuration

        decodeAttribute : String -> (Value -> Result String a) -> Value -> Result String a
        decodeAttribute attrName decoder value =
            case value of
                Tag _ attrs _ ->
                    Dict.get attrName attrs
                        |> Result.fromMaybe ("Attribute \"" ++ attrName ++ "\" does not exists")
                        |> Result.andThen decoder

                _ ->
                    Result.Err "Expected Tag value"

        document =
            decode input

        decodePartName : Value -> Result String String
        decodePartName =
            tag "part-name" string

        scoreParts : Result String (Dict PartID HapticPart)
        scoreParts =
            document
                |> Result.map (tags "score-part")
                |> Result.map (List.map (toTuple2 (decodeAttribute "id" string) decodePartName))
                |> Result.map (List.map Result.combineBoth)
                |> Result.andThen Result.combine
                |> Result.map (List.map (Tuple.mapSecond (\name -> { name = name, measures = [] })))
                |> Result.map Dict.fromList

        parts : Result String (Dict PartID (List Measure))
        parts =
            let
                toMeasures : ( String, Value ) -> Result String ( String, List Measure )
                toMeasures =
                    Result.combineMapSecond measuresDecoder
            in
            document
                |> Result.map (tags "part")
                |> resultMapList (\part -> Result.combineFirst ( decodeAttribute "id" string part, part ))
                |> Result.andThen Result.combine
                |> resultMapList toMeasures
                |> Result.map Result.combine
                |> Result.join
                |> Result.map Dict.fromList

        measuresDecoder : Value -> Result String (List Measure)
        measuresDecoder value =
            let
                measureValues =
                    tags "measure" value

                partialValues =
                    measureValues
                        |> List.map measureDecoder
                        |> Result.combine

                completeMeasures : PartialMeasure -> List PartialMeasure -> Result String (List Measure)
                completeMeasures init rest =
                    let
                        completeMeasures_ : Int -> Signature -> List PartialMeasure -> List Measure
                        completeMeasures_ initDivisions initSignature pMeasures =
                            case pMeasures of
                                [] ->
                                    []

                                m :: rest_ ->
                                    { number = m.number
                                    , signature = Maybe.withDefault initSignature m.signature
                                    , divisionsPerQuarter = Maybe.withDefault initDivisions m.divisionsPerQuarter
                                    , notes = m.notes
                                    }
                                        :: completeMeasures_ (Maybe.withDefault initDivisions m.divisionsPerQuarter)
                                            (Maybe.withDefault initSignature m.signature)
                                            rest_
                    in
                    case ( init.divisionsPerQuarter, init.signature ) of
                        ( Just div, Just sig ) ->
                            Result.Ok
                                ({ number = init.number, signature = sig, divisionsPerQuarter = div, notes = init.notes }
                                    :: completeMeasures_ div sig rest
                                )

                        _ ->
                            Result.Err "First measure must specify time signature and divisions"
            in
            partialValues
                |> Result.andThen
                    (\pValues ->
                        case pValues of
                            [] ->
                                Ok []

                            initial :: rest ->
                                completeMeasures initial rest
                    )

        measureDecoder : Value -> Result String PartialMeasure
        measureDecoder value =
            let
                measureNum =
                    decodeAttribute "number" int

                signature : Result String Signature
                signature =
                    tagPath [ "attributes", "time" ] signatureDecoder value

                divisions : Result String Int
                divisions =
                    tagPath [ "attributes", "divisions" ] int value

                notes : Result String (List HapticNote)
                notes =
                    value
                        |> tags "note"
                        |> List.map notesDecoder
                        |> Result.combine
            in
            ( measureNum value, notes )
                |> Result.combineBoth
                |> Result.map
                    (\( num, notes_ ) ->
                        { number = num
                        , signature = Result.toMaybe signature
                        , divisionsPerQuarter = Result.toMaybe divisions
                        , notes = notes_
                        }
                    )

        signatureDecoder : Value -> Result String Signature
        signatureDecoder value =
            let
                beats =
                    tag "beats" int value

                beatsType =
                    value
                        |> tag "beat-type" int
            in
            ( beats, beatsType )
                |> Result.combineBoth
                |> Result.map (\( beats_, beatsType_ ) -> { beats = beats_, beatType = beatsType_ })

        notesDecoder : Value -> Result String HapticNote
        notesDecoder value =
            let
                duration : Result String Timing
                duration =
                    value
                        |> tag "duration" int

                hasRest =
                    value
                        |> tag "rest" (\_ -> Ok True)
                        |> Result.isOk

                step : Result String String
                step =
                    value
                        |> tagPath [ "pitch", "step" ] string

                octave : Result String Int
                octave =
                    value |> tagPath [ "pitch", "octave" ] int

                alter : Maybe Int
                alter =
                    value
                        |> tagPath [ "pitch", "alter" ] int
                        |> Result.toMaybe

                getAction : Timing -> Result String String -> Result String Int -> Maybe Int -> Result String HapticNote
                getAction timing resStep resOctave maybeAlter =
                    Result.map2
                        (\stepVal octaveVal ->
                            noteToNumber stepVal octaveVal (maybeAlter |> Maybe.withDefault 0)
                                |> Maybe.andThen (\n -> Dict.get n pitchToNoteMap)
                        )
                        resStep
                        resOctave
                        |> Result.andThen
                            (Result.fromMaybe
                                ("value "
                                    ++ (resStep
                                            |> Result.withDefault
                                                "<missing-step>"
                                       )
                                    ++ (resOctave |> Result.map String.fromInt |> Result.withDefault "<missing-octave>")
                                    ++ (maybeAlter
                                            |> Maybe.map
                                                (\n ->
                                                    if n > 0 then
                                                        "+" ++ String.fromInt n

                                                    else
                                                        String.fromInt n
                                                )
                                            |> Maybe.withDefault ""
                                       )
                                    ++ " cannot be matched with action"
                                )
                            )
                        |> Result.andMap (Ok timing)
            in
            case duration of
                Ok duration_ ->
                    if hasRest then
                        Ok (Rest duration_)

                    else
                        getAction duration_ step octave alter

                Err e ->
                    Err e

        merge : Dict PartID HapticPart -> Dict PartID (List Measure) -> Result String (Dict PartID HapticPart)
        merge names measures =
            Dict.merge
                (\key _ _ -> Err ("Part " ++ key ++ " has name but no data"))
                (\key part ms old -> Result.map (Dict.insert key { part | measures = ms }) old)
                (\key _ _ -> Err ("Part " ++ key ++ " has measures but no name"))
                names
                measures
                (Ok Dict.empty)
    in
    Result.combineBoth ( scoreParts, parts )
        |> Result.andThen
            (\( partNames, partValues ) ->
                merge
                    partNames
                    partValues
            )


type alias ConversionParameters =
    { bpm : Int
    , roleMapping : Dict PartID ( Scheduler.RoleName, FlowIO.Port )
    }


type IntermediateAction
    = Inflate
    | Release
    | NoChange


type IntermediateActionForce
    = Full
    | High
    | Medium
    | Low
    | Irrelevant


forceToPwmVal force =
    case force of
        Full ->
            255

        High ->
            200

        Medium ->
            145

        Low ->
            90

        Irrelevant ->
            0


type alias IntermediateRepr =
    { startTimeMs : TypedTime
    , action : IntermediateAction
    , force : IntermediateActionForce
    , measureNumber : Int
    }


scoreToSchedule : ConversionParameters -> HapticScore -> Result String Scheduler.Instructions
scoreToSchedule { bpm, roleMapping } hapticScore =
    let
        absoluteTimeLines : List Measure -> List IntermediateRepr
        absoluteTimeLines measures =
            measures
                |> List.concatMap measureToIntermediate
                |> List.Extra.mapAccuml durationsToAbsolutes TypedTime.zero
                |> (\( lastTime, intermediates ) ->
                        let
                            lastMeasure =
                                List.Extra.last intermediates |> Maybe.map .measureNumber
                        in
                        List.append intermediates
                            [ { startTimeMs = lastTime
                              , action = NoChange
                              , force = Irrelevant
                              , measureNumber = lastMeasure |> Maybe.map ((+) 1) |> Maybe.withDefault 0
                              }
                            ]
                   )

        measureToIntermediate :
            Measure
            ->
                List
                    { duration : TypedTime
                    , action : IntermediateAction
                    , force : IntermediateActionForce
                    , measureNumber : Int
                    }
        measureToIntermediate { divisionsPerQuarter, notes, number } =
            let
                durationPerDivision =
                    let
                        bpmTimesDivisions =
                            toFloat (bpm * divisionsPerQuarter)
                    in
                    TypedTime.seconds 60
                        |> TypedTime.divide bpmTimesDivisions

                toDuration : Timing -> TypedTime
                toDuration d =
                    durationPerDivision |> TypedTime.multiply (toFloat d)

                noteToIntermediate :
                    HapticNote
                    -> { duration : TypedTime, action : IntermediateAction, force : IntermediateActionForce, measureNumber : Int }
                noteToIntermediate hapticNote =
                    case hapticNote of
                        Rest t ->
                            { duration = toDuration t, action = NoChange, force = Irrelevant, measureNumber = number }

                        FullInflate t ->
                            { duration = toDuration t, action = Inflate, force = Full, measureNumber = number }

                        FastInflate t ->
                            { duration = toDuration t, action = Inflate, force = High, measureNumber = number }

                        SlowInflate t ->
                            { duration = toDuration t, action = Inflate, force = Medium, measureNumber = number }

                        SlowestInflate t ->
                            { duration = toDuration t, action = Inflate, force = Low, measureNumber = number }

                        FullRelease t ->
                            { duration = toDuration t, action = Release, force = Full, measureNumber = number }

                        FastRelease t ->
                            { duration = toDuration t, action = Release, force = High, measureNumber = number }

                        SlowRelease t ->
                            { duration = toDuration t, action = Release, force = Medium, measureNumber = number }

                        SlowestRelease t ->
                            { duration = toDuration t, action = Release, force = Low, measureNumber = number }
            in
            notes
                |> List.map noteToIntermediate

        durationsToAbsolutes :
            TypedTime
            -> { duration : TypedTime, action : IntermediateAction, force : IntermediateActionForce, measureNumber : Int }
            -> ( TypedTime, IntermediateRepr )
        durationsToAbsolutes lastEndTime { duration, action, force, measureNumber } =
            ( lastEndTime |> TypedTime.add duration
            , { startTimeMs = lastEndTime
              , action = action
              , force = force
              , measureNumber = measureNumber
              }
            )

        roles :
            Dict
                Scheduler.RoleName
                (List { intermediates : List IntermediateRepr, name : String, port_ : FlowIO.Port })
        roles =
            roleMapping
                |> Dict.filterMap
                    (\part ( role, port_ ) ->
                        Dict.get part hapticScore
                            |> Maybe.map
                                (\{ name, measures } ->
                                    { name = name, role = role, port_ = port_, measures = measures }
                                )
                    )
                |> Dict.toList
                |> List.map Tuple.second
                |> Dict.groupBy (\{ role } -> role)
                |> Dict.map
                    (\_ records ->
                        List.map
                            (\{ name, role, port_, measures } ->
                                { intermediates =
                                    absoluteTimeLines
                                        measures
                                , name = name
                                , port_ = port_
                                }
                            )
                            records
                    )

        commonTime : Array TypedTime
        commonTime =
            roles
                |> Dict.values
                |> List.concatMap
                    (\part ->
                        part
                            |> List.map .intermediates
                            |> List.concatMap (List.map .startTimeMs)
                    )
                |> List.map TypedTime.toMillisecondsRounded
                |> Set.fromList
                -- Is there a better way to deduplicate values?
                |> Set.toList
                |> List.sort
                |> List.map (toFloat >> TypedTime.milliseconds)
                |> Array.fromList

        commonTimeline : List TypedTime
        commonTimeline =
            Array.toList commonTime

        instructions : Result String Scheduler.RolesInstructions
        instructions =
            roles
                |> Dict.map convertToInstructions
                |> Dict.toList
                |> List.map Result.combineSecond
                |> Result.combine
                |> Result.map Dict.fromList

        convertToInstructions :
            Scheduler.RoleName
            -> List { intermediates : List IntermediateRepr, name : String, port_ : FlowIO.Port }
            -> Result String (Array FlowIO.FlowIOCommand)
        convertToInstructions _ intermediatesList =
            let
                getByPort p =
                    List.Extra.find (\{ port_ } -> port_ == p) intermediatesList
                        |> Maybe.map .intermediates
                        |> Maybe.withDefault []

                port1 =
                    getByPort FlowIO.Port1

                port2 =
                    getByPort FlowIO.Port2

                port3 =
                    getByPort FlowIO.Port3

                port4 =
                    getByPort FlowIO.Port4

                port5 =
                    getByPort FlowIO.Port5

                fillInstructionLists :
                    List TypedTime
                    -> Maybe IntermediateRepr
                    -> List IntermediateRepr
                    -> List IntermediateRepr
                fillInstructionLists times last intermediates =
                    let
                        default =
                            Maybe.withDefault
                                { startTimeMs = TypedTime.zero
                                , action = NoChange
                                , force = Irrelevant
                                , measureNumber = 0
                                }
                                last
                    in
                    case ( times, intermediates ) of
                        ( t :: restTimes, inter :: restIntermediates ) ->
                            if t |> TypedTime.equalWithin (TypedTime.milliseconds 4) inter.startTimeMs then
                                -- Nothing to do! They match
                                inter :: fillInstructionLists restTimes (Just inter) restIntermediates

                            else
                                { default | startTimeMs = t }
                                    :: fillInstructionLists restTimes last restIntermediates

                        ( t :: restTimes, [] ) ->
                            { default | startTimeMs = t }
                                :: fillInstructionLists restTimes last []

                        ( [], _ ) ->
                            []

                -- We need to add a new value
                mergePortInstructions :
                    IntermediateRepr
                    -> IntermediateRepr
                    -> IntermediateRepr
                    -> IntermediateRepr
                    -> IntermediateRepr
                    -> Result String FlowIO.FlowIOCommand
                mergePortInstructions p1 p2 p3 p4 p5 =
                    let
                        settleAction : IntermediateAction -> List IntermediateAction -> Result String IntermediateAction
                        settleAction a1 restActions =
                            List.foldl
                                (\action acc ->
                                    case ( action, acc ) of
                                        ( _, Err e ) ->
                                            Err e

                                        ( NoChange, Ok a ) ->
                                            Ok a

                                        ( a, Ok NoChange ) ->
                                            Ok a

                                        ( Inflate, Ok Inflate ) ->
                                            Ok Inflate

                                        ( Release, Ok Release ) ->
                                            Ok Release

                                        _ ->
                                            Err ("Problem in measure " ++ String.fromInt p1.measureNumber ++ ": conflicting actions")
                                )
                                (Ok a1)
                                restActions

                        toFlowIOAction : IntermediateAction -> FlowIO.FlowIOAction
                        toFlowIOAction action =
                            case action of
                                NoChange ->
                                    FlowIO.Stop

                                Inflate ->
                                    FlowIO.Inflate

                                Release ->
                                    FlowIO.Release

                        settleHigherForce : IntermediateActionForce -> List IntermediateActionForce -> Int
                        settleHigherForce initForce forces =
                            List.foldl
                                (\f result ->
                                    case ( f, result ) of
                                        ( Full, _ ) ->
                                            Full

                                        ( _, Full ) ->
                                            Full

                                        ( High, _ ) ->
                                            High

                                        ( _, High ) ->
                                            High

                                        ( Medium, _ ) ->
                                            Medium

                                        ( _, Medium ) ->
                                            Medium

                                        ( Low, _ ) ->
                                            Low

                                        ( _, Low ) ->
                                            Low

                                        _ ->
                                            Irrelevant
                                )
                                initForce
                                forces
                                |> forceToPwmVal
                    in
                    settleAction p1.action [ p2.action, p3.action, p4.action, p5.action ]
                        |> Result.map
                            (\settledAction ->
                                { action = toFlowIOAction settledAction
                                , pumpPwm = settleHigherForce p1.force [ p2.force, p3.force, p4.force, p5.force ]
                                , ports =
                                    { port1 = FlowIO.portFromBool (settledAction /= NoChange && p1.action == settledAction)
                                    , port2 = FlowIO.portFromBool (settledAction /= NoChange && p2.action == settledAction)
                                    , port3 = FlowIO.portFromBool (settledAction /= NoChange && p3.action == settledAction)
                                    , port4 = FlowIO.portFromBool (settledAction /= NoChange && p4.action == settledAction)
                                    , port5 = FlowIO.portFromBool (settledAction /= NoChange && p5.action == settledAction)
                                    }
                                }
                            )
            in
            if List.isEmpty intermediatesList || List.length intermediatesList > 5 then
                Err "Can only map up to five parts to a single role"

            else if
                intermediatesList
                    |> List.map (.port_ >> portToIndex)
                    |> Dict.frequencies
                    |> Dict.any (\_ ps -> ps > 1)
            then
                Err "Each part needs to be mapped to a different port"

            else
                List.map5 mergePortInstructions
                    (port1 |> fillInstructionLists commonTimeline Nothing)
                    (port2 |> fillInstructionLists commonTimeline Nothing)
                    (port3 |> fillInstructionLists commonTimeline Nothing)
                    (port4 |> fillInstructionLists commonTimeline Nothing)
                    (port5 |> fillInstructionLists commonTimeline Nothing)
                    |> Result.combine
                    |> Result.map Array.fromList
    in
    instructions
        |> Result.map (\instructions_ -> { time = commonTime, instructions = instructions_ })
