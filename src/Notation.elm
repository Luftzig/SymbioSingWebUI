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
    )

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


type alias IntermediateRepr =
    { startTimeMs : TypedTime
    , action : IntermediateAction
    , force : IntermediateActionForce
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
                        List.append intermediates [ { startTimeMs = lastTime, action = NoChange, force = Irrelevant } ]
                   )

        measureToIntermediate : Measure -> List { duration : TypedTime, action : IntermediateAction, force : IntermediateActionForce }
        measureToIntermediate { divisionsPerQuarter, notes } =
            let
                durationPerDivision =
                    let
                        bpmTimesDivisions =
                            toFloat (bpm * divisionsPerQuarter)
                    in
                    (TypedTime.seconds 60
                        |> TypedTime.divide (bpmTimesDivisions))

                toDuration : Timing -> TypedTime
                toDuration d =
                    durationPerDivision |> TypedTime.multiply (toFloat d)

                noteToIntermediate : HapticNote -> { duration : TypedTime, action : IntermediateAction, force : IntermediateActionForce }
                noteToIntermediate hapticNote =
                    case hapticNote of
                        Rest t ->
                            { duration = toDuration t, action = NoChange, force = Irrelevant }

                        FullInflate t ->
                            { duration = toDuration t, action = Inflate, force = Full }

                        FastInflate t ->
                            { duration = toDuration t, action = Inflate, force = High }

                        SlowInflate t ->
                            { duration = toDuration t, action = Inflate, force = Medium }

                        SlowestInflate t ->
                            { duration = toDuration t, action = Inflate, force = Low }

                        FullRelease t ->
                            { duration = toDuration t, action = Release, force = Full }

                        FastRelease t ->
                            { duration = toDuration t, action = Release, force = High }

                        SlowRelease t ->
                            { duration = toDuration t, action = Release, force = Medium }

                        SlowestRelease t ->
                            { duration = toDuration t, action = Release, force = Low }
            in
            notes
                |> List.map noteToIntermediate

        durationsToAbsolutes :
            TypedTime
            -> { duration : TypedTime, action : IntermediateAction, force : IntermediateActionForce }
            -> ( TypedTime, IntermediateRepr )
        durationsToAbsolutes lastEndTime { duration, action, force } =
            ( lastEndTime |> TypedTime.add duration
            , { startTimeMs = lastEndTime
              , action = action
              , force = force
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
                Ok Array.empty
    in
    instructions
        |> Result.map (\instructions_ -> { time = commonTime, instructions = instructions_ })
