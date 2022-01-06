module Notation exposing
    ( ConversionParameters
    , Dynamic(..)
    , HapticNote(..)
    , HapticPart
    , HapticScore
    , IntermediateAction(..)
    , IntermediateRepr
    , Measure
    , NotationParserConfiguration
    , PartID
    , PartialMeasure
    , Signature
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
import Maybe.Extra as Maybe
import Regex
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
    = Rest (Maybe Dynamic) Timing
    | Hold (Maybe Dynamic) Timing
    | Actuate (Maybe Dynamic) Timing


type alias Timing =
    Int


type Dynamic
    = Pianississimo
    | Pianissimo
    | Piano
    | Mezzopiano
    | Mezzoforte
    | Forte
    | Fortissimo
    | Fortississimo


dynamicToInt : Dynamic -> Int
dynamicToInt dynamic =
    case dynamic of
        Pianississimo ->
            0

        Pianissimo ->
            1

        Piano ->
            2

        Mezzopiano ->
            3

        Mezzoforte ->
            4

        Forte ->
            5

        Fortissimo ->
            6

        Fortississimo ->
            7


intToDynamic : Int -> Result String Dynamic
intToDynamic int =
    case int of
        0 ->
            Ok Pianississimo

        1 ->
            Ok Pianissimo

        2 ->
            Ok Piano

        3 ->
            Ok Mezzopiano

        4 ->
            Ok Mezzoforte

        5 ->
            Ok Forte

        6 ->
            Ok Fortissimo

        7 ->
            Ok Fortississimo

        _ ->
            Err (String.fromInt int ++ " does not represent a valid dynamic value")


type alias NotationParserConfiguration =
    {}


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
    parseMusicXmlWith {}


parseMusicXmlWith : NotationParserConfiguration -> String -> Result String HapticScore
parseMusicXmlWith _ input =
    let
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
            let
                dtdTagRegex =
                    Regex.fromString "<!DOCTYPE .*?>"
            in
            case dtdTagRegex of
                Nothing ->
                    Err "DTD regex failed compilation"

                Just re ->
                    input
                        |> Regex.replace re (\_ -> "")
                        |> decode

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
                        |> List.map noteDecoder
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

        noteDecoder : Value -> Result String HapticNote
        noteDecoder value =
            let
                duration : Result String Timing
                duration =
                    value
                        |> tag "duration" int

                hasRest =
                    value
                        |> tag "rest" (\_ -> Ok True)
                        |> Result.isOk

                hasNoteheadX =
                    value
                        |> tag "notehead" string
                        |> Result.unwrap False (\s -> String.trim s == "x")
            in
            case duration of
                Ok duration_ ->
                    if hasRest then
                        Ok (Rest Nothing duration_)

                    else if hasNoteheadX then
                        Ok (Hold Nothing duration_)

                    else
                        Ok (Actuate Nothing duration_)

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
    , roleMapping : Dict String ( Scheduler.RoleName, FlowIO.Port )
    , dynamics :
        { pianissimo : Int
        , piano : Int
        , mezzopiano : Int
        , mezzoforte : Int
        , forte : Int
        , fortissimo : Int
        , fortississimo : Int
        }
    }


type IntermediateAction
    = Inflate
    | Release
    | NoChange


type alias IntermediateRepr =
    { startTimeMs : TypedTime
    , action : IntermediateAction
    , dynamic : Maybe Dynamic
    , measureNumber : Int
    }


scoreToSchedule : ConversionParameters -> HapticScore -> Result String Scheduler.Instructions
scoreToSchedule { bpm, roleMapping, dynamics } hapticScore =
    let
        dynamicToPwm : Dynamic -> Int
        dynamicToPwm dynamic =
            case dynamic of
                Pianississimo ->
                    0

                Pianissimo ->
                    dynamics.pianissimo

                Piano ->
                    dynamics.piano

                Mezzopiano ->
                    dynamics.mezzopiano

                Mezzoforte ->
                    dynamics.mezzoforte

                Forte ->
                    dynamics.forte

                Fortissimo ->
                    dynamics.fortissimo

                Fortississimo ->
                    dynamics.fortississimo

        absoluteTimeLines : List Measure -> List IntermediateRepr
        absoluteTimeLines measures =
            measures
                |> List.concatMap measureToIntermediate
                |> List.Extra.mapAccuml durationsToAbsolutes ( TypedTime.zero, Nothing )
                |> (\( ( lastTime, lastDynamic ), intermediates ) ->
                        let
                            lastMeasure =
                                List.Extra.last intermediates |> Maybe.map .measureNumber
                        in
                        List.append intermediates
                            [ { startTimeMs = lastTime
                              , action = NoChange
                              , dynamic = Just Pianississimo
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
                    , dynamic : Maybe Dynamic
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
                    -> { duration : TypedTime, action : IntermediateAction, dynamic : Maybe Dynamic, measureNumber : Int }
                noteToIntermediate hapticNote =
                    case hapticNote of
                        Rest dynamic t ->
                            { duration = toDuration t, action = Release, dynamic = dynamic, measureNumber = number }

                        Hold dynamic t ->
                            { duration = toDuration t, action = NoChange, dynamic = dynamic, measureNumber = number }

                        Actuate dynamic t ->
                            { duration = toDuration t, action = Inflate, dynamic = dynamic, measureNumber = number }
            in
            notes
                |> List.map noteToIntermediate

        durationsToAbsolutes :
            ( TypedTime, Maybe Dynamic )
            -> { duration : TypedTime, action : IntermediateAction, dynamic : Maybe Dynamic, measureNumber : Int }
            -> ( ( TypedTime, Maybe Dynamic ), IntermediateRepr )
        durationsToAbsolutes ( lastEndTime, lastDynamic ) { duration, action, dynamic, measureNumber } =
            let
                nextDynamic =
                    Maybe.or dynamic lastDynamic
            in
            ( ( lastEndTime |> TypedTime.add duration, nextDynamic )
            , { startTimeMs = lastEndTime
              , action = action
              , dynamic = nextDynamic
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
                                , dynamic = Just Pianississimo
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

                        settleDynamic : Maybe Dynamic -> List (Maybe Dynamic) -> Result String Int
                        settleDynamic initMaybeDynamic maybeDynamics =
                            let
                                initDynamic =
                                    Result.fromMaybe "Expected all dynamic values to be resolved" initMaybeDynamic

                                dyns =
                                    List.map (Result.fromMaybe "Expected all dynamic values to be resolved") maybeDynamics
                            in
                            List.foldl
                                (\f result ->
                                    Result.map2
                                        (\f_ r_ ->
                                            max (dynamicToInt f_) (dynamicToInt r_)
                                                |> intToDynamic
                                        )
                                        f
                                        result
                                        |> Result.join
                                )
                                initDynamic
                                dyns
                                |> Result.map dynamicToPwm
                    in
                    Result.map2
                        (\settledDynamic settledAction ->
                            { action = toFlowIOAction settledAction
                            , pumpPwm = settledDynamic
                            , ports =
                                { port1 = FlowIO.portFromBool (settledAction /= NoChange && p1.action == settledAction)
                                , port2 = FlowIO.portFromBool (settledAction /= NoChange && p2.action == settledAction)
                                , port3 = FlowIO.portFromBool (settledAction /= NoChange && p3.action == settledAction)
                                , port4 = FlowIO.portFromBool (settledAction /= NoChange && p4.action == settledAction)
                                , port5 = FlowIO.portFromBool (settledAction /= NoChange && p5.action == settledAction)
                                }
                            }
                        )
                        (settleDynamic p1.dynamic [ p2.dynamic, p3.dynamic, p4.dynamic, p5.dynamic ])
                        (settleAction p1.action [ p2.action, p3.action, p4.action, p5.action ])
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
