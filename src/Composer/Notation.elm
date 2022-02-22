module Composer.Notation exposing
    ( ConversionParameters
    , Dynamics
    , IntermediateAction(..)
    , IntermediateRepr
    , NotationParserConfiguration
    , parseMusicXml
    , parseMusicXmlWith
    , scoreToSchedule
    , toTuple2
    )

import Array exposing (Array)
import Dict exposing (Dict)
import Dict.Extra as Dict
import Extra.Result as Result
import Extra.TypedTime as TypedTime exposing (TypedTime)
import Extra.Xml exposing (oneOfTags, tagPath)
import FlowIO exposing (Action, Command, PortState, portToIndex)
import List.Extra
import Maybe.Extra as Maybe
import Messages
    exposing
        ( Dynamic(..)
        , HapticNote(..)
        , HapticPart
        , HapticScore
        , Instructions
        , Measure
        , PartID
        , PartialMeasure
        , RoleName
        , RolesInstructions
        , Signature
        , Timing
        )
import Regex
import Result.Extra as Result
import Set
import Xml exposing (Value(..))
import Xml.Decode exposing (decode)
import Xml.Query exposing (contains, int, string, tag, tags)


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


toTuple2 : (a -> b) -> (a -> c) -> a -> ( b, c )
toTuple2 f g v =
    ( f v, g v )


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
                partTagToMeasures : ( String, Value ) -> Result String ( String, List Measure )
                partTagToMeasures ( partId, value ) =
                    measuresDecoder partId value
                        |> Result.map (\measures -> ( partId, measures ))
            in
            document
                |> Result.map (tags "part")
                |> Result.mapList (\part -> Result.combineFirst ( decodeAttribute "id" string part, part ))
                |> Result.mapList partTagToMeasures
                |> Result.map Dict.fromList

        measuresDecoder : PartID -> Value -> Result String (List Measure)
        measuresDecoder partId value =
            let
                measureValues : List Value
                measureValues =
                    tags "measure" value

                partialValues =
                    measureValues
                        |> List.Extra.mapAccuml (measureDecoder partId) Nothing
                        |> Tuple.second
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
                            Result.Err ("In part " ++ partId ++ ": First measure must specify time signature and divisions")
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

        measureDecoder : PartID -> Maybe Dynamic -> Value -> ( Maybe Dynamic, Result String PartialMeasure )
        measureDecoder partId previousMeasureDynamic value =
            let
                measureNum =
                    decodeAttribute "number" int value

                measureNumString =
                    measureNum
                        |> Result.toMaybe
                        |> Maybe.map (\n -> "measure " ++ String.fromInt n)
                        |> Maybe.withDefault "measure unknown"

                loc =
                    "part " ++ partId ++ ", " ++ measureNumString

                signature : Result String Signature
                signature =
                    tagPath [ "attributes", "time" ] signatureDecoder value

                divisions : Result String Int
                divisions =
                    tagPath [ "attributes", "divisions" ] int value

                combineMeasureValues :
                    Value
                    -> ( Maybe Dynamic, List (Result String HapticNote) )
                    -> ( Maybe Dynamic, List (Result String HapticNote) )
                combineMeasureValues currentTag ( lastDynamic_, ns ) =
                    case currentTag of
                        Tag "note" _ innerValue ->
                            ( lastDynamic_, noteDecoder loc lastDynamic_ innerValue :: ns )

                        Tag "direction" _ innerValue ->
                            ( Maybe.or (directionTagDecoder innerValue) lastDynamic_, ns )

                        _ ->
                            ( lastDynamic_, ns )

                directionTagDecoder : Value -> Maybe Dynamic
                directionTagDecoder dirValue =
                    dirValue
                        |> tag "dynamics" dynamicsDecoder
                        |> Result.toMaybe

                dynamicsDecoder : Value -> Result String Dynamic
                dynamicsDecoder dynValue =
                    dynValue
                        |> oneOfTags [ "ppp", "pp", "p", "mp", "mf", "f", "ff", "fff" ]
                        |> Maybe.andThen
                            (\dynValue_ ->
                                case dynValue_ of
                                    Tag "ppp" _ _ ->
                                        Just Pianississimo

                                    Tag "pp" _ _ ->
                                        Just Pianissimo

                                    Tag "p" _ _ ->
                                        Just Piano

                                    Tag "mp" _ _ ->
                                        Just Mezzopiano

                                    Tag "mf" _ _ ->
                                        Just Mezzoforte

                                    Tag "f" _ _ ->
                                        Just Forte

                                    Tag "ff" _ _ ->
                                        Just Fortissimo

                                    Tag "fff" _ _ ->
                                        Just Fortississimo

                                    _ ->
                                        Nothing
                            )
                        |> Result.fromMaybe ("In " ++ loc ++ ": dynamics value was not found")

                notes : ( Maybe Dynamic, Result String (List HapticNote) )
                notes =
                    value
                        |> Xml.foldl
                            combineMeasureValues
                            ( previousMeasureDynamic, [] )
                        |> Tuple.mapSecond List.reverse
                        |> Tuple.mapSecond Result.combine

                ( lastDynamic, notesRes ) =
                    notes
            in
            ( lastDynamic
            , Result.map2
                (\num notes_ ->
                    { number = num
                    , signature = Result.toMaybe signature
                    , divisionsPerQuarter = Result.toMaybe divisions
                    , notes = notes_
                    }
                )
                measureNum
                notesRes
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

        noteDecoder : String -> Maybe Dynamic -> Value -> Result String HapticNote
        noteDecoder loc lastDynamic value =
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

                hasTrill =
                    value
                        |> contains (Tag "trill-mark" Dict.empty (Object []))
            in
            case duration of
                Ok duration_ ->
                    if hasRest then
                        case lastDynamic of
                            Just dyn ->
                                Ok (Rest dyn duration_)

                            Nothing ->
                                Err ("In " ++ loc ++ ", handling rest: Encountered a rest before any dynamic signs")

                    else if hasNoteheadX then
                        Ok (Hold Pianississimo duration_)

                    else if hasTrill then
                        Ok (HardTrill (lastDynamic |> Maybe.withDefault Mezzopiano) duration_)

                    else
                        case lastDynamic of
                            Just dyn ->
                                Ok (Actuate dyn duration_)

                            Nothing ->
                                Err ("In " ++ loc ++ " handling note: Encountered a note before any dynamic signs")

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


type alias Dynamics =
    { pianissimo : Int
    , piano : Int
    , mezzopiano : Int
    , mezzoforte : Int
    , forte : Int
    , fortissimo : Int
    , fortississimo : Int
    }


type alias ConversionParameters =
    { bpm : Int
    , roleMapping : Dict String ( RoleName, FlowIO.Port )
    , dynamics : Dynamics
    , trillInterval : TypedTime
    }


type IntermediateAction
    = Inflate
    | Release
    | NoChange
    | TrillRelease
    | TrillInflate


type alias IntermediateRepr =
    { startTimeMs : TypedTime
    , action : IntermediateAction
    , dynamic : Dynamic
    , measureNumber : Int
    , numberInMeasure : Int
    }


scoreToSchedule : ConversionParameters -> HapticScore -> Result ( List (List IntermediateRepr), String ) Instructions
scoreToSchedule { bpm, roleMapping, dynamics, trillInterval } hapticScore =
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

        absoluteTimeLines : String -> List Measure -> List IntermediateRepr
        absoluteTimeLines partName measures =
            measures
                |> List.concatMap (measureToIntermediate partName)
                |> List.Extra.mapAccuml durationsToAbsolutes TypedTime.zero
                |> (\( lastTime, intermediates ) ->
                        let
                            lastMeasure =
                                List.Extra.last intermediates |> Maybe.map .measureNumber
                        in
                        List.append intermediates
                            [ { startTimeMs = lastTime
                              , action = NoChange
                              , dynamic = Pianississimo
                              , measureNumber = lastMeasure |> Maybe.map ((+) 1) |> Maybe.withDefault 0
                              , numberInMeasure = 0
                              }
                            ]
                   )

        measureToIntermediate :
            String
            -> Measure
            ->
                List
                    { duration : TypedTime
                    , action : IntermediateAction
                    , dynamic : Dynamic
                    , measureNumber : Int
                    , numberInMeasure : Int
                    }
        measureToIntermediate _ { divisionsPerQuarter, notes, number } =
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
                    Int
                    -> HapticNote
                    ->
                        List
                            { duration : TypedTime
                            , action : IntermediateAction
                            , dynamic : Dynamic
                            , measureNumber : Int
                            , numberInMeasure : Int
                            }
                noteToIntermediate noteIndex hapticNote =
                    case hapticNote of
                        Rest dynamic t ->
                            [ { duration = toDuration t
                              , action = Release
                              , dynamic = dynamic
                              , measureNumber = number
                              , numberInMeasure = noteIndex + 1
                              }
                            ]

                        Hold dynamic t ->
                            [ { duration = toDuration t
                              , action = NoChange
                              , dynamic = dynamic
                              , measureNumber = number
                              , numberInMeasure = noteIndex + 1
                              }
                            ]

                        Actuate dynamic t ->
                            [ { duration = toDuration t
                              , action = Inflate
                              , dynamic = dynamic
                              , measureNumber = number
                              , numberInMeasure = noteIndex + 1
                              }
                            ]

                        Trill dynamic t ->
                            let
                                trills =
                                    toDuration t
                                        |> TypedTime.divideByInterval trillInterval
                            in
                            List.range 0 (floor trills - 1)
                                |> List.map
                                    (\i ->
                                        if (i |> modBy 2) == 0 then
                                            Inflate

                                        else
                                            NoChange
                                    )
                                |> List.map
                                    (\action ->
                                        { duration = trillInterval
                                        , action = action
                                        , dynamic = dynamic
                                        , measureNumber = number
                                        , numberInMeasure = noteIndex + 1
                                        }
                                    )

                        HardTrill dynamic t ->
                            let
                                trills =
                                    toDuration t
                                        |> TypedTime.divideByInterval trillInterval
                            in
                            List.range 0 (floor trills - 1)
                                |> List.map
                                    (\i ->
                                        if (i |> modBy 2) == 0 then
                                            TrillInflate

                                        else
                                            TrillRelease
                                    )
                                |> List.map
                                    (\action ->
                                        { duration = trillInterval
                                        , action = action
                                        , dynamic = dynamic
                                        , measureNumber = number
                                        , numberInMeasure = noteIndex + 1
                                        }
                                    )
            in
            notes
                |> List.indexedMap noteToIntermediate
                |> List.concat

        durationsToAbsolutes :
            TypedTime
            ->
                { duration : TypedTime
                , action : IntermediateAction
                , dynamic : Dynamic
                , measureNumber : Int
                , numberInMeasure : Int
                }
            -> ( TypedTime, IntermediateRepr )
        durationsToAbsolutes lastEndTime { duration, action, dynamic, measureNumber, numberInMeasure } =
            ( lastEndTime |> TypedTime.add duration
            , { startTimeMs = lastEndTime
              , action = action
              , dynamic = dynamic
              , measureNumber = measureNumber
              , numberInMeasure = numberInMeasure
              }
            )

        roles :
            Dict
                RoleName
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
                                        name
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

        instructions : Result ( List (List IntermediateRepr), String ) RolesInstructions
        instructions =
            roles
                |> Dict.map convertToInstructions
                |> Dict.toList
                |> List.map Result.combineSecond
                |> Result.combine
                |> Result.map Dict.fromList

        convertToInstructions :
            RoleName
            -> List { intermediates : List IntermediateRepr, name : String, port_ : FlowIO.Port }
            -> Result ( List (List IntermediateRepr), String ) (Array FlowIO.Command)
        convertToInstructions roleName intermediatesList =
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
                                , dynamic = Pianississimo
                                , measureNumber = 0
                                , numberInMeasure = 0
                                }
                                last
                    in
                    case ( times, intermediates ) of
                        ( t :: restTimes, inter :: restIntermediates ) ->
                            if t |> TypedTime.equalWithin (TypedTime.milliseconds 0.1) inter.startTimeMs then
                                -- Nothing to do! They match
                                inter :: fillInstructionLists restTimes (Just inter) restIntermediates

                            else
                                { default | startTimeMs = t }
                                    :: fillInstructionLists restTimes last (inter :: restIntermediates)

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
                    -> Result ( Maybe IntermediateRepr, String ) FlowIO.Command
                mergePortInstructions p1 p2 p3 p4 p5 =
                    let
                        settleAction : List IntermediateRepr -> Result ( Maybe IntermediateRepr, String ) IntermediateAction
                        settleAction actions =
                            let
                                releaseActions =
                                    List.filter (.action >> (==) Release) actions
                                        |> List.indexedMap Tuple.pair

                                inflateActions =
                                    List.filter (.action >> (==) Inflate) actions
                                        |> List.indexedMap Tuple.pair

                                trillReleaseActions =
                                    List.filter (.action >> (==) TrillRelease) actions
                                        |> List.indexedMap Tuple.pair

                                trillInflateActions =
                                    List.filter (.action >> (==) TrillInflate) actions
                                        |> List.indexedMap Tuple.pair

                                has =
                                    not << List.isEmpty

                                errorCtx =
                                    List.head actions
                                        |> (\repr ->
                                                "In role "
                                                    ++ roleName
                                                    ++ ", measure "
                                                    ++ (repr
                                                            |> Maybe.map (.measureNumber >> String.fromInt)
                                                            |> Maybe.withDefault "unknown"
                                                       )
                                           )

                                noteErrorString ( i, repr ) =
                                    "(P" ++ String.fromInt (i + 1) ++ ", note " ++ String.fromInt repr.numberInMeasure ++ ")"

                                conflictingError list =
                                    list
                                        |> List.map noteErrorString
                                        |> String.join ", "
                            in
                            {- Truth table:
                               Has Release | Has Inflate | Has Trill-Release | Has Trill-Inflate | Result
                               No          | No          | No                | No                | NoChange
                               No          | No          | Yes               | No                | TrillRelease
                               Yes         | No          | No                | No                | Release
                               Yes         | No          | Yes               | No                | TrillRelease
                               No          | Yes         | No                | No                | Inflate
                               No          | Yes         | Yes               | No                | TrillRelease
                               Yes         | Yes         | No                | No                | Error
                               Yes         | Yes         | Yes               | No                | Error
                               No          | No          | No                | Yes               | TrillInflate
                               No          | No          | Yes               | Yes               | Error
                               Yes         | No          | No                | Yes               | TrillInflate
                               Yes         | No          | Yes               | Yes               | Error
                               No          | Yes         | No                | Yes               | TrillInflate
                               No          | Yes         | Yes               | Yes               | Error
                               Yes         | Yes         | No                | Yes               | Error
                               Yes         | Yes         | Yes               | Yes               | Error
                            -}
                            case ( has trillReleaseActions, has trillInflateActions ) of
                                ( False, False ) ->
                                    case ( has releaseActions, has inflateActions ) of
                                        ( False, False ) ->
                                            Ok NoChange

                                        ( False, True ) ->
                                            Ok Inflate

                                        ( True, False ) ->
                                            Ok Release

                                        ( True, True ) ->
                                            Err
                                                ( Just p1
                                                , errorCtx
                                                    ++ " I found conflicting actions: Release in notes ["
                                                    ++ conflictingError releaseActions
                                                    ++ "] and Inflate in ["
                                                    ++ conflictingError inflateActions
                                                    ++ "]"
                                                )

                                ( False, True ) ->
                                    Ok TrillInflate

                                ( True, False ) ->
                                    Ok TrillRelease

                                ( True, True ) ->
                                    Err
                                        ( Just p1
                                        , errorCtx
                                            ++ " I found conflicting actions: Trill-Release in notes ["
                                            ++ conflictingError trillReleaseActions
                                            ++ "] and Trill-Inflate in ["
                                            ++ conflictingError trillInflateActions
                                            ++ "]"
                                        )

                        toFlowIOAction : IntermediateAction -> FlowIO.Action
                        toFlowIOAction action =
                            case action of
                                NoChange ->
                                    FlowIO.Stop

                                Inflate ->
                                    FlowIO.Actuate

                                Release ->
                                    FlowIO.Release

                                TrillRelease ->
                                    FlowIO.Release

                                TrillInflate ->
                                    FlowIO.Actuate

                        settleDynamic : List ( IntermediateAction, Dynamic ) -> Int
                        settleDynamic dyns =
                            let
                                actuationOnly =
                                    dyns |> List.filter (\( action, _ ) -> action == Inflate || action == TrillInflate)

                                maxDynamic =
                                    actuationOnly
                                        |> List.map Tuple.second
                                        |> List.map dynamicToPwm
                                        |> List.maximum
                            in
                            maxDynamic
                                |> Maybe.withDefault 0

                        decidePortState : IntermediateAction -> IntermediateAction -> PortState
                        decidePortState requested chosen =
                            case ( requested, chosen ) of
                                ( _, NoChange ) ->
                                    FlowIO.PortClosed

                                ( NoChange, _ ) ->
                                    FlowIO.PortClosed

                                ( Inflate, Inflate ) ->
                                    FlowIO.PortOpen

                                ( Release, Release ) ->
                                    FlowIO.PortOpen

                                ( TrillRelease, TrillRelease ) ->
                                    FlowIO.PortOpen

                                (TrillInflate, TrillInflate) ->
                                    FlowIO.PortOpen

                                (Inflate, TrillRelease) ->
                                    FlowIO.PortClosed

                                (Release, TrillRelease) ->
                                    FlowIO.PortOpen

                                (Inflate, TrillInflate) ->
                                    FlowIO.PortOpen

                                (Release, TrillInflate) ->
                                    FlowIO.PortClosed

                                -- Remaining options are impossible from `settleAction`:
                                --    ( Inflate, Release )             - Contradicting
                                --    ( Release, Inflate )             - Contradicting
                                --    ( TrillRelease, Inflate )        - TrillRelease > Inflate
                                --    ( TrillRelease, Release )        - TrillRelease > Release
                                --    ( TrillRelease, TrillInflate )   - Contradicting
                                --    ( TrillInflate, Inflate )        - TrillInflate > Inflate
                                --    ( TrillInflate, Release )        - TrillInflate > Release
                                --    ( TrillInflate, TrillRelease )   - Contradicting

                                _ ->
                                    FlowIO.PortClosed

                    in
                    Result.map
                        (\settledAction ->
                            let
                                settledDynamic =
                                    settleDynamic
                                        [ ( p1.action, p1.dynamic )
                                        , ( p2.action, p2.dynamic )
                                        , ( p3.action, p3.dynamic )
                                        , ( p4.action, p4.dynamic )
                                        , ( p5.action, p5.dynamic )
                                        ]
                            in
                            { action = toFlowIOAction settledAction
                            , pumpPwm = settledDynamic
                            , ports =
                                { port1 = decidePortState p1.action settledAction
                                , port2 = decidePortState p2.action settledAction
                                , port3 = decidePortState p3.action settledAction
                                , port4 = decidePortState p4.action settledAction
                                , port5 = decidePortState p5.action settledAction
                                }
                            }
                        )
                        (settleAction [ p1, p2, p3, p4, p5 ])
            in
            if List.isEmpty intermediatesList || List.length intermediatesList > 5 then
                Err ( [], "Can only map up to five parts to a single role" )

            else if
                intermediatesList
                    |> List.map (.port_ >> portToIndex)
                    |> Dict.frequencies
                    |> Dict.any (\_ ps -> ps > 1)
            then
                Err ( [], "Each part needs to be mapped to a different port" )

            else
                List.map5 mergePortInstructions
                    (port1 |> fillInstructionLists commonTimeline Nothing)
                    (port2 |> fillInstructionLists commonTimeline Nothing)
                    (port3 |> fillInstructionLists commonTimeline Nothing)
                    (port4 |> fillInstructionLists commonTimeline Nothing)
                    (port5 |> fillInstructionLists commonTimeline Nothing)
                    |> Result.combine
                    |> Result.mapError
                        (\( inter, errorMessage ) ->
                            ( case inter of
                                Just interError ->
                                    [ port1, port2, port3, port4, port5 ]
                                        |> List.map (\innerList -> List.filter (\item -> item.measureNumber == interError.measureNumber) innerList)

                                Nothing ->
                                    []
                            , errorMessage
                            )
                        )
                    |> Result.map Array.fromList
    in
    instructions
        |> Result.map (\instructions_ -> { time = commonTime, instructions = instructions_ })
