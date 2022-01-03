module Notation exposing (..)

import Dict exposing (Dict)
import Result.Extra
import Xml exposing (Value(..))
import Xml.Decode exposing (decode)
import Xml.Query exposing (tag, tags)


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
    , divisions : Int
    , notes : List HapticNote
    }


type alias PartialMeasure =
    { number : Int
    , signature : Maybe Signature
    , divisions : Maybe Int
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


type Timing
    = Duration Int


toTuple2 : (a -> b) -> (a -> c) -> a -> ( b, c )
toTuple2 f g v =
    ( f v, g v )


resultMapList : (a -> b) -> Result e (List a) -> Result e (List b)
resultMapList f =
    Result.map (List.map f)


string : Value -> Result String String
string value =
    case value of
        StrNode s ->
            Result.Ok s

        _ ->
            Result.Err "Expected a StrNode"


int : Value -> Result String Int
int value =
    case value of
        IntNode a ->
            Result.Ok a

        StrNode s ->
            String.toInt s
                |> Result.fromMaybe "Expect value to be an integer string"

        _ ->
            Result.Err "Expected an integer"


tagPath : List String -> (Value -> Result String a) -> Value -> Result String a
tagPath path decoder value =
    case path of
        [] ->
            decoder value

        p :: rest ->
            tag p (tagPath rest decoder) value


parseMusicXml : String -> Result String HapticScore
parseMusicXml input =
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
            decode input

        decodePartName : Value -> Result String String
        decodePartName =
            tag "part-name" string

        scoreParts : Result String (Dict PartID HapticPart)
        scoreParts =
            document
                |> Result.map (tags "score-part")
                |> Result.map (List.map (toTuple2 (decodeAttribute "id" string) decodePartName))
                |> Result.map (List.map Result.Extra.combineBoth)
                |> Result.andThen Result.Extra.combine
                |> Result.map (List.map (Tuple.mapSecond (\name -> { name = name, measures = [] })))
                |> Result.map Dict.fromList

        parts : Result String (Dict PartID (List Measure))
        parts =
            let
                toMeasures : ( String, Value ) -> Result String ( String, List Measure )
                toMeasures =
                    Result.Extra.combineMapSecond measuresDecoder
            in
            document
                |> Result.map (tags "part")
                |> resultMapList (\part -> Result.Extra.combineFirst ( decodeAttribute "id" string part, part ))
                |> Result.andThen Result.Extra.combine
                |> resultMapList toMeasures
                |> Result.map Result.Extra.combine
                |> Result.Extra.join
                |> Result.map Dict.fromList

        measuresDecoder : Value -> Result String (List Measure)
        measuresDecoder value =
            let
                measureValues =
                    tags "measure" value

                partialValues =
                    measureValues
                        |> List.map measureDecoder
                        |> Result.Extra.combine

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
                                    , divisions = Maybe.withDefault initDivisions m.divisions
                                    , notes = m.notes
                                    }
                                        :: completeMeasures_ (Maybe.withDefault initDivisions m.divisions)
                                            (Maybe.withDefault initSignature m.signature)
                                            rest_
                    in
                    case ( init.divisions, init.signature ) of
                        ( Just div, Just sig ) ->
                            Result.Ok
                                ({ number = init.number, signature = sig, divisions = div, notes = init.notes }
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
                        |> tags "notes"
                        |> List.map notesDecoder
                        |> Result.Extra.combine
            in
            ( measureNum value, notes )
                |> Result.Extra.combineBoth
                |> Result.map
                    (\( num, notes_ ) ->
                        { number = num
                        , signature = Result.toMaybe signature
                        , divisions = Result.toMaybe divisions
                        , notes = notes_
                        }
                    )

        signatureDecoder : Value -> Result String Signature
        signatureDecoder value =
            -- TODO
            let
                beats =
                    tag "beats" int value

                beatsType =
                    value
                        |> tag "beat-type" int
                        |> Result.map Duration
            in
            ( beats, beatsType )
                |> Result.Extra.combineBoth
                |> Result.map (\( beats_, beatsType_ ) -> { beats = beats_, beatType = beatsType_ })

        notesDecoder : Value -> Result String HapticNote
        notesDecoder value =
            -- TODO: Complete implementation
            Ok (Rest (Duration 4))

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
    Result.Extra.combineBoth ( scoreParts, parts )
        |> Result.andThen
            (\( partNames, partValues ) ->
                merge
                    partNames
                    partValues
            )
