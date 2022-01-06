module Extra.Xml exposing (..)

import Maybe.Extra
import Result.Extra
import Xml exposing (Value(..))
import Xml.Query exposing (collect, tag)


tagPath : List String -> (Value -> Result String a) -> Value -> Result String a
tagPath path decoder value =
    case path of
        [] ->
            decoder value

        p :: rest ->
            tag p (tagPath rest decoder) value


{-| Returns old the tags in an XML value
-}
oneOfTags : List String -> Value -> Maybe Value
oneOfTags tags value =
    case tags of
        tag :: rest ->
            value
                |> Debug.log ("search for tag " ++ tag)
                |> Xml.Query.tags tag
                |> List.head
                |> Maybe.Extra.orElseLazy (\() -> oneOfTags rest value)
                |> Debug.log "found:"

        [] ->
            Nothing
