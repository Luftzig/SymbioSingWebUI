port module LocalStorage exposing (load, remove, save, listen, getAllKeys, StorageEvent(..))

import Json.Encode exposing (Value, encode)


save : String -> Value -> Cmd msg
save keyName value =
    saveInternal ( keyName, encode 0 value )


port remove : String -> Cmd msg


port saveInternal : ( String, String ) -> Cmd msg


port getAllKeys : () -> Cmd msg

port load : String -> Cmd msg


port loaded : (( String, String ) -> msg) -> Sub msg


port receivedAllKeys : (List String -> msg) -> Sub msg


type StorageEvent
    = Loaded String String
    | ReceivedAllKeys (List String)


listen : (StorageEvent -> msg) -> Sub msg
listen handler =
    Sub.batch
        [ loaded (\(key, value) -> handler <| Loaded key value)
        , receivedAllKeys (ReceivedAllKeys >> handler)
        ]
