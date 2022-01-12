module Extra.Resource exposing (Resource(..), fromResult)


type Resource e d
    = NotLoaded
    | Processing
    | Loaded d
    | Error e


fromResult : Result e d -> Resource e d
fromResult result =
    case result of
        Ok value ->
            Loaded value

        Err error ->
            Error error