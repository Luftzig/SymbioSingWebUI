module Extra.Resource exposing (Resource(..), fromResult, isLoaded)


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


isLoaded : Resource e d -> Bool
isLoaded resource =
    case resource of
        Loaded _ ->
            True

        _ ->
            False
