module Extra.Resource exposing (Resource(..))


type Resource e d
    = NotLoaded
    | Loaded d
    | Error e