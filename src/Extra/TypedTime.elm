module Extra.TypedTime exposing (..)

import TypedTime exposing (TypedTime)


toMilliseconds : TypedTime -> Float
toMilliseconds t =
    t
        |> TypedTime.toSeconds
        |> (/) 1000.0


toMillisecondsRounded : TypedTime -> Int
toMillisecondsRounded t =
    t
        |> toMilliseconds
        |> round
