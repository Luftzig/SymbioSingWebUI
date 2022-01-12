module Extra.TypedTime exposing (TypedTime, add, divide, equal, lt, milliseconds, minutes, multiply, seconds, toMilliseconds, toMillisecondsRounded, toMillisecondsString, zero, equalWithin)

import Float.Extra


type TypedTime
    = Milliseconds Float


milliseconds : Float -> TypedTime
milliseconds =
    Milliseconds


seconds : Float -> TypedTime
seconds s =
    Milliseconds (s * 1000)


minutes : Float -> TypedTime
minutes m =
    Milliseconds (m * 60 * 1000)


zero : TypedTime
zero =
    Milliseconds 0


equal : TypedTime -> TypedTime -> Bool
equal op2 op1 =
    toMilliseconds op1 == toMilliseconds op2


equalWithin : TypedTime -> TypedTime -> TypedTime -> Bool
equalWithin tolerance op2 op1 =
    Float.Extra.equalWithin (toMilliseconds tolerance) (toMilliseconds op1) (toMilliseconds op2)


lt : TypedTime -> TypedTime -> Bool
lt t2 t1 =
    toMilliseconds t1 < toMilliseconds t2


divide : Float -> TypedTime -> TypedTime
divide op2 t =
    toMilliseconds t
        |> (\ms ->
                ms
                    / op2
           )
        |> milliseconds


multiply : Float -> TypedTime -> TypedTime
multiply op2 t =
    toMilliseconds t
        |> (\ms ->
                ms
                    * op2
           )
        |> milliseconds


add : TypedTime -> TypedTime -> TypedTime
add t2 t1 =
    toMilliseconds t2 + toMilliseconds t1 |> milliseconds


toMilliseconds : TypedTime -> Float
toMilliseconds t =
    case t of
        Milliseconds ms ->
            ms


toMillisecondsRounded : TypedTime -> Int
toMillisecondsRounded t =
    t
        |> toMilliseconds
        |> round


toMillisecondsString : TypedTime -> String
toMillisecondsString t =
    (toMilliseconds t |> String.fromFloat) ++ "ms"
