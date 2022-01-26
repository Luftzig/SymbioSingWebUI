module Extra.TypedTime exposing
    ( Format(..)
    , TypedTime
    , add
    , divide
    , equal
    , equalWithin
    , hours
    , lessThan
    , milliseconds
    , minutes
    , multiply
    , seconds
    , toFormattedString
    , toHours
    , toMilliseconds
    , toMillisecondsRounded
    , toMillisecondsString
    , toMinutes
    , zero
    , sum, fromPosix, subtract, greaterEqual)

import Float.Extra
import Time exposing (Posix)


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


hours : Float -> TypedTime
hours h =
    Milliseconds (h * 60 * 60 * 1000)


fromPosix : Posix -> TypedTime
fromPosix posix =
    posix |> Time.posixToMillis |> toFloat |> milliseconds


zero : TypedTime
zero =
    Milliseconds 0


equal : TypedTime -> TypedTime -> Bool
equal op2 op1 =
    toMilliseconds op1 == toMilliseconds op2


equalWithin : TypedTime -> TypedTime -> TypedTime -> Bool
equalWithin tolerance op2 op1 =
    Float.Extra.equalWithin (toMilliseconds tolerance) (toMilliseconds op1) (toMilliseconds op2)


lessThan : TypedTime -> TypedTime -> Bool
lessThan t2 t1 =
    toMilliseconds t1 < toMilliseconds t2

greaterEqual : TypedTime -> TypedTime -> Bool
greaterEqual t2 t1 =
    not (t1 |> lessThan t2)



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


subtract : TypedTime -> TypedTime -> TypedTime
subtract t2 t1 =
    toMilliseconds t1 - toMilliseconds t2 |> milliseconds


sum : List TypedTime -> TypedTime
sum ts =
    ts
        |> List.map toMilliseconds
        |> List.sum
        |> milliseconds


toMilliseconds : TypedTime -> Float
toMilliseconds t =
    case t of
        Milliseconds ms ->
            ms


toSeconds : TypedTime -> Float
toSeconds (Milliseconds s) =
    s / 1000


toMinutes : TypedTime -> Float
toMinutes (Milliseconds ms) =
    ms / (60 * 1000)


toHours : TypedTime -> Float
toHours t =
    case t of
        Milliseconds ms ->
            ms / (60 * 60 * 1000)


toMillisecondsRounded : TypedTime -> Int
toMillisecondsRounded t =
    t
        |> toMilliseconds
        |> round


toMillisecondsString : TypedTime -> String
toMillisecondsString t =
    (toMilliseconds t |> String.fromFloat) ++ "ms"


type Format
    = HoursMinutesSecondsHundredths


toFormattedString : Format -> TypedTime -> String
toFormattedString format t =
    let
        hrs =
            toHours t |> truncate

        minsReminder =
            t |> subtract (hours <| toFloat hrs) |> toMinutes |> truncate |> abs

        mins =
            toMinutes t |> truncate |> toFloat |> abs

        secsReminder =
            t |> subtract (minutes mins) |> toSeconds |> abs

        secsTrunc =
            secsReminder
                |> truncate

        millis =
            secsReminder
                - toFloat secsTrunc
                |> (*) 1000
                |> round

        --secsFracString =
        --    String.fromFloat secsFraction |> String.dropLeft 2
    in
    case format of
        HoursMinutesSecondsHundredths ->
            (String.fromInt hrs |> String.padLeft 2 '0')
                ++ ":"
                ++ (String.fromInt minsReminder |> String.padLeft 2 '0')
                ++ ":"
                ++ (String.fromInt secsTrunc |> String.padLeft 2 '0')
                ++ "."
                ++ (millis |> String.fromInt |> String.left 2 |> String.padRight 2 '0')
