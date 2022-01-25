module TypedTimeTests exposing (typedTimeTests)

import Expect
import Extra.TypedTime as TypedTime
import Fuzz
import Test exposing (Test, describe, fuzz, only, test)


typedTimeTests : Test
typedTimeTests =
    describe "operators"
        [ test "divide" <|
            \_ -> Expect.equal (TypedTime.divide 0.5 (TypedTime.minutes 1)) (TypedTime.seconds 120)
        , fuzz Fuzz.float "toHours" <|
            \x ->
                TypedTime.milliseconds (x * 60 * 60 * 1000)
                    |> TypedTime.toHours
                    |> Expect.within (Expect.Absolute 0.0001) x
        , describe "toFormattedString"
            [ test "HHMMSSss seconds" <|
                \() ->
                    TypedTime.seconds 3.2
                        |> TypedTime.toFormattedString TypedTime.HoursMinutesSecondsHundredths
                        |> Expect.equal "00:00:03.20"
            , test "HHMMSSss minutes" <|
                              \() ->
                                  TypedTime.seconds 73.2
                                      |> TypedTime.toFormattedString TypedTime.HoursMinutesSecondsHundredths
                                      |> Expect.equal "00:01:13.20"
            , test "HHMMSSss hours" <|
                              \() ->
                                  TypedTime.minutes 61.25
                                      |> TypedTime.toFormattedString TypedTime.HoursMinutesSecondsHundredths
                                      |> Expect.equal "01:01:15.00"
            ]
        ]
