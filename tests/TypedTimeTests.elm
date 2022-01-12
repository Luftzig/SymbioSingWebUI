module TypedTimeTests exposing (..)

import Expect
import Extra.TypedTime as TypedTime
import Test exposing (Test, describe, test)
typedTimeSuite : Test
typedTimeSuite =
    describe "operators" [
    test "divide" <|
        \_ -> Expect.equal (TypedTime.divide 0.5 (TypedTime.minutes 1)) (TypedTime.seconds 120)
    ]