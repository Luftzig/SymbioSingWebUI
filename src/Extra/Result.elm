module Extra.Result exposing (..)

import Result
import List
import Result.Extra as Result


mapList : (a -> Result e b) -> Result e (List a) -> Result e (List b)
mapList f =
    Result.andThen ((List.map f) >> Result.combine)


