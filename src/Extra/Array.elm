module Extra.Array exposing (..)


import Array exposing (Array)
last : Array a -> Maybe a
last array =
    Array.get (Array.length array - 1) array