module Extra.Dict exposing (addKeys)

import Dict exposing (Dict)


addKeys : a -> List comparable -> Dict comparable a -> Dict comparable a
addKeys default keys dict =
    List.foldl
        (\key acc ->
            Dict.update key
                (\old ->
                    case old of
                        Just x ->
                            Just x

                        Nothing ->
                            Just default
                )
                acc
        )
        dict
        keys
