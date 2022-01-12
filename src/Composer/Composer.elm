module Composer.Composer exposing (Model, view)

import Element exposing (Element, column)
import Extra.Resource exposing (Resource)
import File exposing (File)
import Notation exposing (ConversionParameters, HapticScore)



type alias Model =
    { sourceFile : File
    , hapticScore : Resource String HapticScore
    , conversionParams : ConversionParameters
    }


view : Model -> Element msg
view model =
    column []
        [ loadFile model
        , showHapticScore model.hapticScore
        , showConversionParams model.conversionParams
        ]


loadFile =
