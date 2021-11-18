module Sensors exposing (Model, Msg(..), initialModel, update, view)

import Dict exposing (Dict)
import Dict.Extra
import Element as El
import FlowIO exposing (AnalogReadings, DeviceId)
import Time


type alias Model =
    { sensorData : Dict DeviceId (List ( Time.Posix, AnalogReadings )) }


initialModel : Model
initialModel =
    { sensorData = Dict.empty }


type Msg
    = NewReading DeviceId Time.Posix AnalogReadings


view : Model -> El.Element msg
view model =
    El.el [] <| El.text "Placeholder"


update : Model -> Msg -> ( Model, Cmd Msg )
update model msg =
    case msg of
        NewReading deviceId timestamp analogReadings ->
            ( { model
                | sensorData =
                    Dict.Extra.insertDedupe
                        (++)
                        deviceId
                        [ ( timestamp, analogReadings ) ]
                        model.sensorData
              }
            , Cmd.none
            )
