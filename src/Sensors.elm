module Sensors exposing (Model, barChart, initialModel, update, view)

import Array
import Array.Extra
import Chart as C
import Chart.Attributes as CA
import Color exposing (Color)
import Color.Interpolate
import Dict exposing (Dict)
import Dict.Extra
import Element as El
import Element.Background
import Element.Font
import Element.Input
import Float.Extra
import FlowIO exposing (AnalogReadings, DeviceId)
import Images exposing (bodyImage, lightBlue, midnightBlue)
import Messages exposing (..)
import Styles exposing (externClass, fullWidth)
import Time


type alias Model =
    { sensorData : Dict DeviceId (List ( Time.Posix, AnalogReadings ))
    , selectedDevice : Maybe DeviceId
    }


initialModel : Model
initialModel =
    { sensorData = Dict.empty
    , selectedDevice = Nothing
    }


view : Model -> El.Element SensorsMsg
view model =
    let
        showDevice ( device, data ) =
            let
                isSelectedDevice =
                    model.selectedDevice == Just device
            in
            Element.Input.button
                (Styles.card
                    ++ (if isSelectedDevice then
                            Styles.colorsPrimary

                        else
                            Styles.colorsNormal
                       )
                    ++ [ El.width <| El.px 236 ]
                )
                { onPress =
                    Just <|
                        DeviceSelected <|
                            if not isSelectedDevice then
                                Just device

                            else
                                Nothing
                , label =
                    El.column
                        [ El.height <| El.px 100 ]
                        [ El.el [ fullWidth ] <| El.text device
                        , case List.head data of
                            Just ( posix, readings ) ->
                                El.el
                                    [ fullWidth
                                    , El.paddingEach { left = 36, top = 8, bottom = 0, right = 0 }
                                    ]
                                <|
                                    barChart { width = 200, height = 60 } posix readings

                            Nothing ->
                                El.el [ Styles.fontSize.small ] <| El.text "no data"
                        ]
                }

        imageData : Maybe { sensor1 : { width : Float, fill : String } }
        imageData =
            model.selectedDevice
                |> Maybe.andThen (\deviceId -> Dict.get deviceId model.sensorData)
                |> Maybe.andThen List.head
                |> Maybe.andThen (\( _, readings ) -> Array.get 15 readings)
                |> Maybe.map
                    (\v ->
                        { sensor1 =
                            { width = Float.Extra.interpolateFrom 0 100 (toFloat v / FlowIO.sensorsResolution)
                            , fill =
                                Color.Interpolate.interpolate Color.Interpolate.HSL
                                    midnightBlue
                                    lightBlue
                                    (toFloat v / FlowIO.sensorsResolution)
                                    |> Color.toCssString
                            }
                        }
                    )
    in
    El.row [ fullWidth ]
        [ El.el
            [ externClass "fix-body"
            , El.width <| El.fillPortion 2
            , El.height <| El.maximum 700 <| El.fill
            ]
          <|
            El.html <|
                bodyImage imageData
        , El.column [ El.width <| El.fillPortion 1, El.alignTop ] <|
            List.map showDevice <|
                Dict.toList model.sensorData
        ]


maxReadings =
    1000


update : Model -> SensorsMsg -> ( Model, Cmd SensorsMsg )
update model msg =
    case msg of
        NewReading deviceId timestamp analogReadings ->
            ( { model
                | sensorData =
                    Dict.Extra.insertDedupe
                        (\old new -> List.append new old |> List.take maxReadings)
                        deviceId
                        [ ( timestamp, analogReadings ) ]
                        model.sensorData
              }
            , Cmd.none
            )

        DeviceSelected maybeDevice ->
            ( { model | selectedDevice = maybeDevice }, Cmd.none )


barChart : { width : Float, height : Float } -> Time.Posix -> AnalogReadings -> El.Element SensorsMsg
barChart { width, height } timestamp analogReadings =
    El.html <|
        C.chart
            [ CA.width width
            , CA.height height
            , CA.range
                [ CA.lowest 1 CA.orLower
                , CA.highest 16 CA.orHigher
                ]
            , CA.domain
                [ CA.lowest 0 CA.exactly
                , CA.highest FlowIO.sensorsResolution CA.exactly
                ]
            ]
            [ C.xLabels [ CA.ints ]
            , C.yLabels []
            , C.bars [ CA.x1 (.index >> (+) 1 >> toFloat) ]
                [ C.bar (.value >> toFloat) [] ]
                (analogReadings |> Array.Extra.indexedMapToList (\i v -> { index = i, value = v }))
            ]
