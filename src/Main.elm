port module Main exposing (main)

import Array exposing (Array)
import Array.Extra
import Browser
import Element as UI
import Element.Background as UIBackground
import Element.Border as UIBorder
import Element.Font as UIFont
import Element.Input as UIInput
import FlowIO exposing (..)
import Html
import Json.Decode exposing (Value, decodeValue)
import Json.Encode
import List.Extra as LE
import Scheduler
import Task


type alias Model =
    { devices : Array FlowIODevice
    , listeners : List { deviceIndex : Int, to : FlowIOService, shouldListen : Bool }
    }


type Msg
    = AddDevice
    | ConnectToDevice Int
    | DeviceStatusChanged { deviceIndex : Int, status : String, details : Maybe DeviceDetails }
    | RequestControlServiceUpdates Int
    | DisconnectDevice Int
    | ControlServiceUpdate { deviceIndex : Int, status : Value }
    | SendCommand Int FlowIOCommand


initModel : Model
initModel =
    { devices = Array.empty
    , listeners = []
    }


port createDevice : () -> Cmd msg


port connectToDevice : Int -> Cmd msg


port disconnectDevice : Int -> Cmd msg


port deviceStatusChanged : ({ deviceIndex : Int, status : String, details : Maybe DeviceDetails } -> msg) -> Sub msg


port controlServiceStatusChanged : ({ deviceIndex : Int, status : Value } -> msg) -> Sub msg


port listenToControlService : Int -> Cmd msg


port sendCommand : { deviceIndex : Int, command : Json.Encode.Value } -> Cmd msg


main : Program () Model Msg
main =
    Browser.document
        { init = \() -> ( initModel, Task.perform (\() -> AddDevice) (Task.succeed ()) )
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        shouldListenToControlService =
            List.any
                (\{ to, shouldListen } ->
                    case ( to, shouldListen ) of
                        ( ControlService, True ) ->
                            True

                        _ ->
                            False
                )
                model.listeners
    in
    Sub.batch
        ([ deviceStatusChanged DeviceStatusChanged
         ]
            ++ (if shouldListenToControlService then
                    [ controlServiceStatusChanged ControlServiceUpdate ]

                else
                    []
               )
        )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ConnectToDevice deviceIndex ->
            case Array.get deviceIndex model.devices of
                Just device ->
                    if device.status == NotConnected then
                        ( { model | devices = Array.Extra.update deviceIndex (setStatusTo Pending) model.devices }, connectToDevice deviceIndex )

                    else
                        ( model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        DeviceStatusChanged { deviceIndex, status, details } ->
            case Array.get deviceIndex model.devices of
                Just device ->
                    case status of
                        "connected" ->
                            ( { model | devices = Array.Extra.update deviceIndex (setStatusTo Connected >> setDetailsTo details) model.devices }, Cmd.none )

                        "disconnected" ->
                            ( { model | devices = Array.Extra.update deviceIndex (setStatusTo NotConnected) model.devices }, Cmd.none )

                        _ ->
                            ( { model | devices = Array.Extra.update deviceIndex (setStatusTo Pending) model.devices }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        RequestControlServiceUpdates requestedDeviceIndex ->
            let
                hasListener =
                    \{ deviceIndex, to } -> deviceIndex == requestedDeviceIndex && to == ControlService

                controlServiceListenerState =
                    LE.find hasListener model.listeners

                newListener =
                    { deviceIndex = requestedDeviceIndex, to = ControlService, shouldListen = True }
            in
            case controlServiceListenerState of
                Nothing ->
                    ( { model | listeners = newListener :: model.listeners }
                    , listenToControlService requestedDeviceIndex
                    )

                Just { shouldListen } ->
                    if shouldListen then
                        -- There is already a listener
                        ( model, Cmd.none )

                    else
                        ( { model | listeners = LE.setIf hasListener newListener model.listeners }
                        , listenToControlService requestedDeviceIndex
                        )

        DisconnectDevice deviceIndex ->
            case Array.get deviceIndex model.devices of
                Just device ->
                    case device.status of
                        Connected ->
                            ( { model
                                | devices = Array.Extra.update deviceIndex (setStatusTo NotConnected) model.devices
                                , listeners = List.filter (\listener -> listener.deviceIndex /= deviceIndex) model.listeners
                              }
                            , disconnectDevice deviceIndex
                            )

                        NotConnected ->
                            -- Nothing to do
                            ( model, Cmd.none )

                        Pending ->
                            -- This is an error to request to disconnect while pending
                            Debug.log ("Requested to disconnect from device " ++ Debug.toString deviceIndex ++ ", but device is pending connection.") ( model, Cmd.none )

                Nothing ->
                    Debug.log "Requested to remove a non-existing device" <| ( model, Cmd.none )

        ControlServiceUpdate { deviceIndex, status } ->
            let
                controlServiceStatus =
                    decodeValue controlServiceStatusDecoder status

                maybeDevice =
                    Array.get deviceIndex model.devices
            in
            case ( controlServiceStatus, maybeDevice ) of
                ( Err error, _ ) ->
                    Debug.log ("Failed to decode control service status. Error: " ++ Json.Decode.errorToString error) ( model, Cmd.none )

                ( _, Nothing ) ->
                    Debug.log ("Status update for device " ++ Debug.toString deviceIndex ++ " received, but device does not exist") ( model, Cmd.none )

                ( Ok newStatus, Just _ ) ->
                    ( { model | devices = Array.Extra.update deviceIndex (setControlServiceStatusTo newStatus) model.devices }
                    , Cmd.none
                    )

        AddDevice ->
            ( { model | devices = Array.push defaultDevice model.devices }, createDevice () )

        SendCommand deviceIndex command ->
            -- TODO: Maybe we should update the model to reflect that?
            ( model, sendCommand { deviceIndex = deviceIndex, command = encodeCommand command } )


view : Model -> Browser.Document Msg
view model =
    { title = "FlowIO"
    , body = [ body model ]
    }


body : Model -> Html.Html Msg
body model =
    UI.layout [ UI.width <| UI.fill, UI.height <| UI.fill, UI.padding 20 ] <|
        UI.column [ UI.width <| UI.fill, UI.height <| UI.fill ]
            [ header
            , UI.row [ UI.spacing 10, UI.width UI.fill, UI.height <| UI.fillPortion 10, UI.alignTop ]
                [ displayDeviceList model
                , displayHardwareStatus model
                , UI.el [UI.width <| UI.fillPortion 8] <| UI.text "Placeholder for scheduler"
                ]
            , footer
            ]


darkGrey : UI.Color
darkGrey =
    UI.rgb 0.2 0.2 0.2


transparent : UI.Color
transparent =
    UI.rgba 0 0 0 0


white : UI.Color
white =
    UI.rgb 1 1 1


bottomBorder =
    UIBorder.widthEach { bottom = 2, left = 0, right = 0, top = 0 }


rightBorder =
    UIBorder.widthEach { bottom = 0, left = 0, right = 2, top = 0 }


displayDeviceList : Model -> UI.Element Msg
displayDeviceList model =
    let
        showDevice : Int -> FlowIODevice -> UI.Element Msg
        showDevice index device =
            UI.row [ bottomBorder, UI.width UI.fill, UI.height <| UI.px 56, UI.spacing 5 ]
                [ UI.el [] <| UI.text (String.fromInt (index + 1) ++ ": ")
                , case device.status of
                    NotConnected ->
                        UIInput.button [] { label = UI.text "connect", onPress = Just <| ConnectToDevice index }

                    Pending ->
                        UI.el [] <| UI.text "Connecting..."

                    Connected ->
                        -- TODO: Replace with a button to disconnect
                        UI.column []
                            [ UI.text "Connected"
                            , showDeviceDetails device.details
                            ]
                ]

        showDeviceDetails : Maybe DeviceDetails -> UI.Element Msg
        showDeviceDetails maybeDetails =
            case maybeDetails of
                Just details ->
                    UI.el [ UIFont.size 10 ] <| UI.text (details.name ++ " (" ++ details.id ++ ")")

                _ ->
                    UI.none

        listHeader =
            UI.el [ UI.width UI.fill ] <| UI.text "Devices"

        buttons =
            UI.row [ UI.width UI.fill ]
                [ UI.el [] <| UI.text "+"
                ]
    in
    UI.column [ UI.alignTop, UI.width <| UI.fillPortion 2, UI.spacing 5, rightBorder, UI.padding 5 ]
        (listHeader
            :: (Array.indexedMap
                    showDevice
                    model.devices
                    |> Array.toList
               )
            ++ [ buttons ]
        )


displayHardwareStatus : Model -> UI.Element Msg
displayHardwareStatus { devices } =
    let
        displayStatus : Int -> FlowIODevice -> Maybe (UI.Element Msg)
        displayStatus index device =
            case ( device.status, device.controlServiceStatus ) of
                ( Connected, Just hardwareStatus ) ->
                    Just <|
                        UI.column []
                            [ UI.text ("Status for " ++ String.fromInt (index + 1))
                            , displayStatusDetails hardwareStatus
                            ]

                _ ->
                    Nothing

        displayStatusDetails : ControlServiceStatus -> UI.Element Msg
        displayStatusDetails details =
            let
                displayPort label status =
                    UI.el
                        [ UI.width <| UI.px 16
                        , UI.height <| UI.px 16
                        , UIBorder.width 2
                        , UIBackground.color
                            (if status then
                                darkGrey

                             else
                                white
                            )
                        , UIFont.size 10
                        , UIFont.color
                            (if status then
                                white

                             else
                                darkGrey
                            )
                        ]
                    <|
                        UI.text label
            in
            UI.column [ UI.spacing 2 ]
                [ if details.active then
                    UI.text "Active"

                  else
                    UI.text "Inactive"
                , UI.row [ UI.spacing 2 ]
                    [ displayPort "In" details.inlet
                    , displayPort "1" details.port1
                    , displayPort "2" details.port2
                    , displayPort "3" details.port3
                    , displayPort "4" details.port4
                    , displayPort "5" details.port5
                    , displayPort "Out" details.outlet
                    ]
                , UI.row [ UI.spaceEvenly ]
                    [ UI.text
                        ("Pump 1: "
                            ++ (if details.pump1 then
                                    "Active"

                                else
                                    "Inactive"
                               )
                        )
                    , UI.text
                        ("Pump 2: "
                            ++ (if details.pump2 then
                                    "Active"

                                else
                                    "Inactive"
                               )
                        )
                    ]
                ]

        listHeader =
            UI.el [ UI.width UI.fill ] <| UI.text "Hardware Status"
    in
    UI.column [ UI.alignTop, UI.width <| UI.fillPortion 2, UI.spacing 5, rightBorder, UI.padding 5 ]
        (listHeader
            :: (Array.Extra.indexedMapToList displayStatus devices |> List.filterMap identity)
        )


header : UI.Element Msg
header =
    UI.el ([ UI.centerX, UI.alignTop ] ++ [ UIFont.size 24 ]) <| UI.text "FlowIO Scheduler"


footer : UI.Element Msg
footer =
    UI.el [ UI.alignBottom, UI.height <| UI.px 24 ] UI.none
