port module Main exposing (main)

import Array exposing (Array)
import Array.Extra
import Browser
import Element as UI
import Element.Background as UIBackground
import Element.Border as UIBorder
import Element.Font as UIFont
import Element.Input as UIInput
import Element.Region as UIRegion
import FlowIO exposing (..)
import Html
import Html.Attributes
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
    | RemoveDevice Int
    | ConnectToDevice Int
    | DeviceStatusChanged { deviceIndex : Int, status : String, details : Maybe DeviceDetails }
    | RequestControlServiceUpdates Int
    | DisconnectDevice Int
    | ControlServiceUpdate { deviceIndex : Int, status : Value }
    | SendCommand Int FlowIOCommand
    | ChangeCommandPortState Int FlowIO.Port FlowIO.PortState
    | ChangeCommandPwm Int Int
    | ChangeCommandAction Int FlowIO.FlowIOAction


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


sendMessage : msg -> Cmd msg
sendMessage msg =
    Task.perform (\() -> msg) <|
        Task.succeed ()


main : Program () Model Msg
main =
    Browser.document
        { init = \() -> ( initModel, sendMessage AddDevice )
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
    let
        updateDevice : Int -> (FlowIODevice -> FlowIODevice) -> Array FlowIODevice
        updateDevice index updater =
            Array.Extra.update index updater model.devices

        updateCommand deviceIndex newCommand =
            case Array.get deviceIndex model.devices |> Maybe.map .controlServiceStatus of
                Just _ ->
                    { model | devices = updateDevice deviceIndex (setLastCommand newCommand) }

                Nothing ->
                    Debug.log ("Tried to update command to device " ++ String.fromInt deviceIndex ++ " that does not exist, or does not have a control service.") model
    in
    case msg of
        ConnectToDevice deviceIndex ->
            case Array.get deviceIndex model.devices of
                Just device ->
                    if device.status == NotConnected then
                        ( { model | devices = updateDevice deviceIndex (setStatusTo Pending) }
                        , connectToDevice deviceIndex
                        )

                    else
                        ( model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        DeviceStatusChanged { deviceIndex, status, details } ->
            case Array.get deviceIndex model.devices of
                Just device ->
                    case status of
                        "connected" ->
                            ( { model | devices = updateDevice deviceIndex (setStatusTo Connected >> setDetailsTo details) }
                            , sendMessage (RequestControlServiceUpdates deviceIndex)
                            )

                        "disconnected" ->
                            ( { model
                                | devices = updateDevice deviceIndex (setStatusTo NotConnected)
                                , listeners = List.filter (\listener -> listener.deviceIndex /= deviceIndex) model.listeners
                              }
                            , Cmd.none
                            )

                        _ ->
                            ( { model | devices = updateDevice deviceIndex (setStatusTo Pending) }
                            , Cmd.none
                            )

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
                                | devices = updateDevice deviceIndex (setStatusTo NotConnected)
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
                    let
                        updateControlStatus device =
                            setControlServiceStatusTo
                                { newStatus | command = Maybe.withDefault defaultCommand <| Maybe.map .command device.controlServiceStatus }
                                device
                    in
                    ( { model | devices = updateDevice deviceIndex updateControlStatus }
                    , Cmd.none
                    )

        AddDevice ->
            ( { model | devices = Array.push defaultDevice model.devices }, createDevice () )

        RemoveDevice index ->
            Debug.todo "TODO: How should we handle removed devices?"

        SendCommand deviceIndex command ->
            -- TODO: Maybe we should update the model to reflect that?
            ( model, sendCommand { deviceIndex = deviceIndex, command = encodeCommand command } )

        ChangeCommandPortState deviceIndex port_ portState ->
            let
                command : FlowIOCommand
                command =
                    Array.get deviceIndex model.devices
                        |> Maybe.andThen getLastCommand
                        |> Maybe.withDefault defaultCommand

                newCommand =
                    command
                        |> setPort port_ portState
            in
            ( updateCommand deviceIndex newCommand, Cmd.none )

        ChangeCommandPwm deviceIndex newValue ->
            let
                command : FlowIOCommand
                command =
                    Array.get deviceIndex model.devices
                        |> Maybe.andThen getLastCommand
                        |> Maybe.withDefault defaultCommand

                newCommand =
                    command
                        |> setPumpPwm newValue
            in
            ( updateCommand deviceIndex newCommand, Cmd.none )

        ChangeCommandAction deviceIndex action ->
            let
                command : FlowIOCommand
                command =
                    Array.get deviceIndex model.devices
                        |> Maybe.andThen getLastCommand
                        |> Maybe.withDefault defaultCommand

                newCommand =
                    command
                        |> setAction action
            in
            ( updateCommand deviceIndex newCommand, sendMessage <| SendCommand deviceIndex newCommand )



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
                , UI.el [ UI.width <| UI.fillPortion 8 ] <| UI.text "Placeholder for scheduler"
                ]
            , footer
            ]


rust =
    UI.rgb255 183 65 14


grey : UI.Color
grey =
    UI.rgb 0.5 0.5 0.5


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


externClass : String -> UI.Attribute Msg
externClass class =
    UI.htmlAttribute <| Html.Attributes.class class


buttonCssIcon : String -> UI.Element Msg
buttonCssIcon class =
    UI.el [ UI.height <| UI.px 32, UI.width <| UI.px 32, UI.alignTop, externClass class ] <| UI.none


displayDeviceList : Model -> UI.Element Msg
displayDeviceList model =
    let
        showDevice : Int -> FlowIODevice -> UI.Element Msg
        showDevice index device =
            UI.row [ bottomBorder, UI.width UI.fill, UI.height <| UI.px 56, UI.spacing 5 ]
                [ UI.el [] <| UI.text (String.fromInt (index + 1) ++ ": ")
                , case device.status of
                    NotConnected ->
                        UI.row []
                        [
                        UIInput.button [ UIRegion.description "Connect" ]
                            { label = buttonCssIcon "icon-disconnected", onPress = Just <| ConnectToDevice index }
                        , if index > 0 then
                            UIInput.button [UI.alignRight, UIRegion.description "Remove", UIFont.heavy]
                            {label = UI.text "-", onPress = Just <| RemoveDevice index }
                            else UI.none
                        ]

                    Pending ->
                        UI.el [ UIRegion.description "waiting connection" ] <| buttonCssIcon "icon-loading"

                    Connected ->
                        UI.column []
                            [ UI.row [ UI.width UI.fill ]
                                [ UI.text <| Maybe.withDefault "Unknown" <| Maybe.map .name device.details
                                , UIInput.button [ UIRegion.description "Disconnect", UI.alignLeft ]
                                    { label = buttonCssIcon "icon-connected", onPress = Just <| DisconnectDevice index }
                                ]
                            , UI.paragraph [ UIFont.size 10, UI.width UI.fill ]
                                [ UI.text "id: "
                                , UI.text <| Maybe.withDefault "Unknown" <| Maybe.map .id device.details
                                ]
                            ]
                ]

        listHeader =
            UI.el [ UI.width UI.fill ] <| UI.text "Devices"

        buttons =
            UI.row [ UI.width UI.fill ]
                [ UIInput.button []
                    { label = UI.el [ UIRegion.description "Add Device", UIFont.heavy ] <| UI.text "+"
                    , onPress = Just AddDevice
                    }
                ]
    in
    UI.column [ UI.alignTop, UI.width <| UI.fillPortion 2, UI.spacing 5, rightBorder, UI.padding 5, UI.height UI.fill ]
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
                        UI.column [ UI.width UI.fill, UIFont.size 16 ]
                            [ UI.text
                                ("Status for "
                                    ++ String.fromInt (index + 1)
                                    ++ ": "
                                    ++ (Maybe.map .name device.details |> Maybe.withDefault "")
                                )
                            , displayStatusDetails index hardwareStatus
                            , displayControls index hardwareStatus.command
                            ]

                _ ->
                    Nothing

        displayControls : Int -> FlowIOCommand -> UI.Element Msg
        displayControls deviceIndex command =
            UI.wrappedRow [ UI.width UI.fill ]
                [ pwmSlider (ChangeCommandPwm deviceIndex) command.pumpPwm
                , actions (ChangeCommandAction deviceIndex) command.action
                , displayPorts (ChangeCommandPortState deviceIndex) command.ports
                ]

        pwmSlider : (Int -> Msg) -> Int -> UI.Element Msg
        pwmSlider onUpdate currentValue =
            let
                fullWidth =
                    160

                filled =
                    toFloat currentValue / 255
            in
            UIInput.slider
                [ UI.width <| UI.px fullWidth
                , UI.height <| UI.px 12
                , UI.behindContent <|
                    UI.el
                        [ UI.width UI.fill
                        , UI.height <| UI.px 8
                        , UI.padding 2
                        , UI.centerX
                        , UI.centerY
                        , UIBackground.color grey
                        , UIBorder.rounded 4
                        ]
                        UI.none
                , UI.behindContent <|
                    UI.el
                        [ UI.width <| UI.px <| round <| (fullWidth - 4) * filled
                        , UI.height <| UI.px 4
                        , UI.spacing 2
                        , UI.alignLeft
                        , UI.centerY
                        , UIBackground.color rust
                        , UIBorder.rounded 2
                        ]
                        UI.none
                ]
                { label = UIInput.labelAbove [ UIFont.size 12, UIFont.center ] <| UI.text "PWM"
                , onChange = round >> onUpdate
                , max = 255
                , min = 0
                , step = Just 1
                , thumb = UIInput.defaultThumb
                , value = filled
                }

        actions : (FlowIOAction -> Msg) -> FlowIOAction -> UI.Element Msg
        actions onUpdate currentValue =
            UI.row [ UI.spacing 5, UI.padding 5 ]
                [ UIInput.button [] { label = buttonCssIcon "icon-inflate", onPress = Just <| onUpdate Inflate }
                , UIInput.button [] { label = buttonCssIcon "icon-vacuum", onPress = Just <| onUpdate Vacuum }
                , UIInput.button [] { label = buttonCssIcon "icon-release", onPress = Just <| onUpdate Release }
                , UIInput.button [] { label = buttonCssIcon "icon-stop", onPress = Just <| onUpdate Stop }
                ]

        displayPorts : (Port -> PortState -> Msg) -> PortsState -> UI.Element Msg
        displayPorts onUpdate ports =
            let
                checkbox label port_ currentValue =
                    UIInput.checkbox []
                        { onChange = portFromBool >> onUpdate port_
                        , label = UIInput.labelAbove [ UIFont.size 12, UIFont.center ] <| UI.text label
                        , icon = UIInput.defaultCheckbox
                        , checked = isPortOpen currentValue
                        }
            in
            UI.row [ UI.spacing 5, UI.padding 5 ]
                [ checkbox "1" Port1 ports.port1
                , checkbox "2" Port2 ports.port2
                , checkbox "3" Port3 ports.port3
                , checkbox "4" Port4 ports.port4
                , checkbox "5" Port5 ports.port5
                ]

        displayStatusDetails : Int -> ControlServiceStatus -> UI.Element Msg
        displayStatusDetails deviceIndex details =
            let
                displayPort label status =
                    UI.el
                        [ UI.width <| UI.px 24
                        , UI.height <| UI.px 24
                        , UIBorder.width 2
                        , UIBorder.color darkGrey
                        , UI.padding 2
                        , UIBackground.color
                            (if status then
                                rust

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
                        , UIFont.center
                        ]
                    <|
                        UI.el [ UI.centerY, UI.centerX ] <|
                            UI.text label
            in
            UI.column [ UI.spacing 2, UI.width UI.fill ]
                [ if details.active then
                    UI.text "Active"

                  else
                    UI.text "Inactive"
                , UI.row [ UI.spacing 2, UI.width UI.fill, UI.centerX ]
                    [ displayPort "In" details.inlet
                    , displayPort "1" details.port1
                    , displayPort "2" details.port2
                    , displayPort "3" details.port3
                    , displayPort "4" details.port4
                    , displayPort "5" details.port5
                    , displayPort "Out" details.outlet
                    ]
                , UI.row [ UI.spaceEvenly, UI.width UI.fill ]
                    [ UI.text
                        ("Pump 1: "
                            ++ (if details.pump1 then
                                    "On"

                                else
                                    "Off"
                               )
                        )
                    , UI.text
                        ("Pump 2: "
                            ++ (if details.pump2 then
                                    "On"

                                else
                                    "Off"
                               )
                        )
                    ]
                ]

        listHeader =
            UI.el [ UI.width UI.fill ] <| UI.text "Hardware Status"
    in
    UI.column
        [ UI.alignTop
        , UI.width <| UI.fillPortion 2
        , UI.spacing 5
        , rightBorder
        , UI.padding 5
        , UI.height UI.fill
        ]
        (listHeader
            :: (Array.Extra.indexedMapToList displayStatus devices |> List.filterMap identity)
        )


header : UI.Element Msg
header =
    UI.el ([ UI.centerX, UI.alignTop ] ++ [ UIFont.size 24 ]) <| UI.text "FlowIO Scheduler"


footer : UI.Element Msg
footer =
    UI.el [ UI.alignBottom, UI.height <| UI.px 24 ] UI.none
