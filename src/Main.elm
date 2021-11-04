module Main exposing (main)

import Array exposing (Array)
import Array.Extra
import Browser
import Color.Dracula as Dracula
import Element as UI
import Element.Background as UIBackground
import Element.Border as UIBorder
import Element.Font as UIFont
import Element.Input as UIInput
import Element.Region as UIRegion
import FlowIO exposing (..)
import Html
import Json.Decode exposing (Value, decodeValue)
import Json.Encode
import List.Extra as LE
import Scheduler
import Styles
    exposing
        ( bottomBorder
        , buttonCssIcon
        , darkGrey
        , grey
        , inflateButton
        , releaseButton
        , rightBorder
        , rust
        , stopButton
        , vacuumButton
        )
import Task


type alias Model =
    { devices : Array FlowIODevice
    , listeners : List { deviceIndex : Int, to : FlowIOService, shouldListen : Bool }
    , scheduler : Scheduler.Model
    , commandClicked : Maybe { deviceIndex : Int, action : FlowIOAction }
    }


type Msg
    = AddDevice
    | RemoveDevice Int
    | ConnectToDevice Int
    | DeviceStatusChanged { deviceIndex : Int, status : String, details : Maybe Value }
    | RequestControlServiceUpdates Int
    | DisconnectDevice Int
    | ControlServiceUpdate { deviceIndex : Int, status : Value }
    | SendCommand Int FlowIOCommand
    | ChangeCommandPortState Int FlowIO.Port FlowIO.PortState
    | ChangeCommandPwm Int Int
      --| ChangeCommandAction Int FlowIO.FlowIOAction
    | ActionClicked Int FlowIOAction
    | ActionReleased
    | SchedulerMessage Scheduler.Msg


initModel : Model
initModel =
    { devices = Array.empty
    , listeners = []
    , scheduler = Scheduler.initModel
    , commandClicked = Nothing
    }


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
         , Scheduler.subscriptions model.scheduler |> Sub.map SchedulerMessage
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
        updateDevices : Array FlowIODevice -> Model -> Model
        updateDevices newDevices model_ =
            let
                scheduler =
                    model_.scheduler

                newScheduler =
                    { scheduler | devices = newDevices }
            in
            { model_ | devices = newDevices, scheduler = newScheduler }

        updateDevice : Int -> (FlowIODevice -> FlowIODevice) -> Array FlowIODevice
        updateDevice index updater =
            Array.Extra.update index updater model.devices

        updateCommand deviceIndex newCommand =
            case Array.get deviceIndex model.devices |> Maybe.map .controlServiceStatus of
                Just _ ->
                    updateDevices (updateDevice deviceIndex (setLastCommand newCommand)) model

                Nothing ->
                    Debug.log ("Tried to update command to device " ++ String.fromInt deviceIndex ++ " that does not exist, or does not have a control service.") model
    in
    case msg of
        ConnectToDevice deviceIndex ->
            case Array.get deviceIndex model.devices of
                Just device ->
                    if device.status == NotConnected then
                        ( updateDevices (updateDevice deviceIndex (setStatusTo Pending)) model
                        , connectToDevice deviceIndex
                        )

                    else
                        ( model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        DeviceStatusChanged { deviceIndex, status, details } ->
            case Array.get deviceIndex model.devices of
                Just _ ->
                    case status of
                        "connected" ->
                            let
                                decodedDetails =
                                    details
                                        |> Maybe.map (Json.Decode.decodeValue deviceDetailsDecoder)
                                        |> Maybe.andThen Result.toMaybe
                            in
                            ( updateDevices (updateDevice deviceIndex (setStatusTo Connected >> setDetailsTo decodedDetails)) model
                            , sendMessage (RequestControlServiceUpdates deviceIndex)
                            )

                        "disconnected" ->
                            ( { model
                                | listeners = List.filter (\listener -> listener.deviceIndex /= deviceIndex) model.listeners
                              }
                                |> updateDevices (updateDevice deviceIndex (setStatusTo NotConnected))
                            , Cmd.none
                            )

                        _ ->
                            ( updateDevices (updateDevice deviceIndex (setStatusTo Pending)) model
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
                                | listeners = List.filter (\listener -> listener.deviceIndex /= deviceIndex) model.listeners
                              }
                                |> updateDevices (updateDevice deviceIndex (setStatusTo NotConnected))
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
                    ( model |> updateDevices (updateDevice deviceIndex updateControlStatus)
                    , Cmd.none
                    )

        AddDevice ->
            ( updateDevices (Array.push defaultDevice model.devices) model, createDevice () )

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

        SchedulerMessage message ->
            let
                ( scheduler, cmd ) =
                    Scheduler.update message model.scheduler
            in
            ( { model | scheduler = scheduler }, Cmd.map SchedulerMessage cmd )

        ActionClicked deviceIndex action ->
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
            ( updateCommand deviceIndex newCommand
                |> (\model_ ->
                        { model_ | commandClicked = Just { deviceIndex = deviceIndex, action = action } }
                   )
            , sendMessage <| SendCommand deviceIndex newCommand
            )

        ActionReleased ->
            let
                cmd =
                    case model.commandClicked of
                        Just { deviceIndex } ->
                            stopAll deviceIndex

                        Nothing ->
                            Cmd.none
            in
            ( { model | commandClicked = Nothing }, cmd )


view : Model -> Browser.Document Msg
view model =
    { title = "FlowIO"
    , body = [ body model ]
    }


body : Model -> Html.Html Msg
body model =
    UI.layout
        [ UI.width <| UI.fill
        , UI.height <| UI.fill
        , UI.padding 20
        , UIFont.color Dracula.white
        , UIFont.family [ UIFont.typeface "Overpass", UIFont.typeface "Open Sans", UIFont.typeface "Helvetica", UIFont.sansSerif ]
        , UIFont.size 15
        , UIBackground.color Dracula.black
        ]
    <|
        UI.column [ UI.width <| UI.fill, UI.height <| UI.fill ]
            [ header
            , UI.row [ UI.spacing 10, UI.width UI.fill, UI.height <| UI.fillPortion 10, UI.alignTop ]
                [ displayDeviceList model
                , displayHardwareStatus model
                , UI.el [ UI.width <| UI.fillPortion 8, UI.alignTop ] <|
                    UI.map SchedulerMessage <|
                        UI.html <|
                            Scheduler.view model.scheduler
                ]
            , footer
            ]


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
                            [ UIInput.button [ UIRegion.description "Connect" ]
                                { label = buttonCssIcon "icon-disconnected" "Disconnected", onPress = Just <| ConnectToDevice index }
                            , if index > 0 then
                                UIInput.button [ UI.alignRight, UIRegion.description "Remove", UIFont.heavy ]
                                    { label = UI.text "-", onPress = Just <| RemoveDevice index }

                              else
                                UI.none
                            ]

                    Pending ->
                        UI.el [ UIRegion.description "waiting connection" ] <| buttonCssIcon "icon-loading" "Awaiting connection"

                    Connected ->
                        UI.column []
                            [ UI.row [ UI.width UI.fill ]
                                [ UI.text <| Maybe.withDefault "Unknown" <| Maybe.map .name device.details
                                , UIInput.button [ UIRegion.description "Disconnect", UI.alignLeft ]
                                    { label = buttonCssIcon "icon-connected" "Connected", onPress = Just <| DisconnectDevice index }
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
                , actions (ActionClicked deviceIndex) command.action
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
        actions onMouseDown currentValue =
            UI.row [ UI.spacing 5, UI.padding 5 ]
                [ inflateButton (onMouseDown Inflate) ActionReleased
                , vacuumButton (onMouseDown Vacuum) ActionReleased
                , releaseButton (onMouseDown Release) ActionReleased
                , stopButton (onMouseDown Stop) ActionReleased
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
                                Dracula.white
                            )
                        , UIFont.size 10
                        , UIFont.color
                            (if status then
                                Dracula.white

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
    UI.el [ UI.centerX, UI.alignTop, UIFont.color Dracula.white, UIFont.size 24 ] <| UI.text "FlowIO Scheduler"


footer : UI.Element Msg
footer =
    UI.el [ UI.alignBottom, UI.height <| UI.px 24 ] UI.none
