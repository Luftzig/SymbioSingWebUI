module Main exposing (main)

import Array exposing (Array)
import Array.Extra
import Browser
import Browser.Events
import Color.Dracula as Dracula
import Composer.Converter as Converter
import Composer.Sequencer as Sequencer exposing (IncomingMsg(..), OutgoingMsg(..))
import Element as El
import Element.Background as UIBackground
import Element.Border as UIBorder
import Element.Font as UIFont
import Element.Input as UIInput
import Element.Region as UIRegion
import Extra.RemoteService as RemoteService
import FlowIO exposing (..)
import Html
import Images exposing (configGeneralIcon, configInflateParallelIcon, configInflateSeriesIcon, configVacuumParallelIcon, configVacuumSeriesIcon)
import Json.Decode exposing (Value, decodeValue)
import List.Extra as LE
import LocalStorage
import Scheduler exposing (IncomingMsg(..))
import Sensors
import Set exposing (Set)
import Styles exposing (borderWhite, bottomBorder, buttonCssIcon, colorToCssString, darkGrey, fullWidth, grey, inflateButton, palette, releaseButton, rightBorder, rust, stopButton, vacuumButton)
import Task
import Time


type alias Model =
    { devices : Array FlowIODevice
    , listeners : List { deviceIndex : Int, to : FlowIOService, shouldListen : Bool }
    , scheduler : Scheduler.Model
    , commandClicked : Maybe { deviceIndex : Int, action : FlowIOAction }
    , servicesPanel :
        { services : List ( Int, FlowIOService )
        , panelState : PanelState
        }
    , openTab : MainTab
    , sensorData : Sensors.Model
    , composerData : Converter.Model
    , windowSize : { width : Int, height : Int }
    , errorLog : List String
    , errorLogState : ErrorLogState
    , savedMenuState : PanelState
    , savedSchedules : Set String
    , sequencerData : Sequencer.Model
    }


type ErrorLogState
    = LogHasUnread Int
    | LogClosedAndRead
    | LogOpen


type PanelState
    = PanelFolded
    | PanelOpen


togglePanel : PanelState -> PanelState
togglePanel state =
    case state of
        PanelOpen ->
            PanelFolded

        PanelFolded ->
            PanelOpen


type MainTab
    = SchedulerTab
    | SensorReadingsTab
    | NotationConverterTab
    | SequencerTab


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
    | ActionClicked Int FlowIOAction
    | ActionReleased
    | SchedulerMessage Scheduler.Msg
    | DeviceConfigurationChanged { deviceIndex : Int, configuration : Maybe Configuration }
    | RequestDeviceConfiguration Int
    | SetDeviceConfiguration Int Configuration
    | ToggleServicePanelState
    | AddServiceToPanel Int FlowIOService
    | RemoveServiceFromPanel Int FlowIOService
    | DevicePowerOffStatusChange Int PowerOffStatus
    | SendNewPowerOffStatus Int PowerOffStatus
    | SensorReadingReceived Int (Result Json.Decode.Error AnalogReadings)
    | SensorReadingTimestampAttached Int ( Time.Posix, AnalogReadings )
    | SensorReadingModeChanged Int AnalogServiceRequest
    | SensorsMessage Sensors.Msg
    | ComposerMessage Converter.Msg
    | ChangeTabTo MainTab
    | WindowDimensionsChanged Int Int
    | NoAction String
    | ToggleErrorLog
    | ScheduleLoaded String String
    | ReceivedSavedSchedules (List String)
    | ToggleSavedMenu
    | SavedScheduleRequested String
    | SequencerMessage Sequencer.Msg
    | SendInstructionsToSequencerRequestedFromScheduler
    | SendInstructionsToSequencerRequestedFromConverter


initModel : { width : Int, height : Int } -> Model
initModel { width, height } =
    { devices = Array.empty
    , listeners = []
    , scheduler = Scheduler.initModel
    , commandClicked = Nothing
    , servicesPanel =
        { services = []
        , panelState = PanelOpen
        }
    , openTab = SchedulerTab
    , sensorData = Sensors.initialModel
    , composerData = Converter.init
    , windowSize = { width = width, height = height }
    , errorLog = []
    , errorLogState = LogClosedAndRead
    , savedMenuState = PanelFolded
    , savedSchedules = Set.empty
    , sequencerData = Sequencer.init
    }


sendMessage : msg -> Cmd msg
sendMessage msg =
    Task.perform (\() -> msg) <|
        Task.succeed ()


main : Program { width : Int, height : Int } Model Msg
main =
    Browser.document
        { init =
            \windowSize ->
                ( initModel windowSize
                , Cmd.batch
                    [ sendMessage AddDevice
                    , LocalStorage.getAllKeys ()
                    ]
                )
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

        localStorageListener : LocalStorage.StorageEvent -> Msg
        localStorageListener event =
            case event of
                LocalStorage.Loaded key value ->
                    ScheduleLoaded key (Debug.log "Loaded value" value)

                LocalStorage.ReceivedAllKeys strings ->
                    ReceivedSavedSchedules strings
    in
    Sub.batch
        ([ listenToDeviceStatus DeviceStatusChanged
         , Scheduler.subscriptions model.scheduler |> Sub.map SchedulerMessage
         , listenToDeviceConfiguration DeviceConfigurationChanged
         , listenToPowerOffStatus DevicePowerOffStatusChange
         , listenToAnalogReadings SensorReadingReceived
         , Browser.Events.onResize WindowDimensionsChanged
         , LocalStorage.listen localStorageListener
         ]
            ++ (if shouldListenToControlService then
                    [ listenToDeviceControlStatus ControlServiceUpdate ]

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

                ( sequencerModel, _ ) =
                    handleSequencerMessage <| Sequencer.send (DevicesChanged newDevices)
            in
            { model_ | devices = newDevices, scheduler = newScheduler, sequencerData = sequencerModel.sequencerData }

        updateDevice : Int -> (FlowIODevice -> FlowIODevice) -> Array FlowIODevice
        updateDevice index updater =
            Array.Extra.update index updater model.devices

        updateCommand deviceIndex newCommand =
            case Array.get deviceIndex model.devices |> Maybe.map .controlServiceStatus of
                Just _ ->
                    updateDevices (updateDevice deviceIndex (setLastCommand newCommand)) model

                Nothing ->
                    Debug.log ("Tried to update command to device " ++ String.fromInt deviceIndex ++ " that does not exist, or does not have a control service.") model

        removeDeviceServices deviceIndex =
            let
                servicesPanel =
                    model.servicesPanel
            in
            { servicesPanel | services = List.filter (\( index, _ ) -> index /= deviceIndex) servicesPanel.services }

        logError error =
            { model
                | errorLog = error :: model.errorLog
                , errorLogState =
                    case model.errorLogState of
                        LogHasUnread n ->
                            LogHasUnread (n + 1)

                        LogClosedAndRead ->
                            LogHasUnread 1

                        LogOpen ->
                            LogOpen
            }

        handleSchedulerUpdate messageToScheduler =
            let
                ( scheduler, effect, cmd ) =
                    Scheduler.update messageToScheduler model.scheduler

                ( effectedModel, effectedCmd ) =
                    case effect of
                        Scheduler.NoEffect ->
                            ( model, Cmd.none )

                        Scheduler.LogError string ->
                            ( logError string, Cmd.none )

                        Scheduler.AskSaveInstructions key value ->
                            ( { model | savedSchedules = Set.insert key model.savedSchedules }
                            , Cmd.batch
                                [ LocalStorage.save key value
                                , LocalStorage.getAllKeys ()
                                ]
                            )
            in
            ( { effectedModel | scheduler = scheduler }
            , Cmd.batch
                [ Cmd.map SchedulerMessage cmd
                , effectedCmd
                ]
            )

        handleSequencerMessage sequencerMsg =
            let
                ( newModel, effect, cmd ) =
                    Sequencer.update sequencerMsg model.sequencerData

                ( effectedModel, effectCommand ) =
                    case effect of
                        NoMessage ->
                            ( model, Cmd.none )

                        GetInstructionFromScheduler ->
                            ( model, sendMessage SendInstructionsToSequencerRequestedFromScheduler )

                        GetInstructionFromConverter ->
                            ( model, sendMessage SendInstructionsToSequencerRequestedFromConverter )

                        LogError error ->
                            ( logError error, Cmd.none )
            in
            ( { effectedModel | sequencerData = newModel }
            , Cmd.batch
                [ cmd |> Cmd.map SequencerMessage
                , effectCommand
                ]
            )
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
                                , servicesPanel = removeDeviceServices deviceIndex
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

        RemoveDevice _ ->
            ( logError "Removing devices is not supported at the moment", Cmd.none )

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
            handleSchedulerUpdate message

        ScheduleLoaded key value ->
            case Json.Decode.decodeString Scheduler.instructionsDecoder value of
                Ok newInstructions ->
                    handleSchedulerUpdate (Scheduler.send <| InstructionsLoaded key newInstructions)

                Err error ->
                    ( logError (Json.Decode.errorToString error), Cmd.none )

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
                            sendStopAll deviceIndex

                        Nothing ->
                            Cmd.none
            in
            ( { model | commandClicked = Nothing }, cmd )

        DeviceConfigurationChanged { deviceIndex, configuration } ->
            ( model |> updateDevices (updateDevice deviceIndex (setConfiguration configuration)), Cmd.none )

        RequestDeviceConfiguration deviceIndex ->
            ( model, queryDeviceConfiguration deviceIndex )

        SetDeviceConfiguration deviceIndex configuration ->
            ( model, sendDeviceConfiguration deviceIndex configuration )

        ToggleServicePanelState ->
            let
                servicesPanel =
                    model.servicesPanel

                newState =
                    case model.servicesPanel.panelState of
                        PanelFolded ->
                            PanelOpen

                        PanelOpen ->
                            PanelFolded
            in
            ( { model | servicesPanel = { servicesPanel | panelState = newState } }, Cmd.none )

        AddServiceToPanel deviceIndex flowIOService ->
            let
                servicesPanel =
                    model.servicesPanel
            in
            ( { model | servicesPanel = { servicesPanel | services = ( deviceIndex, flowIOService ) :: servicesPanel.services } }, Cmd.none )

        RemoveServiceFromPanel deviceIndex flowIOService ->
            let
                servicesPanel =
                    model.servicesPanel
            in
            ( { model
                | servicesPanel =
                    { servicesPanel
                        | services =
                            List.filter (\( i, s ) -> not (i == deviceIndex && s == flowIOService)) servicesPanel.services
                    }
              }
            , Cmd.none
            )

        DevicePowerOffStatusChange deviceIndex powerOffStatus ->
            ( model |> updateDevices (updateDevice deviceIndex (setPowerOffStatus <| Just powerOffStatus))
            , Cmd.none
            )

        SendNewPowerOffStatus deviceIndex powerOffStatus ->
            ( model, sendPowerOffStatus deviceIndex powerOffStatus )

        NoAction explanation ->
            ( Debug.log ("Received a no action Msg: " ++ explanation) model, Cmd.none )

        SensorReadingReceived deviceIndex result ->
            result
                |> Result.map
                    (\analogReadings ->
                        ( model
                        , Time.now
                            |> Task.perform
                                (\timestamp ->
                                    SensorReadingTimestampAttached deviceIndex
                                        ( timestamp
                                        , analogReadings
                                        )
                                )
                        )
                    )
                |> Result.withDefault ( model, Cmd.none )

        SensorReadingTimestampAttached deviceIndex ( timestamp, analogReadings ) ->
            let
                deviceId : Maybe DeviceId
                deviceId =
                    Array.get deviceIndex model.devices
                        |> Maybe.andThen .details
                        |> Maybe.map .id
            in
            case deviceId of
                Just id ->
                    let
                        ( sensorData, cmd ) =
                            Sensors.update model.sensorData (Sensors.NewReading id timestamp analogReadings)
                    in
                    ( { model
                        | sensorData = sensorData
                      }
                        |> updateDevices
                            (updateDevice deviceIndex
                                (setNewAnalogServiceReadings timestamp analogReadings)
                            )
                    , cmd
                        |> Cmd.map
                            SensorsMessage
                    )

                Nothing ->
                    ( model, Cmd.none )

        SensorsMessage message ->
            let
                ( sensorData, cmd ) =
                    Sensors.update model.sensorData message
            in
            ( { model | sensorData = sensorData }, cmd |> Cmd.map SensorsMessage )

        ChangeTabTo mainTab ->
            ( { model | openTab = mainTab }, Cmd.none )

        WindowDimensionsChanged width height ->
            ( { model | windowSize = { width = width, height = height } }, Cmd.none )

        SensorReadingModeChanged deviceIndex analogServiceRequest ->
            ( model |> updateDevices (updateDevice deviceIndex (setNewAnalogReadRequest analogServiceRequest))
            , requestAnalogReadings deviceIndex analogServiceRequest
            )

        ComposerMessage composerMsg ->
            let
                ( composerData, cmd ) =
                    Converter.update composerMsg model.composerData
            in
            ( { model
                | composerData = composerData
              }
            , cmd |> Cmd.map ComposerMessage
            )

        ToggleErrorLog ->
            ( { model
                | errorLogState =
                    case model.errorLogState of
                        LogHasUnread _ ->
                            LogOpen

                        LogClosedAndRead ->
                            LogOpen

                        LogOpen ->
                            LogClosedAndRead
              }
            , Cmd.none
            )

        ReceivedSavedSchedules strings ->
            ( { model | savedSchedules = Set.fromList strings }, Cmd.none )

        ToggleSavedMenu ->
            ( { model | savedMenuState = togglePanel model.savedMenuState }, Cmd.none )

        SavedScheduleRequested key ->
            ( model, LocalStorage.load key )

        SequencerMessage sequencerMsg ->
            handleSequencerMessage sequencerMsg

        SendInstructionsToSequencerRequestedFromScheduler ->
            handleSequencerMessage <|
                Sequencer.send
                    (ReceivedNewPart
                        model.scheduler.scheduleName
                        model.scheduler.instructions
                    )

        SendInstructionsToSequencerRequestedFromConverter ->
            handleSequencerMessage <|
                Sequencer.send
                    (ReceivedNewPart
                        model.scheduler.scheduleName
                        model.scheduler.instructions
                    )


view : Model -> Browser.Document Msg
view model =
    { title = "FlowIO"
    , body = [ body model ]
    }


body : Model -> Html.Html Msg
body model =
    El.layout
        [ El.width <| El.px model.windowSize.width
        , El.height <| El.px model.windowSize.height
        , El.padding 20
        , UIFont.color Dracula.white
        , UIFont.family [ UIFont.typeface "Overpass", UIFont.typeface "Open Sans", UIFont.typeface "Helvetica", UIFont.sansSerif ]
        , Styles.fontSize.standard
        , UIBackground.color Dracula.black
        ]
    <|
        El.column [ El.width <| El.fill, El.height <| El.fill, El.spacing 10 ]
            [ header model
            , El.row [ El.spacing 10, El.width El.fill, El.height <| El.fillPortion 10, El.alignTop ]
                [ displayDeviceList model
                , displayServices model
                , tabs model
                ]
            , footer model
            ]


tabs : Model -> El.Element Msg
tabs { scheduler, sensorData, composerData, openTab, windowSize, servicesPanel, sequencerData } =
    let
        tabSize =
            if servicesPanel.panelState == PanelFolded then
                { height = toFloat windowSize.height * 0.85 |> round
                , width = toFloat windowSize.width * (8 / 10) - 80 |> round
                }

            else
                { height = toFloat windowSize.height * 0.85 |> round
                , width = toFloat windowSize.width * (8 / 12) - 80 |> round
                }

        tabStyle selected =
            [ UIBorder.roundEach { bottomRight = 0, topRight = 4, bottomLeft = 0, topLeft = 4 }
            , UIBorder.widthEach { bottom = 0, left = 2, right = 2, top = 2 }
            , El.paddingXY 8 6
            , UIBorder.color palette.onBackground
            ]
                ++ (if selected then
                        [ UIBackground.color palette.primary, UIFont.color palette.onPrimary ]

                    else
                        [ UIBackground.color palette.background, UIFont.color palette.onBackground ]
                   )
    in
    El.column [ El.spacing 4, tabSize.width |> El.px |> El.width, El.alignTop, tabSize.height |> El.px |> El.height ]
        [ El.row [ bottomBorder, El.paddingXY 12 0, El.alignLeft, fullWidth ]
            [ UIInput.button (tabStyle (openTab == SchedulerTab))
                { label =
                    El.row [ El.spacing 4 ]
                        [ Images.schedulerIcon
                        , El.text "Scheduler"
                        ]
                , onPress = Just <| ChangeTabTo SchedulerTab
                }
            , UIInput.button (tabStyle (openTab == SensorReadingsTab))
                { label =
                    El.row [ El.spacing 4 ]
                        [ Images.sensorsIcon, El.text "Sensors" ]
                , onPress = Just <| ChangeTabTo SensorReadingsTab
                }
            , UIInput.button (tabStyle (openTab == NotationConverterTab))
                { label =
                    El.row [ El.spacing 4 ] [ Images.converterIcon, El.text "Convert Score" ]
                , onPress = Just <| ChangeTabTo NotationConverterTab
                }
            , UIInput.button (tabStyle (openTab == SequencerTab))
                { label =
                    El.row [ El.spacing 4 ] [ Images.sequencerIcon, El.text "Sequencer" ]
                , onPress = Just <| ChangeTabTo SequencerTab
                }
            ]
        , case openTab of
            SchedulerTab ->
                El.map SchedulerMessage <|
                    Scheduler.view scheduler

            SensorReadingsTab ->
                El.map SensorsMessage <| Sensors.view sensorData

            NotationConverterTab ->
                El.map ComposerMessage <| Converter.view composerData

            SequencerTab ->
                El.map SequencerMessage <| Sequencer.view sequencerData
        ]


displayDeviceList : Model -> El.Element Msg
displayDeviceList model =
    let
        showDevice : Int -> FlowIODevice -> El.Element Msg
        showDevice index device =
            El.row [ bottomBorder, El.width El.fill, El.spacing 5, El.paddingXY 0 4 ]
                [ El.el [] <| El.text (String.fromInt (index + 1) ++ ": ")
                , case device.status of
                    NotConnected ->
                        El.row [ fullWidth ]
                            [ UIInput.button [ El.alignRight, UIRegion.description "Connect" ]
                                { label = buttonCssIcon "icon-disconnected" "Disconnected", onPress = Just <| ConnectToDevice index }
                            , if index > 0 then
                                UIInput.button [ El.alignLeft, UIRegion.description "Remove", UIFont.heavy ]
                                    { label = El.text "-", onPress = Just <| RemoveDevice index }

                              else
                                El.none
                            ]

                    Pending ->
                        El.el [ fullWidth, UIRegion.description "waiting connection" ] <|
                            buttonCssIcon
                                "icon-loading"
                                "Awaiting connection"

                    Connected ->
                        let
                            serviceButton service =
                                let
                                    serviceShown =
                                        model.servicesPanel.services |> List.member ( index, service )

                                    color =
                                        if serviceShown then
                                            Dracula.green

                                        else
                                            Dracula.white

                                    backgroundColor =
                                        if serviceShown then
                                            Dracula.white

                                        else
                                            Dracula.gray
                                in
                                UIInput.button
                                    [ Styles.fontSize.small
                                    , UIFont.color color
                                    , UIBackground.color backgroundColor
                                    , UIBorder.rounded 4
                                    , El.padding 4
                                    ]
                                    { onPress =
                                        Just <|
                                            if serviceShown then
                                                RemoveServiceFromPanel index service

                                            else
                                                AddServiceToPanel index service
                                    , label = El.text <| serviceToPrettyName service
                                    }
                        in
                        El.column [ El.spacing 5 ]
                            [ El.row [ El.width El.fill ]
                                [ El.text <| Maybe.withDefault "Unknown" <| Maybe.map .name device.details
                                , UIInput.button [ UIRegion.description "Disconnect", El.alignLeft ]
                                    { label = buttonCssIcon "icon-connected" "Connected", onPress = Just <| DisconnectDevice index }
                                ]
                            , El.paragraph [ Styles.fontSize.smaller, El.width El.fill ]
                                [ El.text "id: "
                                , El.text <| Maybe.withDefault "Unknown" <| Maybe.map .id device.details
                                ]
                            , case device.details |> Maybe.map .services of
                                Just services ->
                                    El.wrappedRow [ El.spacing 4 ] <|
                                        List.map serviceButton services

                                Nothing ->
                                    El.el [] <| El.text "No services"

                            --device.details
                            --    |> Maybe.map .services
                            --    |> Maybe.withDefault []
                            --    |>
                            --El.wrappedRow [El.spacing 4]
                            ]
                ]

        listHeader =
            El.el [ El.width El.fill ] <| El.text "Devices"

        buttons =
            El.row [ El.width El.fill ]
                [ UIInput.button []
                    { label = El.el [ UIRegion.description "Add Device", UIFont.heavy ] <| El.text "+"
                    , onPress = Just AddDevice
                    }
                ]
    in
    El.column
        [ El.alignTop
        , El.width <| El.fillPortion 2
        , El.spacing 5
        , borderWhite
        , rightBorder
        , El.padding 5
        , El.height El.fill
        ]
        (listHeader
            :: (Array.indexedMap
                    showDevice
                    model.devices
                    |> Array.toList
               )
            ++ [ buttons ]
        )


type PowerOffServiceSelectOptions
    = TurnOff
    | DisableTimer
    | SetTimer
    | UnsupportedOption


type AnalogServiceCommands
    = SingleRead
    | StopRead
    | ContinuousRead


displayServices : Model -> El.Element Msg
displayServices { devices, servicesPanel } =
    let
        serviceWrapper : Int -> FlowIODevice -> FlowIOService -> El.Element Msg -> El.Element Msg
        serviceWrapper index device service content =
            let
                serviceTitle =
                    El.column [ El.spacing 4 ]
                        [ device.details
                            |> Maybe.map .name
                            |> Maybe.withDefault "Unknown"
                            |> (++) (String.fromInt (index + 1) ++ ": ")
                            |> El.text
                        , El.text <| serviceToString service
                        ]

                serviceButtons =
                    El.row [ El.alignRight ]
                        [ UIInput.button [ UIRegion.description ("Close service " ++ serviceToString service) ]
                            { onPress = Just <| RemoveServiceFromPanel index service
                            , label = El.text "✕"
                            }
                        ]
            in
            El.column [ fullWidth, bottomBorder, borderWhite, El.spacing 8 ]
                [ El.row [ fullWidth, bottomBorder, UIBorder.color Dracula.gray ] [ serviceTitle, serviceButtons ]
                , content
                ]

        displayControlService : Int -> FlowIODevice -> El.Element Msg
        displayControlService index device =
            case ( device.status, device.controlServiceStatus ) of
                ( Connected, Just hardwareStatus ) ->
                    El.column [ El.width El.fill, Styles.fontSize.large ]
                        [ El.text
                            ("Status for "
                                ++ String.fromInt (index + 1)
                                ++ ": "
                                ++ (Maybe.map .name device.details |> Maybe.withDefault "")
                            )
                        , displayStatusDetailsDetails index hardwareStatus
                        , displayControls index hardwareStatus.command
                        ]

                _ ->
                    El.none

        displayControls : Int -> FlowIOCommand -> El.Element Msg
        displayControls deviceIndex command =
            El.wrappedRow [ El.width El.fill ]
                [ pwmSlider (ChangeCommandPwm deviceIndex) command.pumpPwm
                , actions (ActionClicked deviceIndex) command.action
                , displayPorts (ChangeCommandPortState deviceIndex) command.ports
                ]

        pwmSlider : (Int -> Msg) -> Int -> El.Element Msg
        pwmSlider onUpdate currentValue =
            let
                fullWidth =
                    160

                filled =
                    toFloat currentValue / 255
            in
            UIInput.slider
                [ El.width <| El.px fullWidth
                , El.height <| El.px 12
                , El.behindContent <|
                    El.el
                        [ El.width El.fill
                        , El.height <| El.px 8
                        , El.padding 2
                        , El.centerX
                        , El.centerY
                        , UIBackground.color grey
                        , UIBorder.rounded 4
                        ]
                        El.none
                , El.behindContent <|
                    El.el
                        [ El.width <| El.px <| round <| (fullWidth - 4) * filled
                        , El.height <| El.px 4
                        , El.spacing 2
                        , El.alignLeft
                        , El.centerY
                        , UIBackground.color rust
                        , UIBorder.rounded 2
                        ]
                        El.none
                ]
                { label = UIInput.labelAbove [ Styles.fontSize.small, UIFont.center ] <| El.text "PWM"
                , onChange = round >> onUpdate
                , max = 255
                , min = 0
                , step = Just 1
                , thumb = UIInput.defaultThumb
                , value = filled
                }

        actions : (FlowIOAction -> Msg) -> FlowIOAction -> El.Element Msg
        actions onMouseDown _ =
            El.row [ El.spacing 5, El.padding 5 ]
                [ inflateButton (onMouseDown Inflate) ActionReleased
                , vacuumButton (onMouseDown Vacuum) ActionReleased
                , releaseButton (onMouseDown Release) ActionReleased
                , stopButton (onMouseDown Stop) ActionReleased
                ]

        displayPorts : (Port -> PortState -> Msg) -> PortsState -> El.Element Msg
        displayPorts onUpdate ports =
            let
                checkbox label port_ currentValue =
                    UIInput.checkbox []
                        { onChange = portFromBool >> onUpdate port_
                        , label = UIInput.labelAbove [ Styles.fontSize.small, UIFont.center ] <| El.text label
                        , icon = UIInput.defaultCheckbox
                        , checked = isPortOpen currentValue
                        }
            in
            El.row [ El.spacing 5, El.padding 5 ]
                [ checkbox "1" Port1 ports.port1
                , checkbox "2" Port2 ports.port2
                , checkbox "3" Port3 ports.port3
                , checkbox "4" Port4 ports.port4
                , checkbox "5" Port5 ports.port5
                ]

        displayStatusDetailsDetails : Int -> ControlServiceStatus -> El.Element Msg
        displayStatusDetailsDetails _ details =
            let
                displayPort label status =
                    El.el
                        [ El.width <| El.px 24
                        , El.height <| El.px 24
                        , UIBorder.width 2
                        , UIBorder.color darkGrey
                        , El.padding 2
                        , UIBackground.color
                            (if status then
                                rust

                             else
                                Dracula.white
                            )
                        , Styles.fontSize.smaller
                        , UIFont.color
                            (if status then
                                Dracula.white

                             else
                                darkGrey
                            )
                        , UIFont.center
                        ]
                    <|
                        El.el [ El.centerY, El.centerX ] <|
                            El.text label
            in
            El.column [ El.spacing 2, El.width El.fill ]
                [ if details.active then
                    El.text "Active"

                  else
                    El.text "Inactive"
                , El.row [ El.spacing 2, El.width El.fill, El.centerX ]
                    [ displayPort "In" details.inlet
                    , displayPort "1" details.port1
                    , displayPort "2" details.port2
                    , displayPort "3" details.port3
                    , displayPort "4" details.port4
                    , displayPort "5" details.port5
                    , displayPort "Out" details.outlet
                    ]
                , El.row [ El.spaceEvenly, El.width El.fill ]
                    [ El.text
                        ("Pump 1: "
                            ++ (if details.pump1 then
                                    "On"

                                else
                                    "Off"
                               )
                        )
                    , El.text
                        ("Pump 2: "
                            ++ (if details.pump2 then
                                    "On"

                                else
                                    "Off"
                               )
                        )
                    ]
                ]

        displayConfigService : Int -> FlowIODevice -> El.Element Msg
        displayConfigService index device =
            let
                configIcon : (String -> Html.Html msg) -> UIInput.OptionState -> El.Element msg
                configIcon baseIcon state =
                    let
                        greenString =
                            Dracula.green |> colorToCssString

                        whiteString =
                            Dracula.white |> colorToCssString

                        attributes =
                            [ El.width <| El.px 32
                            , El.height <| El.px 32
                            , UIBorder.rounded 4
                            ]
                    in
                    case state of
                        UIInput.Idle ->
                            El.el (attributes ++ [ UIBackground.color Dracula.gray ])
                                (El.html <| baseIcon whiteString)

                        UIInput.Focused ->
                            El.el (attributes ++ [ UIBorder.glow Dracula.cyan 3 ]) (El.html <| baseIcon whiteString)

                        UIInput.Selected ->
                            El.el (attributes ++ [ UIBackground.color Dracula.white ])
                                (El.html <| baseIcon greenString)
            in
            UIInput.radioRow [ fullWidth, El.spaceEvenly, El.padding 4 ]
                { onChange = \option -> SetDeviceConfiguration index option
                , label = UIInput.labelAbove [] <| El.text "Device Configuration"
                , selected = device.configuration
                , options =
                    [ UIInput.optionWith StandardConfiguration (configIcon configGeneralIcon)
                    , UIInput.optionWith InflationSeries (configIcon configInflateSeriesIcon)
                    , UIInput.optionWith InflationParallel (configIcon configInflateParallelIcon)
                    , UIInput.optionWith VacuumSeries (configIcon configVacuumSeriesIcon)
                    , UIInput.optionWith VacuumParallel (configIcon configVacuumParallelIcon)
                    ]
                }

        displayBatteryService : Int -> FlowIODevice -> El.Element Msg
        displayBatteryService _ _ =
            El.none

        displayPowerOffService : Int -> FlowIODevice -> El.Element Msg
        displayPowerOffService index device =
            let
                statusToOption =
                    case device.powerOffServiceStatus of
                        Just DeviceOff ->
                            Just TurnOff

                        Just PowerOffTimerDisabled ->
                            Just DisableTimer

                        Just (PowerOffMinutesRemaining _) ->
                            Just SetTimer

                        _ ->
                            Nothing

                minutesRemaining =
                    device.powerOffServiceStatus
                        |> Maybe.andThen
                            (\status ->
                                case status of
                                    PowerOffMinutesRemaining min ->
                                        Just min

                                    _ ->
                                        Nothing
                            )

                onChange option =
                    case option of
                        TurnOff ->
                            SendNewPowerOffStatus index DeviceOff

                        DisableTimer ->
                            SendNewPowerOffStatus index PowerOffTimerDisabled

                        SetTimer ->
                            SendNewPowerOffStatus index
                                (minutesRemaining
                                    |> Maybe.map PowerOffMinutesRemaining
                                    |> Maybe.withDefault (PowerOffMinutesRemaining 5)
                                )

                        UnsupportedOption ->
                            NoAction "Selected unsupported action"

                selector =
                    let
                        minutes =
                            minutesRemaining |> Maybe.map toFloat |> Maybe.withDefault 0
                    in
                    UIInput.radioRow [ El.spaceEvenly, fullWidth ]
                        { onChange = onChange
                        , label = UIInput.labelAbove [] <| El.text "Power-off Status"
                        , selected = statusToOption
                        , options =
                            [ UIInput.optionWith TurnOff <| Styles.option <| El.text "Off"
                            , UIInput.optionWith DisableTimer <| Styles.option <| El.text "Disabled"
                            , UIInput.optionWith SetTimer <|
                                Styles.optionWithSlider <|
                                    { label = UIInput.labelRight []
                                    , labelContent = El.text (String.fromFloat minutes ++ " minutes")
                                    , onChange =
                                        \value ->
                                            SendNewPowerOffStatus index
                                                (PowerOffMinutesRemaining <| round value)
                                    , value = minutes
                                    , min = 1
                                    , max = 30
                                    , step = Just 1
                                    , thumb = UIInput.defaultThumb
                                    , width = El.px 80
                                    }
                            ]
                        }
            in
            selector

        displayAnalogService : Int -> FlowIODevice -> El.Element Msg
        displayAnalogService deviceIndex device =
            let
                currentSelectedCommand : Maybe AnalogServiceCommands
                currentSelectedCommand =
                    device.analogSensorsService
                        |> RemoteService.getCommand
                        |> Maybe.map
                            (\request ->
                                case request of
                                    RequestStopAnalog ->
                                        StopRead

                                    RequestSingleAnalogRead ->
                                        SingleRead

                                    RequestContinuousAnalog _ ->
                                        ContinuousRead
                            )

                onChange : AnalogServiceCommands -> Msg
                onChange command =
                    SensorReadingModeChanged deviceIndex <|
                        case command of
                            SingleRead ->
                                RequestSingleAnalogRead

                            StopRead ->
                                RequestStopAnalog

                            ContinuousRead ->
                                case device.analogSensorsService |> RemoteService.getCommand of
                                    Just (RequestContinuousAnalog samples) ->
                                        RequestContinuousAnalog samples

                                    _ ->
                                        RequestContinuousAnalog 1

                sampleWindowSize =
                    device.analogSensorsService
                        |> RemoteService.getCommand
                        |> (\cmd ->
                                case cmd of
                                    Just (RequestContinuousAnalog samples) ->
                                        samples

                                    _ ->
                                        1
                           )

                commands : El.Element Msg
                commands =
                    UIInput.radioRow [ fullWidth ]
                        { label = UIInput.labelAbove [] <| El.text "Read Mode:"
                        , onChange = onChange
                        , selected = currentSelectedCommand
                        , options =
                            [ UIInput.optionWith StopRead <| Styles.option <| El.text "Stop"
                            , UIInput.optionWith SingleRead <| Styles.option <| El.text "Single"
                            , UIInput.optionWith ContinuousRead <|
                                Styles.optionWithSlider
                                    { onChange =
                                        \val ->
                                            SensorReadingModeChanged deviceIndex (RequestContinuousAnalog <| round val)
                                    , label = UIInput.labelRight []
                                    , labelContent = El.text ("Continuous (" ++ String.fromInt sampleWindowSize ++ " sample avg.)")
                                    , width = El.px 80
                                    , min = 1
                                    , max = 10
                                    , step = Just 1
                                    , value = toFloat sampleWindowSize
                                    , thumb = UIInput.defaultThumb
                                    }
                            ]
                        }
            in
            El.column [ fullWidth ]
                ([ commands ]
                    ++ (case device.analogSensorsService |> RemoteService.getData of
                            Just { lastReading, readingsTimestamp } ->
                                [ El.el
                                    [ fullWidth
                                    , El.height <| El.px 200
                                    , El.paddingEach { left = 30, top = 16, bottom = 16, right = 0 }
                                    ]
                                  <|
                                    (Sensors.barChart { width = 300, height = 200 } readingsTimestamp lastReading
                                        |> El.map SensorsMessage
                                    )
                                , El.paragraph [] <|
                                    [ El.text "Received: "
                                    , El.text (Time.toHour Time.utc readingsTimestamp |> String.fromInt)
                                    , El.text ":"
                                    , El.text (Time.toMinute Time.utc readingsTimestamp |> String.fromInt)
                                    , El.text ":"
                                    , El.text (Time.toSecond Time.utc readingsTimestamp |> String.fromInt)
                                    , El.text "."
                                    , El.text (Time.toMillis Time.utc readingsTimestamp |> String.fromInt)
                                    ]
                                ]

                            Nothing ->
                                []
                       )
                )

        listHeader =
            case servicesPanel.panelState of
                PanelFolded ->
                    El.column [ fullWidth ]
                        [ UIInput.button [ El.alignRight, El.paddingXY 5 0, UIRegion.description "Open Service Panel" ]
                            { onPress = Just ToggleServicePanelState
                            , label = El.el [] <| El.text "▶︎"
                            }
                        , El.el [ El.width <| El.shrink, El.moveDown 40, El.moveLeft 10 ] <|
                            El.el [ El.rotate <| Basics.degrees 90 ] <|
                                El.text "Services"
                        ]

                PanelOpen ->
                    El.row [ El.width El.fill ]
                        [ El.el [ El.centerX, UIFont.underline ] <| El.text "Services"
                        , UIInput.button [ El.alignRight ]
                            { label = El.el [ El.spacing 5 ] <| El.text "◀︎"
                            , onPress = Just ToggleServicePanelState
                            }
                        ]

        displayServicePanel : Int -> FlowIOService -> El.Element Msg
        displayServicePanel deviceIndex service =
            let
                maybeDevice =
                    devices |> Array.get deviceIndex

                hasService =
                    maybeDevice
                        |> Maybe.andThen .details
                        |> Maybe.map .services
                        |> Maybe.map (List.member service)
                        |> Maybe.withDefault False
            in
            case ( maybeDevice, hasService ) of
                ( Nothing, _ ) ->
                    El.none

                ( _, False ) ->
                    El.none

                ( Just device, True ) ->
                    serviceWrapper deviceIndex device service <|
                        case service of
                            ControlService ->
                                displayControlService deviceIndex device

                            ConfigService ->
                                displayConfigService deviceIndex device

                            BatteryService ->
                                displayBatteryService deviceIndex device

                            UnknownService _ ->
                                El.none

                            PowerOffService ->
                                displayPowerOffService deviceIndex device

                            AnalogService ->
                                displayAnalogService deviceIndex device
    in
    El.column
        [ El.alignTop
        , rightBorder
        , borderWhite
        , if servicesPanel.panelState == PanelOpen then
            El.width <| El.fillPortion 2

          else
            El.width <| El.maximum 40 El.shrink
        , El.spacing 5
        , El.padding 5
        , El.height El.fill
        ]
        (listHeader
            :: (if servicesPanel.panelState == PanelOpen then
                    servicesPanel.services |> List.map (\( index, service ) -> displayServicePanel index service)

                else
                    []
               )
        )


header : Model -> El.Element Msg
header { savedMenuState, savedSchedules } =
    let
        savedMenu =
            case savedMenuState of
                PanelFolded ->
                    El.none

                PanelOpen ->
                    Set.toList savedSchedules
                        |> List.filter (not << String.isEmpty)
                        |> List.map savedItem
                        |> El.column
                            (Styles.card
                                ++ Styles.colorsNormal
                                ++ [ Styles.elevatedShadow ]
                            )

        savedItem key =
            UIInput.button Styles.button
                { onPress = Just <| SavedScheduleRequested key
                , label = El.text key
                }
    in
    El.row [ fullWidth, El.centerX, El.alignTop ]
        [ El.el [ El.centerX, El.alignTop, UIFont.color Dracula.white, Styles.fontSize.huge ] <|
            El.text "SymbioSing Control Panel"
        , El.el [ El.alignRight, El.below savedMenu ] <|
            UIInput.button (Styles.button ++ [ El.alignRight ])
                { onPress = Just ToggleSavedMenu
                , label =
                    case savedMenuState of
                        PanelFolded ->
                            El.text "Saved Schedules ◀️"

                        PanelOpen ->
                            El.text "Saved Schedules ⏏️"
                }
        ]


footer : Model -> El.Element Msg
footer { errorLog, errorLogState } =
    let
        errors =
            case errorLogState of
                LogOpen ->
                    El.column
                        [ UIBackground.color Styles.palette.background
                        , UIBorder.color Styles.palette.onBackground
                        , UIBorder.width 1
                        , Styles.elevatedShadow
                        , El.width <| El.px 440
                        , El.padding 4
                        ]
                        (El.row [ fullWidth ]
                            [ UIInput.button [ El.alignRight ]
                                { onPress = Just ToggleErrorLog
                                , label = El.text "X"
                                }
                            ]
                            :: List.map (El.el [ fullWidth ] << El.text) errorLog
                        )

                _ ->
                    El.none

        errorCount =
            case List.length errorLog of
                0 ->
                    [ El.text "No Errors" ]

                _ ->
                    [ El.el [] <| El.text <| String.fromInt <| List.length errorLog
                    , El.text " Errors"
                    , case errorLogState of
                        LogHasUnread n ->
                            El.el
                                [ UIBackground.color Dracula.red
                                , UIFont.color Dracula.white
                                , UIBorder.rounded 4
                                ]
                            <|
                                El.text (String.fromInt n ++ " new")

                        _ ->
                            El.none
                    ]
    in
    El.row [ El.alignBottom, El.height <| El.px 24 ]
        [ El.el [ El.alignLeft, El.above errors ] <|
            UIInput.button [] { onPress = Just ToggleErrorLog, label = El.row [] errorCount }
        ]
