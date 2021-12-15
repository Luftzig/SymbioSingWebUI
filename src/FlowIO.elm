port module FlowIO exposing
    ( AnalogReadings
    , AnalogService
    , AnalogServiceRequest(..)
    , Configuration(..)
    , ControlServiceStatus
    , DeviceDetails
    , DeviceId
    , FlowIOAction(..)
    , FlowIOCommand
    , FlowIODevice
    , FlowIOService(..)
    , FlowIOStatus(..)
    , PidControl
    , PidGoals
    , PidSettings
    , Port(..)
    , PortState(..)
    , PortsState
    , PowerOffStatus(..)
    , commandActionDecoder
    , connectToDevice
    , controlCommandDecoder
    , controlServiceStatusDecoder
    , createDevice
    , defaultCommand
    , defaultDevice
    , deviceDetailsDecoder
    , disconnectDevice
    , encodeCommand
    , encodePidSettings
    , getLastCommand
    , isPortOpen
    , listenToAnalogReadings
    , listenToControlService
    , listenToDeviceConfiguration
    , listenToDeviceControlStatus
    , listenToDeviceStatus
    , listenToPidSettings
    , listenToPowerOffStatus
    , portFromBool
    , queryDeviceConfiguration
    , queryPidSettings
    , queryPowerOffStatus
    , requestAnalogReadings
    , sendCommand
    , sendDeviceConfiguration
    , sendPidSettings
    , sendPowerOffStatus
    , sendStopAll
    , sensorsResolution
    , serviceFromString
    , serviceToPrettyName
    , serviceToString
    , setAction
    , setAnalogServiceData
    , setConfiguration
    , setControlServiceStatusTo
    , setDetailsTo
    , setIncomingPidSettings
    , setLastCommand
    , setNewAnalogReadRequest
    , setNewAnalogServiceReadings
    , setOutgoingPidSettings
    , setPort
    , setPowerOffStatus
    , setPumpPwm
    , setStatusTo
    , updateCommandFromStatus
    , setOutgoingPidGoals, sendPidGoals)

import Array exposing (Array)
import Bitwise
import Json.Decode as JD
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as JE
import RemoteService exposing (Service, getData, mapCommand, updateCommand, updateData)
import Time



--{{ Communicating with FlowIO devices }}


port createDevice : () -> Cmd msg


port connectToDevice : Int -> Cmd msg


port disconnectDevice : Int -> Cmd msg


port listenToDeviceStatus : ({ deviceIndex : Int, status : String, details : Maybe JE.Value } -> msg) -> Sub msg


port listenToDeviceControlStatus : ({ deviceIndex : Int, status : JE.Value } -> msg) -> Sub msg


port listenToControlService : Int -> Cmd msg


port sendCommand : { deviceIndex : Int, command : JE.Value } -> Cmd msg


port sendStopAll : Int -> Cmd msg



-- Configuration service ports


port queryDeviceConfiguration : Int -> Cmd msg


port listenToDeviceConfiguration_ : ({ deviceIndex : Int, configuration : String } -> msg) -> Sub msg


listenToDeviceConfiguration : ({ deviceIndex : Int, configuration : Maybe Configuration } -> msg) -> Sub msg
listenToDeviceConfiguration createMessage =
    listenToDeviceConfiguration_
        (\{ deviceIndex, configuration } ->
            createMessage
                { deviceIndex = deviceIndex
                , configuration = configurationDecoding configuration
                }
        )


port sendDeviceConfiguration_ : { deviceIndex : Int, configuration : String } -> Cmd msg


sendDeviceConfiguration : Int -> Configuration -> Cmd msg
sendDeviceConfiguration index configuration =
    sendDeviceConfiguration_ { deviceIndex = index, configuration = configurationEncoding configuration }



-- Power off service ports


port listenToPowerOffStatus_ : ({ deviceIndex : Int, status : JE.Value } -> msg) -> Sub msg


listenToPowerOffStatus : (Int -> PowerOffStatus -> msg) -> Sub msg
listenToPowerOffStatus toMessage =
    listenToPowerOffStatus_
        (\{ deviceIndex, status } ->
            case JD.decodeValue powerOffStatusDecoder status of
                Ok resolvedStatus ->
                    toMessage deviceIndex resolvedStatus

                Err error ->
                    toMessage deviceIndex PowerOffStatusUnknown
        )


port queryPowerOffStatus : Int -> Cmd msg


port sendPowerOffStatus_ : { deviceIndex : Int, status : JE.Value } -> Cmd msg


sendPowerOffStatus : Int -> PowerOffStatus -> Cmd msg
sendPowerOffStatus index status =
    sendPowerOffStatus_ { deviceIndex = index, status = encodePowerOffStatus status }



-- PID Service Ports


port queryPidSettings : Int -> Cmd msg


port listenToPidSettings_ : ({ deviceIndex : Int, settings : JD.Value } -> msg) -> Sub msg


listenToPidSettings : (Int -> Result JD.Error PidSettings -> msg) -> Sub msg
listenToPidSettings toMsg =
    listenToPidSettings_
        (\{ deviceIndex, settings } ->
            toMsg deviceIndex <|
                JD.decodeValue pidSettingsDecoder settings
        )


port sendPidSettings_ : { deviceIndex : Int, settings : JE.Value } -> Cmd msg


sendPidSettings : Int -> PidSettings -> Cmd msg
sendPidSettings deviceIndex settings =
    sendPidSettings_ { deviceIndex = deviceIndex, settings = encodePidSettings settings }


port sendPidGoals : { deviceIndex : Int, goals : PidGoals } -> Cmd msg



-- FlowIODevice types
{- TODO: Should I wrap all the service in a type to represent their status?
   possible values are probably: not supported, expecting update, updated? We also have local value versus remote one.
-}


type alias FlowIODevice =
    { status : FlowIOStatus
    , details : Maybe DeviceDetails
    , controlServiceStatus : Maybe ControlServiceStatus
    , powerOffServiceStatus : Maybe PowerOffStatus
    , configuration : Maybe Configuration
    , analogSensorsService : AnalogService
    , pidService : RemoteService.Service PidSettings { settings : PidSettings, goals : PidGoals }
    }


type alias DeviceId =
    String


type alias DeviceDetails =
    { name : String, id : DeviceId, services : List FlowIOService }


type FlowIOStatus
    = NotConnected
    | Pending
    | Connected


type FlowIOService
    = ControlService
    | ConfigService
    | BatteryService
    | PowerOffService
    | AnalogService
    | PidService
    | UnknownService String


type PowerOffStatus
    = DeviceOff
    | PowerOffTimerDisabled
    | PowerOffMinutesRemaining Int
    | PowerOffStatusUnknown


serviceToString : FlowIOService -> String
serviceToString service =
    case service of
        ControlService ->
            "control-service"

        ConfigService ->
            "config-service"

        UnknownService details ->
            "unknown-service:" ++ details

        BatteryService ->
            "battery-service"

        PowerOffService ->
            "power-off-service"

        AnalogService ->
            "analog-service"

        PidService ->
            "pid-service"


serviceToPrettyName : FlowIOService -> String
serviceToPrettyName service =
    case service of
        ControlService ->
            "Control"

        ConfigService ->
            "Configuration"

        UnknownService details ->
            "Unknown"

        BatteryService ->
            "Battery"

        PowerOffService ->
            "Shutdown Timer"

        AnalogService ->
            "Sensors"

        PidService ->
            "PID"


serviceFromString : String -> FlowIOService
serviceFromString string =
    case string of
        "control-service" ->
            ControlService

        "config-service" ->
            ConfigService

        "battery-service" ->
            BatteryService

        "power-off-service" ->
            PowerOffService

        "analog-service" ->
            AnalogService

        "pid-service" ->
            PidService

        other ->
            UnknownService other


type alias ControlServiceStatus =
    { pump1 : Bool
    , pump2 : Bool
    , inlet : Bool
    , outlet : Bool
    , port1 : Bool
    , port2 : Bool
    , port3 : Bool
    , port4 : Bool
    , port5 : Bool
    , active : Bool
    , command : FlowIOCommand
    }


type Configuration
    = StandardConfiguration
    | InflationSeries
    | InflationParallel
    | VacuumSeries
    | VacuumParallel



-- FlowIODevice setters


defaultDevice : FlowIODevice
defaultDevice =
    { status = NotConnected
    , details = Nothing
    , controlServiceStatus = Nothing
    , configuration = Nothing
    , powerOffServiceStatus = Nothing
    , analogSensorsService = RemoteService.init
    , pidService = RemoteService.init
    }


setStatusTo : FlowIOStatus -> FlowIODevice -> FlowIODevice
setStatusTo status device =
    { device | status = status }


setDetailsTo : Maybe DeviceDetails -> FlowIODevice -> FlowIODevice
setDetailsTo maybeDeviceDetails flowIODevice =
    { flowIODevice | details = maybeDeviceDetails }


setControlServiceStatusTo : ControlServiceStatus -> FlowIODevice -> FlowIODevice
setControlServiceStatusTo newStatus device =
    { device | controlServiceStatus = Just newStatus }


defaultCommand : FlowIOCommand
defaultCommand =
    { action = Inflate
    , ports = { port1 = PortClosed, port2 = PortClosed, port3 = PortClosed, port4 = PortClosed, port5 = PortClosed }
    , pumpPwm = 0
    }


getLastCommand : FlowIODevice -> Maybe FlowIOCommand
getLastCommand device =
    Maybe.map .command device.controlServiceStatus


setLastCommand : FlowIOCommand -> FlowIODevice -> FlowIODevice
setLastCommand command device =
    { device | controlServiceStatus = Maybe.map (\control -> { control | command = command }) device.controlServiceStatus }


updateCommandFromStatus : ControlServiceStatus -> FlowIOCommand -> FlowIOCommand
updateCommandFromStatus status command =
    let
        newPorts =
            { port1 = portFromBool status.port1
            , port2 = portFromBool status.port2
            , port3 = portFromBool status.port3
            , port4 = portFromBool status.port4
            , port5 = portFromBool status.port5
            }
    in
    { command | ports = newPorts }


setPort : Port -> PortState -> FlowIOCommand -> FlowIOCommand
setPort port_ portState command =
    let
        ports =
            command.ports

        newPorts =
            case port_ of
                Port1 ->
                    { ports | port1 = portState }

                Port2 ->
                    { ports | port2 = portState }

                Port3 ->
                    { ports | port3 = portState }

                Port4 ->
                    { ports | port4 = portState }

                Port5 ->
                    { ports | port5 = portState }

                Inlet ->
                    ports

                Outlet ->
                    ports
    in
    { command | ports = newPorts }


setAction : FlowIOAction -> FlowIOCommand -> FlowIOCommand
setAction action command =
    { command | action = action }


setPumpPwm : Int -> FlowIOCommand -> FlowIOCommand
setPumpPwm pwm command =
    if pwm >= 0 && pwm <= 0xFF then
        { command | pumpPwm = pwm }

    else
        Debug.log ("Got PWM value out of range " ++ Debug.toString pwm) command


setConfiguration : Maybe Configuration -> FlowIODevice -> FlowIODevice
setConfiguration maybeConfiguration flowIODevice =
    { flowIODevice | configuration = maybeConfiguration }


setPowerOffStatus : Maybe PowerOffStatus -> FlowIODevice -> FlowIODevice
setPowerOffStatus maybePowerOffStatus flowIODevice =
    { flowIODevice | powerOffServiceStatus = maybePowerOffStatus }



-- Device Command Types


type FlowIOAction
    = Inflate
    | Vacuum
    | Release
    | Stop


type PortState
    = PortOpen
    | PortClosed


type Port
    = Port1
    | Port2
    | Port3
    | Port4
    | Port5
    | Inlet
    | Outlet


type alias PortsState =
    { port1 : PortState, port2 : PortState, port3 : PortState, port4 : PortState, port5 : PortState }


type alias FlowIOCommand =
    { action : FlowIOAction, pumpPwm : Int, ports : PortsState }



-- Decoders


deviceDetailsDecoder : JD.Decoder DeviceDetails
deviceDetailsDecoder =
    JD.map3 DeviceDetails
        (JD.field "name" JD.string)
        (JD.field "id" JD.string)
        (JD.field "services" <| JD.list (JD.string |> JD.map serviceFromString))


controlServiceStatusDecoder : JD.Decoder ControlServiceStatus
controlServiceStatusDecoder =
    JD.succeed ControlServiceStatus
        |> required "pump1" JD.bool
        |> required "pump2" JD.bool
        |> required "inlet" JD.bool
        |> required "outlet" JD.bool
        |> required "port1" JD.bool
        |> required "port2" JD.bool
        |> required "port3" JD.bool
        |> required "port4" JD.bool
        |> required "port5" JD.bool
        |> required "active" JD.bool
        |> optional "lastCommand" controlCommandDecoder defaultCommand


commandActionDecoder : JD.Decoder FlowIOAction
commandActionDecoder =
    JD.field "action" JD.string
        |> JD.andThen
            (\string ->
                case String.trim string of
                    "inflate" ->
                        JD.succeed Inflate

                    "+" ->
                        JD.succeed Inflate

                    "vacuum" ->
                        JD.succeed Vacuum

                    "-" ->
                        JD.succeed Vacuum

                    "release" ->
                        JD.succeed Release

                    "&" ->
                        JD.succeed Release

                    "stop" ->
                        JD.succeed Stop

                    "!" ->
                        JD.succeed Stop

                    _ ->
                        JD.fail ("Expected valid action symbol ('+-^!') but found '" ++ string ++ "'")
            )


pwmValueDecoder : JD.Decoder Int
pwmValueDecoder =
    JD.field "pumpPwm" JD.int
        |> JD.andThen
            (\i ->
                case ( i < 100, i > 255 ) of
                    ( True, _ ) ->
                        JD.fail "Expected 'pwmVal' to be at least 100"

                    ( _, True ) ->
                        JD.fail "Expected 'pwmVal' to be at most 255"

                    _ ->
                        JD.succeed i
            )


portsFieldDecoder : JD.Decoder PortsState
portsFieldDecoder =
    JD.field "ports" portsObjectArrayOrNumberDecoder


portsObjectArrayOrNumberDecoder : JD.Decoder PortsState
portsObjectArrayOrNumberDecoder =
    let
        fromBool b =
            if b then
                PortOpen

            else
                PortClosed

        portsArrayDecoder =
            JD.map5
                (\p1 p2 p3 p4 p5 ->
                    { port1 = fromBool p1
                    , port2 = fromBool p2
                    , port3 = fromBool p3
                    , port4 = fromBool p4
                    , port5 = fromBool p5
                    }
                )
                (JD.index 0 JD.bool)
                (JD.index 1 JD.bool)
                (JD.index 2 JD.bool)
                (JD.index 3 JD.bool)
                (JD.index 4 JD.bool)

        portsObjectDecoder =
            JD.map5
                (\p1 p2 p3 p4 p5 ->
                    { port1 = fromBool p1
                    , port2 = fromBool p2
                    , port3 = fromBool p3
                    , port4 = fromBool p4
                    , port5 = fromBool p5
                    }
                )
                (JD.field "port1" JD.bool)
                (JD.field "port2" JD.bool)
                (JD.field "port3" JD.bool)
                (JD.field "port4" JD.bool)
                (JD.field "port5" JD.bool)

        portsNumberDecoder =
            JD.int
                |> JD.map
                    (\n ->
                        { port1 = fromBool ((n |> Bitwise.and 0x01) /= 0)
                        , port2 = fromBool ((n |> Bitwise.and 0x02) /= 0)
                        , port3 = fromBool ((n |> Bitwise.and 0x04) /= 0)
                        , port4 = fromBool ((n |> Bitwise.and 0x08) /= 0)
                        , port5 = fromBool ((n |> Bitwise.and 0x10) /= 0)
                        }
                    )
    in
    JD.oneOf
        [ portsArrayDecoder
        , portsObjectDecoder
        , portsNumberDecoder
        ]


controlCommandDecoder : JD.Decoder FlowIOCommand
controlCommandDecoder =
    JD.map3 (\action pwm ports -> { action = action, pumpPwm = pwm, ports = ports })
        commandActionDecoder
        pwmValueDecoder
        portsFieldDecoder


configurationEncoding : Configuration -> String
configurationEncoding configuration =
    case configuration of
        StandardConfiguration ->
            "GENERAL"

        InflationSeries ->
            "INFLATION_SERIES"

        InflationParallel ->
            "INFLATION_PARALLEL"

        VacuumSeries ->
            "VACUUM_SERIES"

        VacuumParallel ->
            "VACUUM_PARALLEL"


configurationDecoding : String -> Maybe Configuration
configurationDecoding int =
    case int of
        "GENERAL" ->
            Just StandardConfiguration

        "INFLATION_SERIES" ->
            Just InflationSeries

        "INFLATION_PARALLEL" ->
            Just InflationParallel

        "VACUUM_SERIES" ->
            Just VacuumSeries

        "VACUUM_PARALLEL" ->
            Just VacuumParallel

        _ ->
            Nothing


configurationToString : Configuration -> String
configurationToString configuration =
    case configuration of
        StandardConfiguration ->
            "General"

        InflationSeries ->
            "Inflation Series"

        InflationParallel ->
            "Inflation Parallel"

        VacuumSeries ->
            "Vacuum Series"

        VacuumParallel ->
            "Vacuum Parallel"



{-
   export type PowerOffStatus
       = { kind: "off" }
       | { kind: "disabled" }
       | { kind: "remaining", minutes: number }

-}


powerOffStatusDecoder : JD.Decoder PowerOffStatus
powerOffStatusDecoder =
    let
        decodeKind kind =
            case kind of
                "off" ->
                    JD.succeed DeviceOff

                "disabled" ->
                    JD.succeed PowerOffTimerDisabled

                "remaining" ->
                    JD.field "minutes" JD.int
                        |> JD.map PowerOffMinutesRemaining

                other ->
                    JD.fail ("Unknown kind '" ++ other ++ "'")
    in
    JD.field "kind" JD.string |> JD.andThen decodeKind



-- Encoders


encodeCommand : FlowIOCommand -> JE.Value
encodeCommand inst =
    JE.object
        [ ( "action", encodeAction inst.action )
        , ( "pumpPwm", JE.int inst.pumpPwm )
        , ( "ports"
          , JE.list JE.bool
                [ isPortOpen inst.ports.port1
                , isPortOpen inst.ports.port2
                , isPortOpen inst.ports.port3
                , isPortOpen inst.ports.port4
                , isPortOpen inst.ports.port5
                ]
          )
        ]


portFromBool : Bool -> PortState
portFromBool bool =
    if bool then
        PortOpen

    else
        PortClosed


isPortOpen : PortState -> Bool
isPortOpen portState =
    portState == PortOpen


togglePort : PortState -> PortState
togglePort portState =
    case portState of
        PortOpen ->
            PortClosed

        PortClosed ->
            PortOpen


encodeAction : FlowIOAction -> JE.Value
encodeAction action =
    let
        symbol =
            case action of
                Inflate ->
                    "inflate"

                Vacuum ->
                    "vacuum"

                Release ->
                    "release"

                Stop ->
                    "stop"
    in
    JE.string symbol


encodePowerOffStatus : PowerOffStatus -> JE.Value
encodePowerOffStatus powerOffStatus =
    {-
       export type PowerOffStatus
           = { kind: "off" }
           | { kind: "disabled" }
           | { kind: "remaining", minutes: number }

    -}
    case powerOffStatus of
        DeviceOff ->
            JE.object [ ( "kind", JE.string "off" ) ]

        PowerOffTimerDisabled ->
            JE.object [ ( "kind", JE.string "disabled" ) ]

        PowerOffMinutesRemaining minutes ->
            JE.object
                [ ( "kind", JE.string "remaining" )
                , ( "minutes", JE.int minutes )
                ]

        PowerOffStatusUnknown ->
            Debug.log "PowerOffStatusUnknown should not be sent to JS service!" <| JE.null



-- Analog Service
-- Analog Service ports


port requestAnalogReadings_ : { deviceIndex : Int, mode : JE.Value } -> Cmd msg


requestAnalogReadings : Int -> AnalogServiceRequest -> Cmd msg
requestAnalogReadings deviceIndex request =
    requestAnalogReadings_ { deviceIndex = deviceIndex, mode = encodeAnalogServiceRequest request }


port listenToAnalogReadings_ : ({ deviceIndex : Int, readings : JD.Value } -> msg) -> Sub msg


listenToAnalogReadings : (Int -> Result JD.Error AnalogReadings -> msg) -> Sub msg
listenToAnalogReadings makeMessage =
    let
        unwrap { deviceIndex, readings } =
            makeMessage deviceIndex <| JD.decodeValue (JD.array JD.int) readings
    in
    listenToAnalogReadings_ unwrap


type AnalogServiceRequest
    = RequestStopAnalog
    | RequestSingleAnalogRead
    | RequestContinuousAnalog Int


encodeAnalogServiceRequest : AnalogServiceRequest -> JE.Value
encodeAnalogServiceRequest analogServiceRequest =
    case analogServiceRequest of
        RequestStopAnalog ->
            JE.object [ ( "kind", JE.string "stop" ) ]

        RequestSingleAnalogRead ->
            JE.object [ ( "kind", JE.string "single" ) ]

        RequestContinuousAnalog averagingWindowSize ->
            JE.object
                [ ( "kind", JE.string "continuous" )
                , ( "averagingWindowSamples", JE.int averagingWindowSize )
                ]


type alias AnalogReadings =
    Array Int


type alias AnalogService =
    Service
        { lastReading : AnalogReadings
        , readingsTimestamp : Time.Posix
        }
        AnalogServiceRequest


setAnalogServiceData : AnalogService -> FlowIODevice -> FlowIODevice
setAnalogServiceData newService flowIODevice =
    { flowIODevice | analogSensorsService = newService }


setNewAnalogServiceReadings : Time.Posix -> AnalogReadings -> FlowIODevice -> FlowIODevice
setNewAnalogServiceReadings posix analogReadings device =
    let
        updatedService =
            device.analogSensorsService
                |> updateData { lastReading = analogReadings, readingsTimestamp = posix }
    in
    setAnalogServiceData updatedService device


setNewAnalogReadRequest : AnalogServiceRequest -> FlowIODevice -> FlowIODevice
setNewAnalogReadRequest request device =
    let
        updated =
            device.analogSensorsService
                |> updateCommand request
    in
    setAnalogServiceData updated device


sensorsResolution : number
sensorsResolution =
    2 ^ 12



-- PID Service


type alias PidControl =
    { proportional : Float
    , integrative : Float
    , differential : Float
    , inputChannel : Int
    , outputPort : PortsState
    }


type alias PidSettings =
    { positiveCommand : FlowIOAction
    , negativeCommand : FlowIOAction
    , control1 : PidControl
    , control2 : PidControl
    , control3 : PidControl
    , control4 : PidControl
    , control5 : PidControl
    }


type alias PidGoals =
    { goal1 : Int
    , goal2 : Int
    , goal3 : Int
    , goal4 : Int
    , goal5 : Int
    }


setIncomingPidSettings : PidSettings -> FlowIODevice -> FlowIODevice
setIncomingPidSettings pidSettings flowIODevice =
    { flowIODevice | pidService = updateData pidSettings flowIODevice.pidService }


setOutgoingPidSettings : PidSettings -> FlowIODevice -> FlowIODevice
setOutgoingPidSettings pidSettings flowIODevice =
    { flowIODevice
        | pidService =
            mapCommand
                (\maybeCommand ->
                    case maybeCommand of
                        Just outgoing ->
                            { outgoing | settings = pidSettings }

                        Nothing ->
                            { settings = pidSettings, goals = defaultPidGoals }
                )
                flowIODevice.pidService
    }


setOutgoingPidGoals : PidGoals -> FlowIODevice -> FlowIODevice
setOutgoingPidGoals pidGoals flowIODevice =
    { flowIODevice
        | pidService =
            mapCommand
                (\maybeCommand ->
                    case maybeCommand of
                        Just outgoing ->
                            { outgoing | goals = pidGoals }

                        Nothing ->
                            { settings = getData flowIODevice.pidService |> Maybe.withDefault defaultPidSettings
                            , goals = pidGoals
                            }
                )
                flowIODevice.pidService
    }


defaultPidGoals : PidGoals
defaultPidGoals =
    { goal1 = 0
    , goal2 = 0
    , goal3 = 0
    , goal4 = 0
    , goal5 = 0
    }


defaultPidSettings : PidSettings
defaultPidSettings =
    { positiveCommand = Inflate
    , negativeCommand = Vacuum
    , control1 = defaultPidControl
    , control2 = defaultPidControl
    , control3 = defaultPidControl
    , control4 = defaultPidControl
    , control5 = defaultPidControl
    }


defaultPidControl : PidControl
defaultPidControl =
    { differential = 0
    , inputChannel = 0
    , integrative = 0
    , outputPort =
        { port1 = PortClosed
        , port2 = PortClosed
        , port3 = PortClosed
        , port4 = PortClosed
        , port5 = PortClosed
        }
    , proportional = 0
    }


pidSettingsDecoder : JD.Decoder PidSettings
pidSettingsDecoder =
    JD.succeed PidSettings
        |> required "positiveCommand" commandActionDecoder
        |> required "negativeCommand" commandActionDecoder
        |> required "control1" flowIOPidControlDecoder
        |> required "control2" flowIOPidControlDecoder
        |> required "control3" flowIOPidControlDecoder
        |> required "control4" flowIOPidControlDecoder
        |> required "control5" flowIOPidControlDecoder


flowIOPidControlDecoder : JD.Decoder PidControl
flowIOPidControlDecoder =
    JD.succeed PidControl
        |> required "proportional" JD.float
        |> required "integrative" JD.float
        |> required "differential" JD.float
        |> required "inputChannel" JD.int
        |> required "outputPort" portsObjectArrayOrNumberDecoder


encodePidSettings : PidSettings -> JE.Value
encodePidSettings pidSettings =
    JE.object <|
        [ ( "positiveCommand", encodeAction pidSettings.positiveCommand )
        , ( "negativeCommand", encodeAction pidSettings.negativeCommand )
        , ( "controls"
          , JE.list encodeFlowIOPidControl
                [ pidSettings.control1
                , pidSettings.control2
                , pidSettings.control3
                , pidSettings.control4
                , pidSettings.control5
                ]
          )
        ]


encodePortState : PortState -> JE.Value
encodePortState portState =
    case portState of
        PortOpen ->
            JE.bool True

        PortClosed ->
            JE.bool False


encodeFlowIOPortsState : PortsState -> JE.Value
encodeFlowIOPortsState portsState =
    JE.object <|
        [ ( "port1", encodePortState portsState.port1 )
        , ( "port2", encodePortState portsState.port2 )
        , ( "port3", encodePortState portsState.port3 )
        , ( "port4", encodePortState portsState.port4 )
        , ( "port5", encodePortState portsState.port5 )
        ]


encodeFlowIOPidControl : PidControl -> JE.Value
encodeFlowIOPidControl pidControl =
    JE.object <|
        [ ( "proportional", JE.float pidControl.proportional )
        , ( "integrative", JE.float pidControl.integrative )
        , ( "differential", JE.float pidControl.differential )
        , ( "inputChannel", JE.int pidControl.inputChannel )
        , ( "outputPort", encodeFlowIOPortsState pidControl.outputPort )
        ]
