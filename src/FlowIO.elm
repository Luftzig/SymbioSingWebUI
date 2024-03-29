port module FlowIO exposing
    ( Action(..)
    , AnalogReadings
    , AnalogService
    , AnalogServiceRequest(..)
    , Command
    , Configuration(..)
    , ControlServiceStatus
    , Device
    , DeviceDetails
    , DeviceId
    , Port(..)
    , PortState(..)
    , PortsState
    , PowerOffStatus(..)
    , Service(..)
    , Status(..)
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
    , getLastCommand
    , isPortOpen
    , listenToAnalogReadings
    , listenToBatteryLevel
    , listenToControlService
    , listenToDeviceConfiguration
    , listenToDeviceControlStatus
    , listenToDeviceStatus
    , listenToPowerOffStatus
    , portFromBool
    , portToIndex
    , portsAllClosed
    , queryDeviceConfiguration
    , queryPowerOffStatus
    , requestAnalogReadings
    , requestBatteryLevel
    , sendCommand
    , sendDeviceConfiguration
    , sendPowerOffStatus
    , sendStopAll
    , sensorsResolution
    , serviceFromString
    , serviceToPrettyName
    , serviceToString
    , setAction
    , setAnalogServiceData
    , setBatteryLevel
    , setConfiguration
    , setControlServiceStatusTo
    , setDetailsTo
    , setLastCommand
    , setNewAnalogReadRequest
    , setNewAnalogServiceReadings
    , setPort
    , setPowerOffStatus
    , setPumpPwm
    , setStatusTo
    , translateAction
    , translateActionInCommand
    , updateCommandFromStatus
    )

import Array exposing (Array)
import Extra.RemoteService as RemoteService exposing (RemoteService, updateCommand, updateData)
import Json.Decode as JD
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as JE
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



-- Battery Service


port requestBatteryLevel : { deviceIndex : Int } -> Cmd msg


port listenToBatteryLevel : ({ deviceIndex : Int, level : Float } -> msg) -> Sub msg



-- FlowIODevice types
{- TODO: Should I wrap all the service in a type to represent their status?
   possible values are probably: not supported, expecting update, updated? We also have local value versus remote one.
-}


type alias Device =
    { status : Status
    , details : Maybe DeviceDetails
    , controlServiceStatus : Maybe ControlServiceStatus
    , powerOffServiceStatus : Maybe PowerOffStatus
    , configuration : Maybe Configuration
    , analogSensorsService : AnalogService
    , batteryLevel : Maybe Float
    }


type alias DeviceId =
    String


type alias DeviceDetails =
    { name : String, id : DeviceId, services : List Service }


type Status
    = NotConnected
    | Pending
    | Connected


type Service
    = ControlService
    | ConfigService
    | BatteryService
    | PowerOffService
    | AnalogService
    | UnknownService String


type PowerOffStatus
    = DeviceOff
    | PowerOffTimerDisabled
    | PowerOffMinutesRemaining Int
    | PowerOffStatusUnknown


serviceToString : Service -> String
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


serviceToPrettyName : Service -> String
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


serviceFromString : String -> Service
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
    , command : Command
    }


type Configuration
    = StandardConfiguration
    | InflationSeries
    | InflationParallel
    | VacuumSeries
    | VacuumParallel
    | RegulatedPressure
    | RegulatedVacuum



-- FlowIODevice setters


defaultDevice : Device
defaultDevice =
    { status = NotConnected
    , details = Nothing
    , controlServiceStatus = Nothing
    , configuration = Nothing
    , powerOffServiceStatus = Nothing
    , analogSensorsService = RemoteService.init
    , batteryLevel = Nothing
    }


setStatusTo : Status -> Device -> Device
setStatusTo status device =
    { device | status = status }


setDetailsTo : Maybe DeviceDetails -> Device -> Device
setDetailsTo maybeDeviceDetails flowIODevice =
    { flowIODevice | details = maybeDeviceDetails }


setControlServiceStatusTo : ControlServiceStatus -> Device -> Device
setControlServiceStatusTo newStatus device =
    { device | controlServiceStatus = Just newStatus }


portsAllClosed : { port1 : PortState, port2 : PortState, port3 : PortState, port4 : PortState, port5 : PortState }
portsAllClosed =
    { port1 = PortClosed, port2 = PortClosed, port3 = PortClosed, port4 = PortClosed, port5 = PortClosed }


defaultCommand : Command
defaultCommand =
    { action = Inflate
    , ports = portsAllClosed
    , pumpPwm = 0
    }


getLastCommand : Device -> Maybe Command
getLastCommand device =
    Maybe.map .command device.controlServiceStatus


setLastCommand : Command -> Device -> Device
setLastCommand command device =
    { device | controlServiceStatus = Maybe.map (\control -> { control | command = command }) device.controlServiceStatus }


updateCommandFromStatus : ControlServiceStatus -> Command -> Command
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


portToIndex : Port -> Int
portToIndex port_ =
    case port_ of
        Port1 ->
            1

        Port2 ->
            2

        Port3 ->
            3

        Port4 ->
            4

        Port5 ->
            5


setPort : Port -> PortState -> Command -> Command
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
    in
    { command | ports = newPorts }


setAction : Action -> Command -> Command
setAction action command =
    { command | action = action }


setPumpPwm : Int -> Command -> Command
setPumpPwm pwm command =
    if pwm >= 0 && pwm <= 0xFF then
        { command | pumpPwm = pwm }

    else
        --Debug.log ("Got PWM value out of range " ++ Debug.toString pwm)
        command


setConfiguration : Maybe Configuration -> Device -> Device
setConfiguration maybeConfiguration flowIODevice =
    { flowIODevice | configuration = maybeConfiguration }


setPowerOffStatus : Maybe PowerOffStatus -> Device -> Device
setPowerOffStatus maybePowerOffStatus flowIODevice =
    { flowIODevice | powerOffServiceStatus = maybePowerOffStatus }


setBatteryLevel : Maybe Float -> Device -> Device
setBatteryLevel newLevel device =
    { device | batteryLevel = newLevel }



-- Device Command Types


type Action
    = Inflate
    | Vacuum
    | Release
    | Stop
    | Actuate -- Depends on context: If using vacuum configuration then it is Vacuum, if pressure it is Inflate


type PortState
    = PortOpen
    | PortClosed


type Port
    = Port1
    | Port2
    | Port3
    | Port4
    | Port5


type alias PortsState =
    { port1 : PortState, port2 : PortState, port3 : PortState, port4 : PortState, port5 : PortState }


type alias Command =
    { action : Action, pumpPwm : Int, ports : PortsState }



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


commandActionDecoder : JD.Decoder Action
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

                    "actuate" ->
                        JD.succeed Actuate

                    _ ->
                        JD.fail ("Expected valid action symbol ('+-^!') but found '" ++ string ++ "'")
            )


pwmValueDecoder : JD.Decoder Int
pwmValueDecoder =
    JD.field "pumpPwm" JD.int
        |> JD.andThen
            (\i ->
                case ( i < 0, i > 255 ) of
                    ( True, _ ) ->
                        JD.fail "Expected 'pwmVal' to be positive"

                    ( _, True ) ->
                        JD.fail "Expected 'pwmVal' to be at most 255"

                    _ ->
                        JD.succeed i
            )


portsDecoder : JD.Decoder PortsState
portsDecoder =
    let
        fromBool b =
            if b then
                PortOpen

            else
                PortClosed
    in
    JD.field "ports" <|
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


controlCommandDecoder : JD.Decoder Command
controlCommandDecoder =
    JD.map3 (\action pwm ports -> { action = action, pumpPwm = pwm, ports = ports })
        commandActionDecoder
        pwmValueDecoder
        portsDecoder


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

        RegulatedPressure ->
            "REGULATED_PRESSURE"

        RegulatedVacuum ->
            "REGULATED_VACUUM"


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

        "REGULATED_PRESSURE" ->
            Just RegulatedPressure

        "REGULATED_VACUUM" ->
            Just RegulatedVacuum

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

        RegulatedPressure ->
            "Regulated Pressure"

        RegulatedVacuum ->
            "Regulated Vacuum"


translateAction : Device -> Action -> Action
translateAction device action =
    case action of
        Actuate ->
            case device.configuration of
                Just conf ->
                    case conf of
                        InflationParallel ->
                            Inflate

                        InflationSeries ->
                            Inflate

                        RegulatedPressure ->
                            Inflate

                        StandardConfiguration ->
                            Inflate

                        VacuumSeries ->
                            Vacuum

                        VacuumParallel ->
                            Vacuum

                        RegulatedVacuum ->
                            Vacuum

                Nothing ->
                    Inflate

        _ ->
            action


translateActionInCommand : Device -> Command -> Command
translateActionInCommand device command =
    { command | action = translateAction device command.action }



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


encodeCommand : Command -> JE.Value
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


{-| Map a boolean to port value:
    True -> Port Open
    False -> Port Closed
-}
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


encodeAction : Action -> JE.Value
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

                Actuate ->
                    "actuate"
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
            JE.null



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
    RemoteService
        { lastReading : AnalogReadings
        , readingsTimestamp : Time.Posix
        }
        AnalogServiceRequest


setAnalogServiceData : AnalogService -> Device -> Device
setAnalogServiceData newService flowIODevice =
    { flowIODevice | analogSensorsService = newService }


setNewAnalogServiceReadings : Time.Posix -> AnalogReadings -> Device -> Device
setNewAnalogServiceReadings posix analogReadings device =
    let
        updatedService =
            device.analogSensorsService
                |> updateData { lastReading = analogReadings, readingsTimestamp = posix }
    in
    setAnalogServiceData updatedService device


setNewAnalogReadRequest : AnalogServiceRequest -> Device -> Device
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
