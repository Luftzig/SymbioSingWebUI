port module FlowIO exposing (..)

import Json.Decode as JD
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as JE


--{{ Communicating with FlowIO devices }}
port createDevice : () -> Cmd msg


port connectToDevice : Int -> Cmd msg


port disconnectDevice : Int -> Cmd msg


port deviceStatusChanged : ({ deviceIndex : Int, status : String, details : Maybe JE.Value } -> msg) -> Sub msg


port controlServiceStatusChanged : ({ deviceIndex : Int, status : JE.Value } -> msg) -> Sub msg


port listenToControlService : Int -> Cmd msg


port sendCommand : { deviceIndex : Int, command : JE.Value } -> Cmd msg


port stopAll : Int -> Cmd msg


-- FlowIODevice types


type alias FlowIODevice =
    { status : FlowIOStatus
    , details : Maybe DeviceDetails
    , controlServiceStatus : Maybe ControlServiceStatus
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
    | UnknownService String


serviceToString : FlowIOService -> String
serviceToString service =
    case service of
        ControlService ->
            "control-service"

        ConfigService ->
            "config-service"

        UnknownService details ->
            "unknown-service:" ++ details


serviceFromString : String -> FlowIOService
serviceFromString string =
    case string of
        "control-service" ->
            ControlService

        "config-service" ->
            ConfigService

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
    = Standard
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
    , ports = { port1 = Close, port2 = Close, port3 = Close, port4 = Close, port5 = Close }
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



-- Device Command Types


type FlowIOAction
    = Inflate
    | Vacuum
    | Release
    | Stop


type PortState
    = Open
    | Close


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
    JD.map3 (DeviceDetails)
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


portsDecoder : JD.Decoder PortsState
portsDecoder =
    let
        fromBool b =
            if b then
                Open

            else
                Close
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


controlCommandDecoder : JD.Decoder FlowIOCommand
controlCommandDecoder =
    JD.map3 (\action pwm ports -> { action = action, pumpPwm = pwm, ports = ports })
        commandActionDecoder
        pwmValueDecoder
        portsDecoder


configurationEncoding : Configuration -> Int
configurationEncoding configuration =
    case configuration of
        Standard ->
            0

        InflationSeries ->
            1

        InflationParallel ->
            2

        VacuumSeries ->
            3

        VacuumParallel ->
            4


configurationDecoding : Int -> Maybe Configuration
configurationDecoding int =
    case int of
        0 ->
            Just Standard

        1 ->
            Just InflationSeries

        2 ->
            Just InflationParallel

        3 ->
            Just VacuumSeries

        4 ->
            Just VacuumParallel

        _ ->
            Nothing


configurationToString : Configuration -> String
configurationToString configuration =
    case configuration of
        Standard ->
            "General"

        InflationSeries ->
            "Inflation Series"

        InflationParallel ->
            "Inflation Parallel"

        VacuumSeries ->
            "Vacuum Series"

        VacuumParallel ->
            "Vacuum Parallel"



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
        Open

    else
        Close


isPortOpen : PortState -> Bool
isPortOpen portState =
    portState == Open


togglePort : PortState -> PortState
togglePort portState =
    case portState of
        Open ->
            Close

        Close ->
            Open


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
