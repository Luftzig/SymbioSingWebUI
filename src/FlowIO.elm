module FlowIO exposing (..)

import Json.Decode as JD
import Json.Decode.Pipeline exposing (optional, required)



-- FlowIODevice types


type alias FlowIODevice =
    { status : FlowIOStatus
    , details : Maybe DeviceDetails
    , controlServiceStatus : Maybe ControlServiceStatus
    }


type alias DeviceDetails =
    { name : String, id : String, services : List String }


type FlowIOStatus
    = NotConnected
    | Pending
    | Connected


type FlowIOService
    = ControlService


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
    , lastCommand : Maybe FlowIOCommand
    }



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


type alias PortsState =
    { port1 : PortState, port2 : PortState, port3 : PortState, port4 : PortState, port5 : PortState }


type alias FlowIOCommand =
    { action : FlowIOAction, pumpPwm : Int, ports : PortsState }



-- Decoders


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
        |> optional "lastCommand" (JD.maybe controlCommandDecoder) Nothing


commandActionDecoder : JD.Decoder FlowIOAction
commandActionDecoder =
    JD.field "action" JD.string
        |> JD.andThen
            (\string ->
                case String.trim string of
                    "+" ->
                        JD.succeed Inflate

                    "-" ->
                        JD.succeed Vacuum

                    "&" ->
                        JD.succeed Release

                    "!" ->
                        JD.succeed Stop

                    _ ->
                        JD.fail ("Expected valid action symbol ('+-^!') but found '" ++ string ++ "'")
            )


pwmValueDecoder : JD.Decoder Int
pwmValueDecoder =
    JD.field "pwmVal" JD.int
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
                Close

            else
                Open
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
