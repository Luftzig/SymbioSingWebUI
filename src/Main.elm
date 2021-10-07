port module Main exposing (main)

import Array exposing (Array)
import Array.Extra
import Browser
import Element as UI
import Element.Input as UIInput
import Html
import Scheduler


type alias Model =
    { devices : Array FlowIODevice }


type alias FlowIODevice =
    { name : String
    , status : FlowIOStatus
    }


type FlowIOStatus
    = NotConnected
    | Pending
    | Connected


changeStatusTo : FlowIOStatus -> FlowIODevice -> FlowIODevice
changeStatusTo status device =
    { device | status = status }


type Msg
    = ConnectToDevice Int
    | DeviceConnected Int FlowIOStatus
    | DeviceStatusChanged { deviceIndex: Int, status: String }


initModel : Model
initModel =
    { devices = Array.fromList [ { name = "default", status = NotConnected } ] }


port createDevice : () -> Cmd msg


port connectToDevice : Int -> Cmd msg


port deviceStatusChanged : ({ deviceIndex: Int, status: String} -> msg) -> Sub msg

main : Program () Model Msg
main =
    Browser.document
        { init = \() -> ( initModel, createDevice () )
        , subscriptions = \_ -> Sub.batch [
        deviceStatusChanged DeviceStatusChanged ]
        , update = update
        , view = view
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ConnectToDevice deviceIndex ->
            case Array.get deviceIndex model.devices of
                Just device ->
                    if device.status == NotConnected then
                        ( { model | devices = Array.Extra.update deviceIndex (changeStatusTo Pending) model.devices }, connectToDevice deviceIndex )

                    else
                        ( model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        DeviceConnected int status ->
            Debug.todo "DeviceConnected"

        DeviceStatusChanged ({deviceIndex, status}) ->
            case (Array.get deviceIndex model.devices) of
                Just device ->
                    case status of
                        "connected" ->
                            ({model | devices = Array.Extra.update deviceIndex (changeStatusTo Connected) model.devices}, Cmd.none)

                        "disconnected" ->
                            ({model | devices = Array.Extra.update deviceIndex (changeStatusTo NotConnected) model.devices}, Cmd.none)

                        _ ->
                            ({model | devices = Array.Extra.update deviceIndex (changeStatusTo Pending) model.devices}, Cmd.none)
                Nothing ->
                    (model, Cmd.none)




view : Model -> Browser.Document Msg
view model =
    { title = "FlowIO"
    , body = [ body model ]
    }


body : Model -> Html.Html Msg
body model =
    let
        devices =
            Array.indexedMap
                (\index deviceInfo ->
                    case deviceInfo.status of
                        NotConnected ->
                            UIInput.button [] { label = UI.text "connect", onPress = Just <| ConnectToDevice index }

                        Pending ->
                            UI.el [] <| UI.text "Connecting..."

                        Connected ->
                            UI.el [] <| UI.text "Connected"
                )
                model.devices
                |> Array.toList
    in
    UI.layout [] <|
        UI.column [] [ UI.row [] devices ]
