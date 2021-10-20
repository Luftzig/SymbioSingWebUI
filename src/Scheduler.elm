port module Scheduler exposing (Model, Msg(..), initModel, subscriptions, update, view)

import Array exposing (Array)
import Array.Extra as AE
import Browser
import Dict exposing (Dict)
import Element as El exposing (fillPortion, htmlAttribute, indexedTable)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input exposing (labelAbove, labelHidden)
import File
import File.Download
import File.Select
import FlowIO exposing (..)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Json.Decode as JD
import Json.Encode as JE
import List.Extra
import Styles exposing (lightGrey, textField)
import Task


port executeInstructions : JE.Value -> Cmd msg


main =
    Browser.element
        { init = \() -> ( initModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type SchedulerState
    = Disabled
    | Editable
    | RunningInstructions


type Time
    = MilliSeconds Int


milliseconds : Time -> Int
milliseconds (MilliSeconds n) =
    n


millisToString : Time -> String
millisToString ms =
    case ms of
        MilliSeconds int ->
            String.fromInt int


type alias SchedulerInstruction =
    { time : Time, deviceInstructions : Dict String FlowIOCommand }


type alias Model =
    { state : SchedulerState
    , instructions : Array SchedulerInstruction
    , devices : Array FlowIODevice
    }


initModel : Model
initModel =
    { state = Disabled
    , instructions = Array.fromList []
    , devices = Array.empty
    }


externalClass : String -> El.Attribute msg
externalClass class =
    htmlAttribute <| Html.Attributes.class class


fullWidth =
    El.width <| El.fill


view : Model -> Html Msg
view model =
    El.layout
        [ Font.family [ Font.typeface "Overpass", Font.typeface "Open Sans", Font.typeface "Helvetica", Font.sansSerif ]
        , Font.size 15
        ]
        (El.column [ fullWidth, El.height <| El.fill ]
            [ header
            , devicesTable model
            , buttons model
            ]
        )


white =
    El.rgb 1.0 1.0 1.0


buttonPadding =
    El.paddingXY 12 4


header : El.Element Msg
header =
    El.row [ fullWidth, El.spacingXY 20 8 ]
        [ El.el [ El.width <| El.fillPortion 3, El.spaceEvenly, Font.bold, Font.size 18 ] <| El.text "Schedule"
        , Element.Input.button [ El.width <| fillPortion 1, externalClass "btn-scheduler", Font.color white, buttonPadding ]
            { onPress = Just AddInstruction, label = El.text "+ Add Row" }
        , Element.Input.button [ El.width <| fillPortion 1, externalClass "btn-scheduler", Font.color white, buttonPadding ]
            { onPress = Just DeleteLastInstruction, label = El.text "Delete" }
        , Element.Input.button [ El.width <| fillPortion 1, externalClass "btn-scheduler", Font.color white, buttonPadding ]
            { onPress = Just ResetInstructions, label = El.text "Reset" }
        ]


getInstructionForDevice : SchedulerInstruction -> FlowIO.FlowIODevice -> Maybe FlowIOCommand
getInstructionForDevice inst { details } =
    case details of
        Just found ->
            Dict.get found.id inst.deviceInstructions

        Nothing ->
            Nothing


type SchedulerRow
    = ExistingInstruction SchedulerInstruction
    | PlannedInstruction


devicesTable : Model -> El.Element Msg
devicesTable model =
    let
        instructions =
            Array.map ExistingInstruction model.instructions
                |> Array.push PlannedInstruction
                |> Array.toList

        maxTime =
            instructions
                |> List.map
                    (\row ->
                        case row of
                            ExistingInstruction inst ->
                                .time inst |> milliseconds

                            _ ->
                                0
                    )
                |> List.maximum
                |> Maybe.map ((+) 10)
                |> Maybe.withDefault 0

        border =
            [ Border.width 1
            , Border.color <| El.rgb255 0xDD 0xDD 0xDD
            ]

        cellHeight =
            El.height <| El.px 42

        commonAttrs =
            cellHeight :: border

        cellWrapper content =
            El.el (El.padding 2 :: commonAttrs) content

        timeColumn : El.IndexedColumn SchedulerRow Msg
        timeColumn =
            { header = El.el ([] ++ border) <| El.text "Time (ms)"
            , width = El.maximum 120 (fillPortion 1)
            , view =
                \index row ->
                    cellWrapper <|
                        case row of
                            ExistingInstruction instruction ->
                                textField [ El.htmlAttribute <| Html.Attributes.type_ "number" ]
                                    { onChange = InstructionTimeChanged index
                                    , label = "Time at step " ++ String.fromInt index
                                    , text = millisToString instruction.time
                                    , placeholder = Just <| Element.Input.placeholder [] <| El.text "0"
                                    , isDisabled = Debug.log "model.state" model.state /= Editable
                                    , onChangeDisabled = DisabledFieldClicked "Time column disabled"
                                    }

                            PlannedInstruction ->
                                textField []
                                    { onChange = InstructionTimeChanged index
                                    , label = "Time at step " ++ String.fromInt index
                                    , text = String.fromInt maxTime
                                    , placeholder = Just <| Element.Input.placeholder [] <| El.text <| String.fromInt maxTime
                                    , isDisabled = True
                                    , onChangeDisabled = AddInstruction
                                    }
            }

        columnsForDevice : FlowIODevice -> List (El.IndexedColumn SchedulerRow Msg)
        columnsForDevice device =
            let
                name =
                    Maybe.map .name device.details |> Maybe.withDefault "Unknown"
            in
            [ { header = El.el border <| El.text (name ++ " Action")
              , width = El.maximum 140 <| fillPortion 1
              , view =
                    \index row ->
                        cellWrapper <|
                            case row of
                                ExistingInstruction instruction ->
                                    let
                                        devInstruction =
                                            getInstructionForDevice instruction device
                                    in
                                    case devInstruction of
                                        Just inst ->
                                            Element.Input.radio [ Font.size 11, El.htmlAttribute <| Html.Attributes.style "flex-wrap" "wrap", cellHeight ]
                                                { options =
                                                    [ Element.Input.option Inflate <| El.text "Inflate"
                                                    , Element.Input.option Vacuum <| El.text "Vacuum"
                                                    , Element.Input.option Release <| El.text "Release"
                                                    , Element.Input.option Stop <| El.text "Stop"
                                                    ]
                                                , onChange = ActionChanged device index
                                                , selected = Just inst.action
                                                , label = labelHidden "Action"
                                                }

                                        Nothing ->
                                            El.none

                                PlannedInstruction ->
                                    El.el commonAttrs <| El.none
              }
            , { header = El.el border <| El.text (name ++ " PWM")
              , width = El.maximum 120 <| fillPortion 1
              , view =
                    \index row ->
                        cellWrapper <|
                            case row of
                                ExistingInstruction instruction ->
                                    let
                                        devInstruction =
                                            getInstructionForDevice instruction device

                                        numberAttrs =
                                            [ Html.Attributes.min "100"
                                            , Html.Attributes.max "255"
                                            , Html.Attributes.step "5"
                                            , Html.Attributes.type_ "number"
                                            ]
                                                |> List.map El.htmlAttribute
                                    in
                                    case devInstruction of
                                        Just inst ->
                                            textField numberAttrs
                                                { isDisabled = model.state /= Editable
                                                , onChange = PWMChanged device index
                                                , onChangeDisabled = DisabledFieldClicked ""
                                                , label = "PWM " ++ name ++ " row " ++ String.fromInt index
                                                , text = String.fromInt inst.pumpPwm
                                                , placeholder = Nothing
                                                }

                                        Nothing ->
                                            El.none

                                PlannedInstruction ->
                                    El.none
              }
            , { header = El.el border <| El.text ("Device " ++ name ++ " Ports")
              , width = El.maximum 140 <| fillPortion 3
              , view =
                    \index row ->
                        cellWrapper <|
                            case row of
                                ExistingInstruction instruction ->
                                    let
                                        devInstruction =
                                            getInstructionForDevice instruction device
                                    in
                                    case devInstruction of
                                        Just inst ->
                                            El.row []
                                                [ Element.Input.checkbox []
                                                    { label = labelAbove [ Font.size 8 ] <| El.text "Port 1"
                                                    , checked = inst.ports.port1 == Open
                                                    , icon = Element.Input.defaultCheckbox
                                                    , onChange = PortStateChanged device index Port1
                                                    }
                                                , Element.Input.checkbox []
                                                    { label = labelAbove [ Font.size 8 ] <| El.text "Port 2"
                                                    , checked = inst.ports.port2 == Open
                                                    , icon = Element.Input.defaultCheckbox
                                                    , onChange = PortStateChanged device index Port2
                                                    }
                                                , Element.Input.checkbox []
                                                    { label = labelAbove [ Font.size 8 ] <| El.text "Port 3"
                                                    , checked = inst.ports.port3 == Open
                                                    , icon = Element.Input.defaultCheckbox
                                                    , onChange = PortStateChanged device index Port3
                                                    }
                                                , Element.Input.checkbox []
                                                    { label = labelAbove [ Font.size 8 ] <| El.text "Port 4"
                                                    , checked = inst.ports.port4 == Open
                                                    , icon = Element.Input.defaultCheckbox
                                                    , onChange = PortStateChanged device index Port4
                                                    }
                                                , Element.Input.checkbox []
                                                    { label = labelAbove [ Font.size 8 ] <| El.text "Port 5"
                                                    , checked = inst.ports.port5 == Open
                                                    , icon = Element.Input.defaultCheckbox
                                                    , onChange = PortStateChanged device index Port5
                                                    }
                                                ]

                                        Nothing ->
                                            El.none

                                PlannedInstruction ->
                                    El.none
              }
            ]
    in
    indexedTable [ fullWidth, Font.size 11, El.padding 2 ]
        { data = instructions
        , columns = timeColumn :: (List.concat <| AE.mapToList columnsForDevice model.devices)
        }


attrIfElse : Bool -> El.Attribute msg -> El.Attribute msg -> El.Attribute msg
attrIfElse condition ifTrue ifFalse =
    if condition then
        ifTrue

    else
        ifFalse


buttons model =
    El.row [ Font.color white, El.centerX, El.spacingXY 8 20 ]
        [ Element.Input.button
            [ externalClass "btn-scheduler"
            , buttonPadding
            , Background.color <| El.rgb255 0x4C 0xAF 0x50
            , attrIfElse (model.state /= Editable) (El.alpha 0.4) (El.alpha 1.0)
            ]
            { label = El.text "▶️ Run"
            , onPress =
                if model.state == Editable then
                    Just RunInstructions

                else
                    Nothing
            }
        , Element.Input.button [ externalClass "btn-scheduler", El.paddingXY 12 4 ] { label = El.text "Save", onPress = Just DownloadInstructions }
        , Element.Input.button [ externalClass "btn-scheduler", El.paddingXY 12 4 ] { label = El.text "Upload", onPress = Just UploadInstructions }
        ]


type Msg
    = AddInstruction
    | DeleteLastInstruction
    | ResetInstructions
    | DevicesChanged (Array FlowIODevice)
    | InstructionTimeChanged Int String
    | ActionChanged FlowIODevice Int FlowIOAction
    | PWMChanged FlowIODevice Int String
    | PortStateChanged FlowIODevice Int Port Bool
    | RunInstructions
    | DownloadInstructions
    | UploadInstructions
    | UploadSelected File.File
    | FileRead String
    | DisabledFieldClicked String


createNewInstruction : Array FlowIODevice -> Array SchedulerInstruction -> SchedulerInstruction
createNewInstruction devices existingInstructions =
    let
        maxTime =
            existingInstructions
                |> Array.map (.time >> (\(MilliSeconds t) -> t))
                |> Array.toList
                |> List.maximum
                |> Maybe.withDefault -1

        defaultDeviceInstruction : FlowIOCommand
        defaultDeviceInstruction =
            { action = Stop
            , pumpPwm = 255
            , ports = allPortsClosed
            }

        allPortsClosed =
            { port1 = Close, port2 = Close, port3 = Close, port4 = Close, port5 = Close }
    in
    { time = MilliSeconds (maxTime + 1)
    , deviceInstructions =
        Dict.fromList <|
            Array.toList <|
                AE.filterMap (\device -> Maybe.map (\{ id } -> ( id, defaultDeviceInstruction )) device.details) devices
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        updateDeviceInstruction :
            Int
            -> FlowIODevice
            -> (FlowIOCommand -> FlowIOCommand)
            -> Array SchedulerInstruction
            -> Array SchedulerInstruction
        updateDeviceInstruction index device updater instructions =
            let
                id =
                    Maybe.map .id device.details

                newInstructions inst =
                    case id of
                        Just id_ ->
                            Dict.update id_ (Maybe.map updater) inst.deviceInstructions

                        Nothing ->
                            inst.deviceInstructions
            in
            AE.update index
                (\inst -> { inst | deviceInstructions = newInstructions inst })
                instructions

        updatePort : Port -> Bool -> PortsState -> PortsState
        updatePort p checked ports =
            let
                newState =
                    if checked then
                        Open

                    else
                        Close
            in
            case p of
                Port1 ->
                    { ports | port1 = newState }

                Port2 ->
                    { ports | port2 = newState }

                Port3 ->
                    { ports | port3 = newState }

                Port4 ->
                    { ports | port4 = newState }

                Port5 ->
                    { ports | port5 = newState }

                _ ->
                    -- Warning we do not allow manipulation of inlet/outlet ports here
                    ports
    in
    case msg of
        AddInstruction ->
            ( { model
                | instructions =
                    Array.push (createNewInstruction model.devices model.instructions) <| model.instructions
              }
            , Cmd.none
            )

        DeleteLastInstruction ->
            ( { model | instructions = Array.slice 0 -1 model.instructions }
            , Cmd.none
            )

        ResetInstructions ->
            ( { model | instructions = Array.empty }, Cmd.none )

        InstructionTimeChanged instructionIndex newValue ->
            case String.toInt newValue of
                Just newTime ->
                    ( { model
                        | instructions =
                            AE.update instructionIndex
                                (\instruction -> { instruction | time = MilliSeconds newTime })
                                model.instructions
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

        ActionChanged flowIODevice index flowIOAction ->
            ( { model
                | instructions =
                    updateDeviceInstruction index flowIODevice (\inst -> { inst | action = flowIOAction }) model.instructions
              }
            , Cmd.none
            )

        --( model, Cmd.none )
        PWMChanged flowIODevice index newValue ->
            case String.toInt newValue of
                Just newPwm ->
                    ( { model
                        | instructions = updateDeviceInstruction index flowIODevice (\inst -> { inst | pumpPwm = newPwm }) model.instructions
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

        PortStateChanged flowIODevice index port_ checked ->
            ( { model
                | instructions =
                    updateDeviceInstruction
                        index
                        flowIODevice
                        (\inst -> { inst | ports = updatePort port_ checked inst.ports })
                        model.instructions
              }
            , Cmd.none
            )

        DevicesChanged devices ->
            let
                newState =
                    case model.state of
                        RunningInstructions ->
                            RunningInstructions

                        _ ->
                            if Array.isEmpty devices then
                                Disabled

                            else
                                Editable
            in
            ( { model | devices = devices, state = newState }, Cmd.none )

        RunInstructions ->
            ( { model | state = RunningInstructions }, executeInstructions (encodeInstructions model.instructions) )

        DownloadInstructions ->
            ( model
            , File.Download.string
                "flowio-schedule.json"
                "application/json"
                (JE.encode 2 <| encodeInstructions model.instructions)
            )

        UploadInstructions ->
            ( model, File.Select.file [ "application/json" ] UploadSelected )

        UploadSelected file ->
            ( model, Task.perform FileRead <| File.toString file )

        FileRead content ->
            let
                result =
                    JD.decodeString instructionsDecoder content
            in
            case result of
                Ok instructions ->
                    ( { model | instructions = instructions }, Cmd.none )

                Err error ->
                    Debug.log (JD.errorToString error)
                        ( model, Cmd.none )

        DisabledFieldClicked _ ->
            ( model, Cmd.none )


encodeInstructions : Array SchedulerInstruction -> JE.Value
encodeInstructions instructions =
    JE.array
        (\inst ->
            JE.object
                [ ( "startTime", JE.string <| millisToString inst.time )
                , ( "instructions", JE.dict identity encodeCommand inst.deviceInstructions )
                ]
        )
        instructions


instructionsDecoder : JD.Decoder (Array SchedulerInstruction)
instructionsDecoder =
    JD.array <|
        JD.map2 (\startTime devices -> { time = MilliSeconds startTime, deviceInstructions = devices })
            (JD.field "startTime" JD.int)
            (JD.field "instructions" (JD.dict controlCommandDecoder))


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        []
