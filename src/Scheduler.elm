port module Scheduler exposing (Model, Msg(..), initModel, subscriptions, update, view)

import Array exposing (Array)
import Array.Extra as AE
import Browser
import Color.Dracula as Dracula
import Dict exposing (Dict)
import Dict.Extra
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
import Styles exposing (darkGrey, inflateIcon, lightGrey, releaseIcon, stopIcon, textField, vacuumIcon)
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


type alias RoleName =
    String


type alias RolesInstructions =
    Dict RoleName (Array FlowIOCommand)


type alias Instructions =
    { time : Array Time
    , instructions : RolesInstructions
    }


type alias Model =
    { state : SchedulerState
    , instructions : Instructions
    , devices : Array FlowIODevice

    -- roles are abstract names for devices
    , roles : Array RoleName
    , roleDeviceMapping : Dict RoleName String
    }


emptyInstructions =
    { time = Array.empty, instructions = Dict.empty }


initModel : Model
initModel =
    { state = Editable
    , instructions = emptyInstructions
    , devices = Array.empty
    , roles = Array.empty
    , roleDeviceMapping = Dict.empty
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
        , Font.color Dracula.white
        ]
        (El.column [ fullWidth, El.height <| El.fill ]
            [ header
            , devicesTable model
            , buttons model
            ]
        )


buttonPadding =
    El.paddingXY 12 4


header : El.Element Msg
header =
    El.row [ fullWidth, El.spacingXY 20 8 ]
        [ El.el [ El.width <| El.fillPortion 3, El.spaceEvenly, Font.bold, Font.size 18 ] <| El.text "Schedule"
        , Element.Input.button [ El.width <| fillPortion 1, externalClass "btn-scheduler", Font.color Dracula.white, buttonPadding ]
            { onPress = Just (AddRole "Role 1"), label = El.text "+ Add Role" }
        , Element.Input.button [ El.width <| fillPortion 1, externalClass "btn-scheduler", Font.color Dracula.white, buttonPadding ]
            { onPress = Just AddInstruction, label = El.text "+ Add Row" }
        , Element.Input.button [ El.width <| fillPortion 1, externalClass "btn-scheduler", Font.color Dracula.white, buttonPadding ]
            { onPress = Just DeleteLastInstruction, label = El.text "Delete" }
        , Element.Input.button [ El.width <| fillPortion 1, externalClass "btn-scheduler", Font.color Dracula.white, buttonPadding ]
            { onPress = Just ResetInstructions, label = El.text "Reset" }
        ]


getCommandForRole : RoleName -> Int -> RolesInstructions -> Maybe FlowIOCommand
getCommandForRole role index instructions =
    instructions
        |> Dict.get role
        |> Maybe.andThen (Array.get index)


cellHeight =
    El.height <| El.px 42


actionSelection : FlowIOCommand -> (FlowIOAction -> msg) -> El.Element msg
actionSelection command onChange =
    let
        renderOption : El.Element msg -> Element.Input.OptionState -> El.Element msg
        renderOption icon state =
            case state of
                Element.Input.Idle ->
                    El.el [ Background.color Dracula.red, Border.rounded 2, Border.width 1, Border.color Dracula.purple ] icon

                Element.Input.Focused ->
                    El.el
                        [ Background.color Dracula.red
                        , Border.rounded 2
                        , Border.width 2
                        , Border.color Dracula.pink
                        ]
                        icon

                Element.Input.Selected ->
                    El.el
                        [ Background.color Dracula.black
                        , Border.rounded 2
                        , Border.width 0
                        , externalClass "svg-red"
                        , El.behindContent <|
                            El.el [ El.width El.fill, El.height El.fill, Background.color Dracula.green, Border.rounded 15 ] El.none
                        ]
                        icon
    in
    Element.Input.radioRow
        [ Font.size 11
        , El.htmlAttribute <| Html.Attributes.style "flex-wrap" "wrap"
        , cellHeight
        , El.width <| El.px 224
        ]
        { options =
            [ Element.Input.optionWith Inflate <| renderOption inflateIcon
            , Element.Input.optionWith Vacuum <| renderOption vacuumIcon
            , Element.Input.optionWith Release <| renderOption releaseIcon
            , Element.Input.optionWith Stop <| renderOption stopIcon
            ]
        , onChange = onChange
        , selected = Just command.action
        , label = labelHidden "Action"
        }


pwmControl : FlowIOCommand -> Bool -> String -> (String -> Msg) -> El.Element Msg
pwmControl inst isDisabled label onChange =
    let
        numberAttrs =
            [ Html.Attributes.min "100"
            , Html.Attributes.max "255"
            , Html.Attributes.step "5"
            , Html.Attributes.type_ "number"
            ]
                |> List.map El.htmlAttribute
    in
    textField (numberAttrs ++ [ El.width <| El.maximum 60 (fillPortion 1), cellHeight ])
        { isDisabled = isDisabled
        , onChange = onChange
        , onChangeDisabled = DisabledFieldClicked ""
        , label = label
        , text = String.fromInt inst.pumpPwm
        , placeholder = Nothing
        }


portsSelection : FlowIOCommand -> (Port -> Bool -> msg) -> El.Element msg
portsSelection inst onPortChange =
    let
        checkBox checked =
            if checked then
                El.el
                    [ El.height <| El.px 16
                    , El.width <| El.px 16
                    , Border.color Dracula.white
                    , Background.color Dracula.gray
                    , Border.rounded 4
                    , El.behindContent <|
                        El.el
                            [ El.height <| El.px 14
                            , El.width <| El.px 14
                            , El.centerX
                            , El.centerY
                            , Border.rounded 7
                            , Border.color Dracula.yellow
                            , Background.color Dracula.green
                            ]
                            El.none
                    ]
                    El.none

            else
                El.el
                    [ El.height <| El.px 16
                    , El.width <| El.px 16
                    , Border.color Dracula.white
                    , Background.color Dracula.gray
                    , Border.rounded 4
                    ]
                    El.none
    in
    El.row [ El.padding 2, El.spacing 2 ]
        [ Element.Input.checkbox []
            { label = labelAbove [ Font.size 8 ] <| El.text "Port 1"
            , checked = inst.ports.port1 == Open
            , icon = checkBox
            , onChange = onPortChange Port1
            }
        , Element.Input.checkbox []
            { label = labelAbove [ Font.size 8 ] <| El.text "Port 2"
            , checked = inst.ports.port2 == Open
            , icon = checkBox
            , onChange = onPortChange Port2
            }
        , Element.Input.checkbox []
            { label = labelAbove [ Font.size 8 ] <| El.text "Port 3"
            , checked = inst.ports.port3 == Open
            , icon = checkBox
            , onChange = onPortChange Port3
            }
        , Element.Input.checkbox []
            { label = labelAbove [ Font.size 8 ] <| El.text "Port 4"
            , checked = inst.ports.port4 == Open
            , icon = checkBox
            , onChange = onPortChange Port4
            }
        , Element.Input.checkbox []
            { label = labelAbove [ Font.size 8 ] <| El.text "Port 5"
            , checked = inst.ports.port5 == Open
            , icon = checkBox
            , onChange = onPortChange Port5
            }
        ]


schedulerControls : RoleName -> SchedulerState -> Int -> FlowIOCommand -> El.Element Msg
schedulerControls role state rowIndex command =
    El.row (commonAttrs ++ [])
        [ actionSelection command (ActionChanged role rowIndex)
        , pwmControl command
            (state /= Editable)
            ("PWM " ++ role ++ " row " ++ String.fromInt rowIndex)
            (PWMChanged role rowIndex)
        , portsSelection command (PortStateChanged role rowIndex)
        ]


border =
    [ Border.width 1
    , Border.color <| El.rgb255 0xDD 0xDD 0xDD
    ]


commonAttrs =
    cellHeight :: border


cellWrapper content =
    El.el (El.padding 2 :: commonAttrs) content


schedulerRow : RoleName -> SchedulerState -> Int -> SchedulerRow -> El.Element Msg
schedulerRow role state index row =
    case row of
        ExistingInstruction _ dict ->
            let
                maybeCommand =
                    Dict.get role dict
            in
            case maybeCommand of
                Just command ->
                    schedulerControls role state index command

                Nothing ->
                    El.row commonAttrs [ El.text "actions", El.text "PWM", El.text "[] [] [] [] []", El.text "no cmd" ]

        PlannedInstruction ->
            El.row commonAttrs [ El.text "actions", El.text "PWM", El.text "[] [] [] [] []" ]


type SchedulerRow
    = ExistingInstruction Time (Dict RoleName FlowIOCommand)
    | PlannedInstruction


devicesTable : Model -> El.Element Msg
devicesTable model =
    let
        instructionsAt : Int -> RolesInstructions -> Dict RoleName FlowIOCommand
        instructionsAt index rolesInstructions =
            let
                last : Array a -> Maybe a
                last array =
                    Array.get (Array.length array - 1) array
            in
            Debug.log ("instructions at " ++ Debug.toString index)
                (rolesInstructions
                    |> Dict.map
                        (\_ commands ->
                            Array.get index commands
                                |> Maybe.withDefault defaultCommand
                        )
                )

        rows : List SchedulerRow
        rows =
            model.instructions.time
                |> Array.indexedMap
                    (\index time ->
                        ExistingInstruction
                            time
                            (instructionsAt index model.instructions.instructions)
                    )
                |> Array.push PlannedInstruction
                |> Array.toList

        maxTime =
            rows
                |> List.map
                    (\row ->
                        case row of
                            ExistingInstruction time _ ->
                                milliseconds time

                            _ ->
                                0
                    )
                |> List.maximum
                |> Maybe.map ((+) 10)
                |> Maybe.withDefault 0

        timeColumn : El.IndexedColumn SchedulerRow Msg
        timeColumn =
            { header = El.el ([] ++ border) <| El.text "Time (ms)"
            , width = El.maximum 120 (fillPortion 1)
            , view =
                \index row ->
                    cellWrapper <|
                        case row of
                            ExistingInstruction time _ ->
                                textField [ El.htmlAttribute <| Html.Attributes.type_ "number" ]
                                    { onChange = InstructionTimeChanged index
                                    , label = "Time at step " ++ String.fromInt index
                                    , text = millisToString time
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

        roleHeader : RoleName -> El.Element Msg
        roleHeader roleName =
            let
                device =
                    Dict.get roleName model.roleDeviceMapping
            in
            El.column (border ++ [ El.width El.fill ])
                [ El.el [ El.width El.fill ] <| El.text roleName
                , El.el [ El.width El.fill, Font.size 12 ] <| El.text (Maybe.withDefault "" device)
                ]

        roleColumn : RoleName -> El.IndexedColumn SchedulerRow Msg
        roleColumn roleName =
            { header = roleHeader roleName
            , width = El.maximum 300 <| fillPortion 2
            , view = schedulerRow roleName model.state
            }
    in
    indexedTable [ fullWidth, Font.size 11, El.padding 2 ]
        { data = rows
        , columns = timeColumn :: AE.mapToList roleColumn model.roles
        }


attrIfElse : Bool -> El.Attribute msg -> El.Attribute msg -> El.Attribute msg
attrIfElse condition ifTrue ifFalse =
    if condition then
        ifTrue

    else
        ifFalse


buttons model =
    El.row [ Font.color Dracula.white, El.centerX, El.spacingXY 8 20 ]
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
    | InstructionTimeChanged Int String
    | ActionChanged RoleName Int FlowIOAction
    | PWMChanged RoleName Int String
    | PortStateChanged RoleName Int Port Bool
    | RunInstructions
    | DownloadInstructions
    | UploadInstructions
    | UploadSelected File.File
    | FileRead String
    | DisabledFieldClicked String
    | AddRole String
    | RemoveRole Int
    | ReorderRole Int Int
    | RenameRole Int String
    | AssociateRoleToDevice RoleName (Maybe String)


createNewInstruction : Array RoleName -> Instructions -> Instructions
createNewInstruction roles { time, instructions } =
    let
        maxTime =
            time
                |> Array.map milliseconds
                |> Array.toList
                |> List.maximum
                |> Maybe.withDefault -1

        commandStop : FlowIOCommand
        commandStop =
            { defaultCommand
                | action = Stop
                , pumpPwm = 255
            }
    in
    { time = Array.push (MilliSeconds (maxTime + 1)) time
    , instructions =
        instructions
            |> Dict.map
                (\_ commands ->
                    Array.push commandStop commands
                )
    }


arrayMove : Int -> Int -> Array a -> Array a
arrayMove origin target arr =
    let
        item =
            Array.get origin arr

        withoutItem =
            AE.removeAt origin arr
    in
    item
        |> Maybe.map (\item_ -> AE.insertAt target item_ withoutItem)
        |> Maybe.withDefault arr


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        updateInstructionForRole :
            Int
            -> RoleName
            -> (FlowIOCommand -> FlowIOCommand)
            -> Instructions
            -> Instructions
        updateInstructionForRole index role updater instructions =
            let
                updateCommands : Maybe (Array FlowIOCommand) -> Maybe (Array FlowIOCommand)
                updateCommands =
                    Maybe.map (\commands -> AE.update index updater commands)
            in
            { time = instructions.time
            , instructions =
                instructions.instructions
                    |> Dict.update role updateCommands
            }

        deleteLastInstruction : Instructions -> Instructions
        deleteLastInstruction { time, instructions } =
            { time = AE.pop time
            , instructions = Dict.map (\_ commands -> AE.pop commands) instructions
            }

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
            ( { model | instructions = createNewInstruction model.roles model.instructions }
            , Cmd.none
            )

        DeleteLastInstruction ->
            ( { model | instructions = deleteLastInstruction model.instructions }
            , Cmd.none
            )

        ResetInstructions ->
            ( { model | instructions = emptyInstructions }, Cmd.none )

        InstructionTimeChanged instructionIndex newValue ->
            case String.toInt newValue of
                Just newTime ->
                    ( { model
                        | instructions =
                            { time =
                                AE.update instructionIndex (\_ -> MilliSeconds newTime) model.instructions.time
                            , instructions = model.instructions.instructions
                            }
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        ActionChanged flowIODevice index flowIOAction ->
            ( { model
                | instructions =
                    updateInstructionForRole index flowIODevice (\inst -> { inst | action = flowIOAction }) model.instructions
              }
            , Cmd.none
            )

        --( model, Cmd.none )
        PWMChanged flowIODevice index newValue ->
            case String.toInt newValue of
                Just newPwm ->
                    ( { model
                        | instructions =
                            updateInstructionForRole
                                index
                                flowIODevice
                                (\inst -> { inst | pumpPwm = newPwm })
                                model.instructions
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

        PortStateChanged flowIODevice index port_ checked ->
            ( { model
                | instructions =
                    updateInstructionForRole
                        index
                        flowIODevice
                        (\inst -> { inst | ports = updatePort port_ checked inst.ports })
                        model.instructions
              }
            , Cmd.none
            )

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

        AddRole roleName ->
            ( { model
                | roles = Array.push roleName model.roles
                , instructions =
                    { time = model.instructions.time
                    , instructions =
                        Dict.insert roleName
                            (Array.repeat (Array.length model.instructions.time) defaultCommand)
                            model.instructions.instructions
                    }
              }
            , Cmd.none
            )

        RemoveRole index ->
            -- TODO: Remove role from instructions as well
            ( { model | roles = AE.removeAt index model.roles }, Cmd.none )

        ReorderRole originalIndex newIndex ->
            ( { model | roles = arrayMove originalIndex newIndex model.roles }, Cmd.none )

        AssociateRoleToDevice roleName maybeDeviceId ->
            ( { model | roleDeviceMapping = Dict.update roleName (\_ -> maybeDeviceId) model.roleDeviceMapping }
            , Cmd.none
            )

        RenameRole index newName ->
            let
                maybeOldRole =
                    Array.get index model.roles

                replaceKey oldKey =
                    Dict.Extra.mapKeys
                        (\k ->
                            if k == oldKey then
                                newName

                            else
                                k
                        )
            in
            case maybeOldRole of
                Just oldRole ->
                    ( { model
                        | roles = Array.set index newName model.roles
                        , roleDeviceMapping = replaceKey oldRole model.roleDeviceMapping
                        , instructions =
                            { time = model.instructions.time
                            , instructions = replaceKey oldRole model.instructions.instructions
                            }
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )


encodeInstructions : Instructions -> JE.Value
encodeInstructions instructions =
    JE.object
        [ ( "time", JE.array (\(MilliSeconds ms) -> JE.int ms) instructions.time )
        , ( "instructions", JE.dict identity (JE.array encodeCommand) instructions.instructions )
        ]


instructionsDecoder : JD.Decoder Instructions
instructionsDecoder =
    JD.map2 (\timeArray instructions -> { time = timeArray, instructions = instructions })
        (JD.field "time" <| JD.array <| (JD.int |> JD.map MilliSeconds))
        (JD.field "instructions" (JD.dict <| JD.array controlCommandDecoder))


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        []
