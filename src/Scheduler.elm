module Scheduler exposing (Model, Msg(..), initModel, subscriptions, update, view)

import Array exposing (Array)
import Array.Extra as AE
import Browser
import Color.Dracula as Dracula
import Dict exposing (Dict)
import Dict.Extra
import Element as El exposing (fillPortion, indexedTable)
import Element.Background as Background
import Element.Border as Border
import Element.Events
import Element.Font as Font
import Element.Input exposing (labelAbove, labelHidden)
import Element.Region as Region
import File
import File.Download
import File.Select
import FlowIO exposing (..)
import Html exposing (Html)
import Html.Attributes
import Json.Decode as JD
import Json.Encode as JE
import Styles exposing (externalClass, fullWidth, inflateIcon, releaseIcon, stopIcon, textField, vacuumIcon)
import Task
import Time exposing (Posix)


type SchedulerState
    = Paused { stoppedTime : Posix, commandIndex : Int }
    | Stopped
    | RunningInstructions { startTime : Posix, commandIndex : Int }


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


type RoleEditState
    = NotEditing
    | Editing Int


type RoleDeviceSelectState
    = SelectionClosed
    | SelectionOpen Int


type alias Model =
    { state : SchedulerState
    , instructions : Instructions
    , devices : Array FlowIODevice

    -- roles are abstract names for devices
    , roles : Array RoleName
    , roleDeviceMapping : Dict RoleName String
    , display :
        { roleEditing : RoleEditState
        , roleDeviceSelection : RoleDeviceSelectState
        }
    }


emptyInstructions =
    { time = Array.empty, instructions = Dict.empty }


initModel : Model
initModel =
    { state = Stopped
    , instructions = emptyInstructions
    , devices = Array.empty
    , roles = Array.empty
    , roleDeviceMapping = Dict.empty
    , display =
        { roleEditing = NotEditing
        , roleDeviceSelection = SelectionClosed
        }
    }


view : Model -> El.Element Msg
view model =
    El.el
        [ Font.family [ Font.typeface "Overpass", Font.typeface "Open Sans", Font.typeface "Helvetica", Font.sansSerif ]
        , Font.size 15
        , Font.color Dracula.white
        ]
        (El.column [ fullWidth, El.height <| El.fill ]
            [ header model
            , devicesTable model
            , buttons model
            ]
        )


buttonPadding =
    El.paddingXY 12 4


header : Model -> El.Element Msg
header { roles } =
    let
        nextRoleName =
            roles
                |> Array.toList
                |> List.filter (String.startsWith "Role ")
                |> List.map (String.split " ")
                |> List.filterMap (List.filterMap String.toInt >> List.head)
                |> List.maximum
                |> Maybe.map ((+) 1)
                |> Maybe.withDefault 1
                |> String.fromInt
                >> (++) "Role "
    in
    El.row [ fullWidth, El.spacingXY 20 8 ]
        [ El.el [ El.width <| El.fillPortion 3, El.spaceEvenly, Font.bold, Font.size 18 ] <| El.text "Schedule"
        , Element.Input.button [ El.width <| fillPortion 1, externalClass "btn-scheduler", Font.color Dracula.white, buttonPadding ]
            { onPress = Just (AddRole nextRoleName), label = El.text "+ Add Role" }
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
                    El.el
                        [ El.centerY
                        , Background.color Dracula.red
                        , Border.rounded 2
                        , Border.width 1
                        , Border.color Dracula.purple
                        ]
                        icon

                Element.Input.Focused ->
                    El.el
                        [ Background.color Dracula.red
                        , Border.rounded 2
                        , Border.width 2
                        , Border.color Dracula.pink
                        , El.centerY
                        ]
                        icon

                Element.Input.Selected ->
                    El.el
                        [ Background.color Dracula.black
                        , Border.rounded 2
                        , Border.width 0
                        , externalClass "svg-red"
                        , El.centerY
                        , El.behindContent <|
                            El.el
                                [ El.width El.fill
                                , El.height El.fill
                                , Background.color Dracula.green
                                , Border.rounded 15
                                ]
                                El.none
                        ]
                        icon
    in
    Element.Input.radioRow
        [ Font.size 11
        , El.htmlAttribute <| Html.Attributes.style "flex-wrap" "wrap"
        , cellHeight
        , El.width <| El.minimum (34 * 4) <| El.fillPortion 3
        , El.centerY
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
    textField (numberAttrs ++ [ El.width <| El.maximum 60 (fillPortion 1), El.centerY ])
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
            , checked = inst.ports.port1 == PortOpen
            , icon = checkBox
            , onChange = onPortChange Port1
            }
        , Element.Input.checkbox []
            { label = labelAbove [ Font.size 8 ] <| El.text "Port 2"
            , checked = inst.ports.port2 == PortOpen
            , icon = checkBox
            , onChange = onPortChange Port2
            }
        , Element.Input.checkbox []
            { label = labelAbove [ Font.size 8 ] <| El.text "Port 3"
            , checked = inst.ports.port3 == PortOpen
            , icon = checkBox
            , onChange = onPortChange Port3
            }
        , Element.Input.checkbox []
            { label = labelAbove [ Font.size 8 ] <| El.text "Port 4"
            , checked = inst.ports.port4 == PortOpen
            , icon = checkBox
            , onChange = onPortChange Port4
            }
        , Element.Input.checkbox []
            { label = labelAbove [ Font.size 8 ] <| El.text "Port 5"
            , checked = inst.ports.port5 == PortOpen
            , icon = checkBox
            , onChange = onPortChange Port5
            }
        ]


schedulerControls : RoleName -> SchedulerState -> Int -> FlowIOCommand -> El.Element Msg
schedulerControls role state rowIndex command =
    let
        currentlyRunningRow =
            case state of
                Paused { commandIndex } ->
                    commandIndex == rowIndex

                Stopped ->
                    False

                RunningInstructions { commandIndex } ->
                    commandIndex == rowIndex

        borderEffect =
            if currentlyRunningRow then
                Border.color Dracula.green

            else
                Border.color Dracula.white
    in
    El.row [ cellHeight, Border.width 1, borderEffect ]
        [ actionSelection command (ActionChanged role rowIndex)
        , pwmControl command
            (state /= Stopped)
            ("PWM " ++ role ++ " row " ++ String.fromInt rowIndex)
            (PWMChanged role rowIndex)
        , portsSelection command (PortStateChanged role rowIndex)
        ]


border =
    [ Border.width 1
    , Border.color <| Dracula.white
    ]


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
                    El.row (cellHeight :: border)
                        [ El.text "actions"
                        , El.text "PWM"
                        , El.text "[] [] [] [] []"
                        , El.text
                            "no cmd"
                        ]

        PlannedInstruction ->
            El.row (cellHeight :: border) [ El.text "actions", El.text "PWM", El.text "[] [] [] [] []" ]


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
            rolesInstructions
                |> Dict.map
                    (\_ commands ->
                        Array.get index commands
                            |> Maybe.withDefault defaultCommand
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

        cellWrapper borderColor content =
            El.el [ El.padding 2, cellHeight, Border.width 1, borderColor ] content

        timeColumn : El.IndexedColumn SchedulerRow Msg
        timeColumn =
            { header = El.el (El.height El.fill :: El.padding 4 :: border) <| El.text "Time (ms)"
            , width = El.maximum 80 (fillPortion 1)
            , view =
                \index row ->
                    let
                        currentlyRunningRow =
                            case model.state of
                                Paused { commandIndex } ->
                                    commandIndex == index

                                Stopped ->
                                    False

                                RunningInstructions { commandIndex } ->
                                    commandIndex == index

                        currentlyRunningBorderColor =
                            if currentlyRunningRow then
                                Border.color Dracula.green

                            else
                                Border.color Dracula.white
                    in
                    cellWrapper currentlyRunningBorderColor <|
                        case row of
                            ExistingInstruction time _ ->
                                let
                                    maybePrevious =
                                        Array.get (index - 1) model.instructions.time

                                    maybeNext =
                                        Array.get (index + 1) model.instructions.time

                                    isCorrectlyOrdered =
                                        case ( maybePrevious, time, maybeNext ) of
                                            ( Just (MilliSeconds previous), MilliSeconds current, Just (MilliSeconds next) ) ->
                                                previous < current && current < next

                                            ( Nothing, MilliSeconds current, Just (MilliSeconds next) ) ->
                                                current < next

                                            ( Just (MilliSeconds previous), MilliSeconds current, Nothing ) ->
                                                previous < current

                                            ( Nothing, MilliSeconds current, Nothing ) ->
                                                True

                                    errorAttributes =
                                        if isCorrectlyOrdered then
                                            []

                                        else
                                            [ El.behindContent <|
                                                El.el
                                                    [ El.width El.fill
                                                    , El.height El.fill
                                                    , Border.widthEach { bottom = 4, left = 0, right = 0, top = 0 }
                                                    , Border.color Dracula.red
                                                    ]
                                                    El.none
                                            ]
                                in
                                textField
                                    ([ El.centerY
                                     , El.htmlAttribute <| Html.Attributes.type_ "number"
                                     , El.width <| El.px 80
                                     ]
                                        ++ errorAttributes
                                    )
                                    { onChange = InstructionTimeChanged index
                                    , label = "Time at step " ++ String.fromInt index
                                    , text = millisToString time
                                    , placeholder = Just <| Element.Input.placeholder [] <| El.text "0"
                                    , isDisabled = model.state /= Stopped
                                    , onChangeDisabled = DisabledFieldClicked "Time column disabled"
                                    }

                            PlannedInstruction ->
                                textField [ El.centerY, El.width <| El.px 80 ]
                                    { onChange = InstructionTimeChanged index
                                    , label = "Time at step " ++ String.fromInt index
                                    , text = String.fromInt maxTime
                                    , placeholder = Just <| Element.Input.placeholder [] <| El.text <| String.fromInt maxTime
                                    , isDisabled = True
                                    , onChangeDisabled = AddInstruction
                                    }
            }

        roleHeader : Int -> RoleName -> El.Element Msg
        roleHeader roleIndex roleName =
            let
                device =
                    Dict.get roleName model.roleDeviceMapping

                isBeingEdit =
                    model.display.roleEditing == Editing roleIndex

                isDuplicate =
                    Array.filter ((==) roleName) model.roles |> Array.length |> (\count -> count > 1)

                duplicateError =
                    if isDuplicate then
                        [ El.behindContent <|
                            El.el
                                [ fullWidth
                                , El.height El.fill
                                , Border.widthEach { bottom = 4, left = 0, right = 0, top = 0 }
                                , Border.color Dracula.red
                                ]
                                El.none
                        ]

                    else
                        []

                namePart =
                    El.row [ fullWidth ] <|
                        if isBeingEdit then
                            [ Element.Input.text
                                (duplicateError
                                    ++ [ fullWidth
                                       , El.padding 2
                                       , El.spacingXY 2 4
                                       , Background.color Dracula.black
                                       , Element.Events.onLoseFocus EndRoleEditing
                                       ]
                                )
                                { onChange = RenameRole roleIndex
                                , text = roleName
                                , placeholder = Nothing
                                , label = Element.Input.labelHidden "Role name"
                                }
                            , Element.Input.button [ El.alignRight, El.padding 2 ]
                                { onPress = Just <| EndRoleEditing
                                , label = El.el [ Region.description "End editing" ] <| El.text "✓"
                                }
                            ]

                        else
                            [ El.el (duplicateError ++ [ El.padding 2, El.width El.fill ]) <| El.text roleName
                            , Element.Input.button [ El.alignRight, El.padding 2 ]
                                { onPress = Just <| EditRole roleIndex
                                , label = El.el [ Region.description "Rename" ] <| El.text "✎"
                                }
                            ]

                deviceSelection =
                    let
                        deviceIds : Array DeviceId
                        deviceIds =
                            AE.filterMap (.details >> Maybe.map .id) model.devices

                        devicesList : El.Element Msg
                        devicesList =
                            El.column
                                [ fullWidth
                                , El.padding 2
                                , El.spacing 2
                                , Background.color Dracula.gray
                                ]
                            <|
                                AE.mapToList
                                    (\id ->
                                        Element.Input.button
                                            [ El.padding 2
                                            , Border.widthEach
                                                { bottom = 1
                                                , top = 0
                                                , left = 0
                                                , right = 0
                                                }
                                            ]
                                            { onPress = Just <| AssociateRoleToDevice roleName (Just id)
                                            , label = El.text id
                                            }
                                    )
                                    deviceIds
                    in
                    El.row [ fullWidth, Font.size 12 ] <|
                        if model.display.roleDeviceSelection == SelectionOpen roleIndex then
                            [ El.el
                                [ El.below <| devicesList
                                , fullWidth
                                ]
                              <|
                                El.text (device |> Maybe.withDefault "")
                            , Element.Input.button [ El.alignRight, El.padding 2 ]
                                { onPress = Just <| ChangeDeviceSelection SelectionClosed
                                , label = El.text "⌃"
                                }
                            ]

                        else
                            [ El.el [ fullWidth ] <| El.text (device |> Maybe.withDefault "")
                            , Element.Input.button [ El.alignRight, El.padding 2 ]
                                { onPress = Just <| ChangeDeviceSelection <| SelectionOpen roleIndex
                                , label = El.text "⌄"
                                }
                            ]
            in
            El.column (border ++ [ El.width El.fill, El.height El.fill ])
                [ namePart
                , deviceSelection
                ]

        roleColumn : Int -> RoleName -> El.IndexedColumn SchedulerRow Msg
        roleColumn roleIndex roleName =
            { header = roleHeader roleIndex roleName
            , width = El.maximum 300 <| fillPortion 2
            , view = schedulerRow roleName model.state
            }
    in
    indexedTable
        [ El.width <| El.maximum 1060 El.fill
        , El.height <| El.minimum 400 (El.fillPortion 10)
        , El.scrollbarY
        , Font.size 11
        , El.padding 2
        ]
        { data = rows
        , columns = timeColumn :: AE.indexedMapToList roleColumn model.roles
        }


attrIfElse : Bool -> El.Attribute msg -> El.Attribute msg -> El.Attribute msg
attrIfElse condition ifTrue ifFalse =
    if condition then
        ifTrue

    else
        ifFalse


buttons : Model -> El.Element Msg
buttons model =
    let
        numRolesAssociated =
            model.roles
                |> Array.map (\role -> model.roleDeviceMapping |> Dict.member role)
                |> Array.length

        allRolesAssociated =
            numRolesAssociated == (model.roles |> Array.length)
    in
    El.row [ Font.color Dracula.white, El.centerX, El.spacingXY 8 20 ]
        [ Element.Input.button
            [ externalClass "btn-scheduler"
            , buttonPadding
            , Background.color <| Dracula.green
            , attrIfElse (model.state /= Stopped) (El.alpha 0.4) (El.alpha 1.0)
            ]
            { label = El.text "▶️ Run"
            , onPress =
                if model.state == Stopped && numRolesAssociated > 0 then
                    Just RunInstructions

                else
                    Nothing
            }
        , Element.Input.button [ externalClass "btn-scheduler", El.paddingXY 12 4 ]
            { label = El.text "Save"
            , onPress = Just DownloadInstructions
            }
        , Element.Input.button [ externalClass "btn-scheduler", El.paddingXY 12 4 ]
            { label = El.text "Upload"
            , onPress = Just UploadInstructions
            }
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
    | StartInstructions Posix
    | StopInstructions
    | DownloadInstructions
    | UploadInstructions
    | UploadSelected File.File
    | FileRead String
    | DisabledFieldClicked String
    | AddRole String
    | RemoveRole Int
    | ReorderRole Int Int
    | RenameRole Int String
    | EditRole Int
    | EndRoleEditing
    | AssociateRoleToDevice RoleName (Maybe String)
    | ChangeDeviceSelection RoleDeviceSelectState
    | Tick Posix


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
                        PortOpen

                    else
                        PortClosed
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
            let
                updateNewTime newTime =
                    ( { model
                        | instructions =
                            { time =
                                AE.update instructionIndex (\_ -> MilliSeconds newTime) model.instructions.time
                            , instructions = model.instructions.instructions
                            }
                      }
                    , Cmd.none
                    )
            in
            case String.toInt newValue of
                Just newTime ->
                    updateNewTime newTime

                _ ->
                    if String.isEmpty newValue then
                        updateNewTime 0

                    else
                        ( model, Cmd.none )

        ActionChanged role index flowIOAction ->
            ( { model
                | instructions =
                    updateInstructionForRole index role (\inst -> { inst | action = flowIOAction }) model.instructions
              }
            , Cmd.none
            )

        PWMChanged role index newValue ->
            case String.toInt newValue of
                Just newPwm ->
                    ( { model
                        | instructions =
                            updateInstructionForRole
                                index
                                role
                                (\inst -> { inst | pumpPwm = newPwm })
                                model.instructions
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

        PortStateChanged role index port_ checked ->
            ( { model
                | instructions =
                    updateInstructionForRole
                        index
                        role
                        (\inst -> { inst | ports = updatePort port_ checked inst.ports })
                        model.instructions
              }
            , Cmd.none
            )

        RunInstructions ->
            ( model, Task.perform StartInstructions Time.now )

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
                    ( { model
                        | instructions = instructions
                        , roles = Dict.keys instructions.instructions |> Array.fromList
                      }
                    , Cmd.none
                    )

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
            let
                display =
                    model.display
            in
            ( { model
                | roleDeviceMapping = Dict.update roleName (\_ -> maybeDeviceId) model.roleDeviceMapping
                , display = { display | roleDeviceSelection = SelectionClosed }
              }
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

        EditRole roleIndex ->
            let
                display =
                    model.display
            in
            ( { model | display = { display | roleEditing = Editing roleIndex } }, Cmd.none )

        EndRoleEditing ->
            let
                display =
                    model.display
            in
            ( { model | display = { display | roleEditing = NotEditing } }, Cmd.none )

        ChangeDeviceSelection roleDeviceSelectState ->
            let
                display =
                    model.display
            in
            ( { model | display = { display | roleDeviceSelection = roleDeviceSelectState } }, Cmd.none )

        StopInstructions ->
            ( { model | state = Stopped }, Cmd.none )

        Tick posix ->
            case model.state of
                Paused record ->
                    Debug.todo "Pausing not implemented yet"

                Stopped ->
                    ( model, Cmd.none )

                RunningInstructions { startTime, commandIndex } ->
                    let
                        elapsedTime =
                            Time.posixToMillis posix - Time.posixToMillis startTime

                        currentCommandTime =
                            Array.get commandIndex model.instructions.time
                    in
                    case currentCommandTime of
                        Just (MilliSeconds currentTime) ->
                            if elapsedTime >= currentTime then
                                -- We need to run the command and update the index
                                let
                                    instructions : RolesInstructions
                                    instructions =
                                        model.instructions.instructions

                                    deviceIdToIdx : Dict DeviceId Int
                                    deviceIdToIdx =
                                        model.devices
                                            |> Array.toIndexedList
                                            |> List.filterMap
                                                (\( idx, device ) ->
                                                    device.details
                                                        |> Maybe.map (\details -> ( details.id, idx ))
                                                )
                                            |> Dict.fromList

                                    findDeviceId : ( RoleName, b ) -> Maybe ( DeviceId, b )
                                    findDeviceId ( role, commandsArray ) =
                                        Dict.get role model.roleDeviceMapping
                                            |> Maybe.map (\deviceId -> ( deviceId, commandsArray ))

                                    mapDeviceIdToDeviceIndex : ( DeviceId, b ) -> Maybe ( Int, b )
                                    mapDeviceIdToDeviceIndex ( deviceId, commandsArray ) =
                                        Dict.get deviceId deviceIdToIdx
                                            |> Maybe.map (\idx -> ( idx, commandsArray ))

                                    extractCommandAtIndex : ( a, Array b ) -> Maybe ( a, b )
                                    extractCommandAtIndex ( deviceIdx, commandsArray ) =
                                        Array.get commandIndex
                                            commandsArray
                                            |> Maybe.map (\cmd -> ( deviceIdx, cmd ))

                                    createCommand : ( Int, FlowIOCommand ) -> Cmd msg
                                    createCommand ( deviceIdx, command ) =
                                        sendCommand
                                            { deviceIndex = deviceIdx
                                            , command =
                                                encodeCommand command
                                            }

                                    commands : List (Cmd Msg)
                                    commands =
                                        instructions
                                            |> Dict.toList
                                            |> List.filterMap findDeviceId
                                            |> List.filterMap mapDeviceIdToDeviceIndex
                                            |> List.filterMap extractCommandAtIndex
                                            |> List.map createCommand
                                in
                                ( { model | state = RunningInstructions { startTime = startTime, commandIndex = commandIndex + 1 } }
                                , Cmd.batch
                                    commands
                                )

                            else
                                -- Do nothing, wait for the next clock tick
                                ( model, Cmd.none )

                        Nothing ->
                            -- There is no next command, so we are done!
                            ( { model | state = Stopped }, Cmd.none )

        StartInstructions posix ->
            ( { model | state = RunningInstructions { startTime = posix, commandIndex = 0 } }, Cmd.none )


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


defaultTickIntervalMilliSeconds =
    5


subscriptions : Model -> Sub Msg
subscriptions { state } =
    let
        runInstructionsSub =
            case state of
                Paused _ ->
                    Sub.none

                Stopped ->
                    Sub.none

                RunningInstructions _ ->
                    Time.every defaultTickIntervalMilliSeconds Tick
    in
    Sub.batch
        [ runInstructionsSub
        ]
