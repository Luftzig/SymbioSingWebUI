module Composer.Sequencer exposing
    ( Model
    , OutgoingMsg(..)
    , init
    , send
    , subscriptions
    , update
    , view
    , viewDialog
    )

import Array exposing (Array)
import Color.Dracula as Dracula
import Dict as Dict exposing (Dict)
import Dict.Extra as Dict
import Element exposing (Element, alignBottom, alignLeft, alignRight, alignTop, centerX, centerY, column, el, fill, fillPortion, height, htmlAttribute, mouseOver, none, padding, paddingXY, paragraph, row, scrollbarY, spaceEvenly, spacing, text, width, wrappedRow)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input exposing (button, checkbox, labelHidden)
import Element.Region as Region
import Extra.Array as Array
import Extra.Dict as Dict
import Extra.TypedTime as TypedTime exposing (TypedTime)
import File exposing (File)
import File.Select
import FlowIO exposing (Command, Device, DeviceId, encodeCommand, translateActionInCommand)
import Html.Attributes
import Images
import Json.Decode as JD
import List.Extra as List
import LocalStorage
import Messages exposing (..)
import Scheduler exposing (instructionsDecoder)
import Set exposing (Set)
import Styles exposing (fullWidth)
import Task
import Time exposing (Posix)


type alias Model =
    { availableParts : Dict String Instructions
    , sequence : List ( String, Instructions )
    , roleAssignments : Dict RoleName (Set DeviceId)
    , availableDevices : Array Device
    , instructionsStorage : Set String
    , dialog : DialogStatus
    , showPartDetails : Dict String Collapsable
    , runStatus : RunStatus
    }


type Collapsable
    = Collapsed
    | Expanded


toggleCollapsable : Collapsable -> Collapsable
toggleCollapsable collapsable =
    case collapsable of
        Collapsed ->
            Expanded

        Expanded ->
            Collapsed


type DialogStatus
    = NoDialog
    | LoadFromStorageDialog
    | SaveSequenceDialog String
    | AssignRoleDialog String


type RunStatus
    = NotRunning
    | Running { startTime : TypedTime, nextCommands : List CommandsEntry, elapsedTime : TypedTime }


init : Model
init =
    { availableParts = Dict.empty
    , sequence = []
    , roleAssignments = Dict.empty
    , availableDevices = Array.empty
    , instructionsStorage = Set.empty
    , dialog = NoDialog
    , showPartDetails = Dict.empty
    , runStatus = NotRunning
    }


send : SequencerIncomingMsg -> SequencerMsg
send =
    ReceivedMessage


type OutgoingMsg
    = NoMessage
    | GetInstructionFromScheduler
    | GetInstructionFromConverter
    | LogError String
    | ShowDialog
    | HideDialog


subscriptions : Model -> Sub SequencerMsg
subscriptions { runStatus } =
    case runStatus of
        NotRunning ->
            Sub.none

        Running _ ->
            Time.every 5 SequencerTick


update : SequencerMsg -> Model -> ( Model, OutgoingMsg, Cmd SequencerMsg )
update msg model =
    case msg of
        ReceivedMessage incomingMsg ->
            case incomingMsg of
                DevicesChanged array ->
                    ( { model | availableDevices = array }, NoMessage, Cmd.none )

                ReceivedNewPart string instructions ->
                    ( { model | availableParts = Dict.insert string instructions model.availableParts }
                    , NoMessage
                    , Cmd.none
                    )

                BackdropClicked ->
                    ( { model | dialog = NoDialog }, NoMessage, Cmd.none )

        RequestLoadFromScheduler ->
            ( model, GetInstructionFromScheduler, Cmd.none )

        RequestFromConverter ->
            ( model, GetInstructionFromConverter, Cmd.none )

        LoadFromStorageRequested ->
            --( { model | dialog = LoadFromStorageDialog }, ShowDialog, Cmd.none )
            ( model, LogError "Loading schedule from local storage not supported yet", Cmd.none )

        LoadFromStorage key ->
            ( model, NoMessage, LocalStorage.load key )

        RequestUploadInstructions ->
            ( model, NoMessage, File.Select.file [ "application/json" ] InstructionsFileSelected )

        InstructionsFileSelected file ->
            ( model, NoMessage, Task.perform (InstructionFileRead (File.name file)) <| File.toString file )

        InstructionFileRead filename content ->
            let
                result =
                    JD.decodeString instructionsDecoder content
            in
            case result of
                Ok instructions ->
                    ( { model | availableParts = Dict.insert filename instructions model.availableParts }
                    , NoMessage
                    , Cmd.none
                    )

                Err error ->
                    ( model, LogError (JD.errorToString error), Cmd.none )

        OpenSaveSequenceDialog ->
            ( { model | dialog = SaveSequenceDialog "" }, ShowDialog, Cmd.none )

        DownloadSequence ->
            ( model, LogError "Download sequence not supported yet", Cmd.none )

        RequestSequenceUpload ->
            ( model, LogError "Uploading sequences not supported yet", Cmd.none )

        CloseDialog ->
            ( { model | dialog = NoDialog }, HideDialog, Cmd.none )

        AddPartToSequence partName ->
            let
                part =
                    Dict.get partName model.availableParts

                newSequence =
                    case part of
                        Just instructions ->
                            model.sequence ++ [ ( partName, instructions ) ]

                        Nothing ->
                            model.sequence

                partRoles =
                    part
                        |> Maybe.map .instructions
                        |> Maybe.map Dict.keys
                        |> Maybe.withDefault []
                        |> List.filterNot String.isEmpty

                newRoles =
                    Dict.addKeys Set.empty partRoles model.roleAssignments
            in
            ( { model
                | sequence = newSequence
                , roleAssignments = newRoles
              }
            , NoMessage
            , Cmd.none
            )

        TogglePartDetails partName ->
            let
                newDetails =
                    Dict.update partName
                        (\oldState ->
                            oldState
                                |> Maybe.withDefault Collapsed
                                |> toggleCollapsable
                                |> Just
                        )
                        model.showPartDetails
            in
            ( { model | showPartDetails = newDetails }, NoMessage, Cmd.none )

        RemoveFromSequence index ->
            let
                newSequence =
                    List.removeAt index model.sequence

                newRoles =
                    newSequence
                        |> List.concatMap (Tuple.second >> .instructions >> Dict.keys)
                        |> Set.fromList

                newAssignments =
                    Dict.keepOnly newRoles model.roleAssignments
            in
            ( { model | sequence = newSequence, roleAssignments = newAssignments }, NoMessage, Cmd.none )

        MovePart partIndex moveDirection ->
            let
                newSequence =
                    case moveDirection of
                        MoveUp ->
                            List.swapAt partIndex (partIndex - 1) model.sequence

                        MoveDown ->
                            List.swapAt partIndex (partIndex + 1) model.sequence
            in
            ( { model | sequence = newSequence }, NoMessage, Cmd.none )

        RoleClicked roleName ->
            ( { model | dialog = AssignRoleDialog roleName }, ShowDialog, Cmd.none )

        DeviceAssignmentChanged role device isSelected ->
            let
                assignments =
                    model.roleAssignments
                        |> Dict.get role
                        |> Maybe.withDefault Set.empty

                deviceId =
                    device.details |> Maybe.map .id

                newAssignments =
                    case deviceId of
                        Nothing ->
                            assignments

                        Just id ->
                            if isSelected then
                                assignments |> Set.insert id

                            else
                                assignments |> Set.remove id

                newRoles =
                    model.roleAssignments |> Dict.insert role newAssignments
            in
            ( { model | roleAssignments = newRoles }, NoMessage, Cmd.none )

        PlaySequenceRequested ->
            let
                devices =
                    model.availableDevices |> Array.toList

                commands : List CommandsEntry
                commands =
                    model.sequence
                        |> List.mapAccuml transformTimes TypedTime.zero
                        |> Tuple.second
                        |> List.concatMap toCommandsWithRoles
                        |> List.map toCommandsWithDevice

                {- todo: when we combine two instruction sets, the second starts with time 0, then we have
                   have two entries with the same start time, and either both will be executed (the second
                   with 1 tick delay) or the second would be skipped. Neither is good. We should remove the
                   first one of the unequal pair.
                   --|> deduplicate
                -}
                transformTimes : TypedTime -> ( String, Instructions ) -> ( TypedTime, Instructions )
                transformTimes accTime ( _, instructions ) =
                    let
                        lastTime =
                            instructions.time |> Array.last |> Maybe.withDefault TypedTime.zero

                        updatedTime =
                            instructions.time
                                |> Array.map (TypedTime.add accTime)
                    in
                    ( lastTime |> TypedTime.add accTime
                    , { instructions | time = updatedTime }
                    )

                toCommandsWithRoles : Instructions -> List ( TypedTime, List ( RoleName, Command ) )
                toCommandsWithRoles instructions =
                    let
                        transpose : Dict RoleName (Array Command) -> List (List ( RoleName, Command ))
                        transpose dict =
                            dict
                                |> Dict.map (\k arr -> arr |> Array.map (\c -> ( k, c )) |> Array.toList)
                                |> Dict.values
                    in
                    List.zip (instructions.time |> Array.toList)
                        (instructions.instructions |> transpose)

                toCommandsWithDevice : ( TypedTime, List ( RoleName, Command ) ) -> CommandsEntry
                toCommandsWithDevice ( t, roleCommands ) =
                    { startTime = t
                    , commands = List.concatMap mapDevices roleCommands
                    }

                mapDevices :
                    ( RoleName, Command )
                    -> List { device : Device, deviceIndex : Int, command : Command }
                mapDevices ( roleName, flowIOCommand ) =
                    let
                        assignedDevices =
                            model.roleAssignments
                                |> Dict.get roleName
                                |> Maybe.withDefault Set.empty

                        getDeviceAndIndex : DeviceId -> Maybe ( Int, Device )
                        getDeviceAndIndex deviceId =
                            devices
                                |> List.indexedMap Tuple.pair
                                |> List.find
                                    (\( _, device ) ->
                                        device.details
                                            |> Maybe.map .id
                                            |> (==) (Just deviceId)
                                    )

                        assignedCommands =
                            assignedDevices
                                |> Set.toList
                                |> List.filterMap
                                    (\deviceId ->
                                        getDeviceAndIndex deviceId
                                            |> Maybe.map
                                                (\( index, device ) ->
                                                    { device = device, deviceIndex = index, command = flowIOCommand }
                                                )
                                    )
                    in
                    assignedCommands
            in
            ( model, NoMessage, Time.now |> Task.perform (PlaySequenceStarted commands) )

        PlaySequenceStarted commands posix ->
            ( { model
                | runStatus =
                    Running
                        { startTime = TypedTime.fromPosix posix
                        , nextCommands = commands
                        , elapsedTime = TypedTime.zero
                        }
              }
            , NoMessage
            , Cmd.none
            )

        PlaySequenceStopped ->
            ( { model | runStatus = NotRunning }, NoMessage, Cmd.none )

        SequencerTick tickTime ->
            case model.runStatus of
                NotRunning ->
                    ( model, NoMessage, Cmd.none )

                Running runState ->
                    case runState.nextCommands of
                        entry :: rest ->
                            let
                                runningTime =
                                    TypedTime.fromPosix tickTime |> TypedTime.subtract runState.startTime

                                ( nextCommands, cmds ) =
                                    if runningTime |> TypedTime.greaterEqual entry.startTime then
                                        ( rest, List.map toCmd entry.commands |> Cmd.batch )

                                    else
                                        ( runState.nextCommands, Cmd.none )

                                toCmd : { a | device : Device, deviceIndex : Int, command : Command } -> Cmd msg
                                toCmd e =
                                    FlowIO.sendCommand
                                        { deviceIndex = e.deviceIndex
                                        , command = encodeCommand <| translateActionInCommand e.device e.command
                                        }

                                newState =
                                    Running { runState | elapsedTime = runningTime, nextCommands = nextCommands }
                            in
                            ( { model | runStatus = newState }, NoMessage, cmds )

                        [] ->
                            ( { model | runStatus = NotRunning }, NoMessage, Cmd.none )


view : Model -> Element SequencerMsg
view model =
    column [ fullWidth, height fill, spacing 10 ]
        [ viewRoleAssignments model
        , row [ fullWidth, height fill, htmlAttribute <| Html.Attributes.style "overflow" "auto" ]
            [ viewSequence model
            , viewAvailableParts model
            ]
        , viewControls model
        ]


viewRoleAssignments : Model -> Element SequencerMsg
viewRoleAssignments model =
    let
        roles =
            model.roleAssignments
                |> Dict.keys
                |> List.map viewRole

        viewRole name =
            let
                assignments =
                    model.roleAssignments
                        |> Dict.get name
                        |> Maybe.withDefault Set.empty

                content =
                    row [ spacing 5 ]
                        [ el [ Font.semiBold ] <| text name
                        , text ":"
                        , if Set.isEmpty assignments then
                            el [ Font.color Styles.palette.error ] <| text "not assigned"

                          else
                            text
                                ((String.fromInt <| Set.size assignments)
                                    ++ " assigned"
                                )
                        ]
            in
            button (Styles.buttonPrimary ++ [])
                { label = content
                , onPress = Just <| RoleClicked name
                }
    in
    wrappedRow [ fullWidth, Styles.bottomBorder, padding 5, spacing 10 ]
        [ text "Roles Assignment:"
        , row [ spacing 10 ] roles
        ]


viewSequence : Model -> Element SequencerMsg
viewSequence { sequence, roleAssignments } =
    let
        numParts =
            List.length sequence

        length instructions =
            instructions.time
                |> Array.last
                |> Maybe.withDefault TypedTime.zero

        totalLength =
            sequence
                |> List.map Tuple.second
                |> List.map length
                |> TypedTime.sum

        relativeLength instructions =
            length instructions |> TypedTime.divide (TypedTime.toMilliseconds totalLength)

        startTimes =
            sequence
                |> List.map (Tuple.second >> length)
                |> List.scanl TypedTime.add TypedTime.zero

        viewPart index ( startTime, ( partName, instructions ) ) =
            let
                size =
                    relativeLength instructions
                        |> TypedTime.toMilliseconds
                        |> (*) (toFloat numParts)
                        |> round

                displayRole role isNotAssigned =
                    button
                        (Styles.card
                            ++ [ Background.color Styles.palette.background
                               , Font.color <|
                                    if isNotAssigned then
                                        Styles.palette.error

                                    else
                                        Styles.palette.onBackground
                               , mouseOver [ Border.innerGlow Dracula.purple 2 ]
                               ]
                        )
                        { label = text role, onPress = Just <| RoleClicked role }

                partRoles =
                    instructions.instructions
                        |> Dict.keys
                        |> List.filterNot String.isEmpty
                        |> List.map
                            (\role ->
                                displayRole role
                                    (Dict.get role roleAssignments
                                        |> Maybe.withDefault Set.empty
                                        |> Set.isEmpty
                                    )
                            )
            in
            row
                (Styles.card
                    ++ Styles.colorsPrimary
                    ++ [ height <|
                            fillPortion size
                       , fullWidth
                       , spacing 10
                       ]
                )
                [ column [ height <| fill, width <| fillPortion 1, Styles.fontSize.smaller, alignLeft, spacing 5 ]
                    [ row [ alignTop, spacing 5 ]
                        [ text <| TypedTime.toFormattedString TypedTime.HoursMinutesSecondsHundredths startTime
                        , if index /= 0 then
                            button (Styles.button ++ [ paddingXY 6 2 ])
                                { label = el [ Region.description "move up" ] <| text "⇧"
                                , onPress = Just <| MovePart index MoveUp
                                }

                          else
                            none
                        ]
                    , el [ height <| Element.minimum 1 fill ] <| none
                    , row [ alignBottom, spacing 5 ]
                        [ startTime
                            |> TypedTime.add (length instructions)
                            |> TypedTime.toFormattedString TypedTime.HoursMinutesSecondsHundredths
                            |> text
                        , if index /= numParts - 1 then
                            button (Styles.button ++ [ paddingXY 6 2 ])
                                { label = el [ Region.description "move down" ] <| text "⇩"
                                , onPress = Just <| MovePart index MoveDown
                                }

                          else
                            none
                        ]
                    ]
                , column [ width <| fillPortion 4, centerX, Font.semiBold ]
                    [ text partName
                    , row [ Styles.fontSize.small, spacing 10 ] partRoles
                    ]
                , button (Styles.button ++ [ alignRight ])
                    { label = el [ Region.description "remove" ] <| Images.removeCircleIcon
                    , onPress = Just <| RemoveFromSequence index
                    }
                ]

        partSpacer =
            el [] none
    in
    sequence
        |> List.zip startTimes
        |> List.indexedMap viewPart
        |> List.intersperse partSpacer
        |> (::) partSpacer
        |> column
            [ width <| fillPortion 3
            , height fill
            , alignTop
            , padding 10
            , Styles.rightBorder
            , spacing 5
            , scrollbarY
            ]


viewAvailableParts : Model -> Element SequencerMsg
viewAvailableParts model =
    let
        newPartControls =
            row [ fullWidth, spaceEvenly, alignTop, spacing 5 ]
                [ button (Styles.button ++ [])
                    { onPress = Just RequestLoadFromScheduler
                    , label = el [] <| text "Copy from Scheduler"
                    }
                , button (Styles.button ++ [])
                    { onPress = Just RequestFromConverter
                    , label = el [] <| text "Copy from Converter"
                    }
                , button (Styles.button ++ [])
                    { onPress = Just LoadFromStorageRequested
                    , label = el [] <| text "Load"
                    }
                , button (Styles.button ++ [])
                    { onPress = Just RequestUploadInstructions
                    , label = el [] <| text "Upload"
                    }
                ]

        viewPart : ( String, Instructions ) -> Element SequencerMsg
        viewPart ( partName, instructions ) =
            let
                length =
                    instructions.time
                        |> Array.last
                        |> Maybe.withDefault TypedTime.zero

                lengthString =
                    length |> TypedTime.toFormattedString TypedTime.HoursMinutesSecondsHundredths

                roles =
                    instructions.instructions
                        |> Dict.keys

                showState =
                    model.showPartDetails |> Dict.get partName |> Maybe.withDefault Collapsed
            in
            column (Styles.card ++ Styles.colorsPrimary ++ [ fullWidth, padding 5, spacing 10 ])
                [ row [ spacing 5, fullWidth ]
                    [ el [ Font.bold, padding 4 ] <| text partName
                    , el [ Font.italic, padding 4 ] <| text lengthString
                    , text "Roles: "
                    , text <| String.fromInt <| List.length roles
                    , button (Styles.button ++ [ alignRight ])
                        { onPress = Just <| AddPartToSequence partName
                        , label = el [ Region.description "add" ] Images.addCircleIcon
                        }
                    , button (Styles.button ++ [ alignRight ])
                        { onPress = Just <| TogglePartDetails partName
                        , label =
                            case showState of
                                Collapsed ->
                                    Images.expandIcon

                                Expanded ->
                                    Images.collapseIcon
                        }
                    ]
                ]
    in
    column [ width <| fillPortion 2, height fill, padding 5, spacing 10 ]
        [ newPartControls
        , model.availableParts
            |> Dict.toList
            |> List.map viewPart
            |> column [ fullWidth, height fill, spacing 10 ]
        ]


viewControls : Model -> Element SequencerMsg
viewControls model =
    row [ centerX, alignBottom ]
        [ row [ spacing 5, width <| fillPortion 3 ] <|
            case model.runStatus of
                NotRunning ->
                    [ button ((width <| fillPortion 1) :: Styles.buttonPrimary)
                        { label = row [ spacing 5 ] [ Images.playIcon, text "Play" ]
                        , onPress = Just PlaySequenceRequested
                        }
                    , el [ width <| fillPortion 2 ] none
                    ]

                Running status ->
                    [ button ((width <| fillPortion 1) :: Styles.buttonPrimary)
                        { label = row [ spacing 5 ] [ Images.stopIcon, text "Stop" ]
                        , onPress = Just PlaySequenceStopped
                        }
                    , el [ width <| fillPortion 2 ] <|
                        text <|
                            TypedTime.toFormattedString
                                TypedTime.HoursMinutesSecondsHundredths
                                status.elapsedTime
                    ]
        , button ((width <| fillPortion 1) :: Styles.button)
            { label = text "Save"
            , onPress = Just OpenSaveSequenceDialog
            }
        , button ((width <| fillPortion 1) :: Styles.button)
            { label = text "Download"
            , onPress = Just DownloadSequence
            }
        , button ((width <| fillPortion 1) :: Styles.button)
            { label = text "Upload"
            , onPress = Just RequestSequenceUpload
            }
        ]


viewDialog : Model -> Element SequencerMsg
viewDialog model =
    let
        dialogBase title content =
            column
                (Styles.card
                    ++ [ Border.width 1
                       , Border.color Styles.palette.onBackground
                       , Styles.elevatedShadow
                       , Background.color Styles.palette.background
                       , width <| Element.minimum 120 <| Element.maximum 800 <| Element.shrink
                       , centerX
                       , centerY
                       , padding 20
                       , spacing 10
                       ]
                )
                [ row [ fullWidth, spacing 10, Styles.bottomBorder ]
                    [ el [ Styles.fontSize.large, centerX ] <| text title
                    , button [ alignRight ] { label = text "✕", onPress = Just CloseDialog }
                    ]
                , content
                ]
    in
    case model.dialog of
        NoDialog ->
            none

        LoadFromStorageDialog ->
            dialogBase "Load" none

        SaveSequenceDialog _ ->
            dialogBase "Save" none

        AssignRoleDialog roleName ->
            let
                assignments =
                    model.roleAssignments
                        |> Dict.get roleName
                        |> Maybe.withDefault Set.empty

                getId device =
                    device.details
                        |> Maybe.map .id
                        |> Maybe.withDefault "unknown ID"

                isAssigned device =
                    Set.member (getId device) assignments

                displayDeviceCheckbox : Device -> Element SequencerMsg
                displayDeviceCheckbox device =
                    row [ fullWidth, spacing 10 ]
                        [ checkbox []
                            { onChange = DeviceAssignmentChanged roleName device
                            , label = labelHidden "select device"
                            , checked = isAssigned device
                            , icon = Element.Input.defaultCheckbox
                            }
                        , device.details
                            |> Maybe.map .name
                            |> Maybe.withDefault "unknown device"
                            |> text
                        ]
            in
            model.availableDevices
                |> Array.filter (\d -> d.status == FlowIO.Connected || isAssigned d)
                |> Array.map displayDeviceCheckbox
                |> Array.toList
                |> (\devices ->
                        if List.isEmpty devices then
                            [ el [ centerX ] <| text "No connected devices" ]

                        else
                            devices
                   )
                |> column []
                |> dialogBase (roleName ++ " Assignments")
