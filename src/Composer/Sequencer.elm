module Composer.Sequencer exposing (IncomingMsg(..), Model, Msg, OutgoingMsg(..), init, send, update, view)

import Array exposing (Array)
import Dict as Dict exposing (Dict)
import Dict.Extra as Dict
import Element exposing (Element, alignBottom, alignLeft, alignRight, alignTop, centerX, column, el, fill, fillPortion, height, htmlAttribute, none, padding, paddingXY, paragraph, row, scrollbarY, spaceEvenly, spacing, text, width, wrappedRow)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input exposing (button)
import Element.Region as Region
import Extra.Array as Array
import Extra.Dict as Dict
import Extra.TypedTime as TypedTime
import File exposing (File)
import File.Select
import FlowIO exposing (DeviceId, FlowIODevice)
import Html.Attributes
import Images
import Json.Decode as JD
import List.Extra as List
import LocalStorage
import Scheduler exposing (Instructions, RoleName, instructionsDecoder)
import Set exposing (Set)
import Styles exposing (fullWidth)
import Task


type alias Model =
    { availableParts : Dict String Instructions
    , sequence : List ( String, Instructions )
    , roleAssignments : Dict RoleName (List DeviceId)
    , availableDevices : Array FlowIODevice
    , instructionsStorage : Set String
    , dialog : DialogStatus
    , showPartDetails : Dict String Collapsable
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


init : Model
init =
    { availableParts = Dict.empty
    , sequence = []
    , roleAssignments = Dict.empty
    , availableDevices = Array.empty
    , instructionsStorage = Set.empty
    , dialog = NoDialog
    , showPartDetails = Dict.empty
    }


type Msg
    = ReceivedMessage IncomingMsg
    | RequestLoadFromScheduler
    | RequestFromConverter
    | LoadFromStorageRequested
    | LoadFromStorage String
    | RequestUploadInstructions
    | InstructionsFileSelected File
    | InstructionFileRead String String
    | PlaySequence
    | OpenSaveSequenceDialog
    | DownloadSequence
    | RequestSequenceUpload
    | CloseDialog
    | AddPartToSequence String
    | TogglePartDetails String
    | RemoveFromSequence Int
    | MovePart Int MoveDirection


type MoveDirection
    = MoveUp
    | MoveDown



-- | MoveToBeginning
-- | MoveToEnd
-- | MoveToPosition Int


type IncomingMsg
    = DevicesChanged (Array FlowIODevice)
    | ReceivedNewPart String Instructions


send : IncomingMsg -> Msg
send =
    ReceivedMessage


type OutgoingMsg
    = NoMessage
    | GetInstructionFromScheduler
    | GetInstructionFromConverter
    | LogError String


update : Msg -> Model -> ( Model, OutgoingMsg, Cmd Msg )
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

        RequestLoadFromScheduler ->
            ( model, GetInstructionFromScheduler, Cmd.none )

        RequestFromConverter ->
            ( model, GetInstructionFromConverter, Cmd.none )

        LoadFromStorageRequested ->
            ( { model | dialog = LoadFromStorageDialog }, NoMessage, Cmd.none )

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

        PlaySequence ->
            Debug.todo "Play Sequence"

        OpenSaveSequenceDialog ->
            ( { model | dialog = SaveSequenceDialog "" }, NoMessage, Cmd.none )

        DownloadSequence ->
            Debug.todo "Download sequence"

        RequestSequenceUpload ->
            Debug.todo "Request sequence upload"

        CloseDialog ->
            ( { model | dialog = NoDialog }, NoMessage, Cmd.none )

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
                        |> List.filterNot (String.isEmpty)

                newRoles =
                    Dict.addKeys [] partRoles model.roleAssignments
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


view : Model -> Element Msg
view model =
    column [ fullWidth, height fill, spacing 10 ]
        [ viewRoleAssignments model
        , row [ fullWidth, height fill, htmlAttribute <| Html.Attributes.style "overflow" "auto" ]
            [ viewSequence model
            , viewAvailableParts model
            ]
        , viewControls model
        ]


viewRoleAssignments model =
    let
        roles =
            model.roleAssignments
                |> Dict.keys
                |> List.map text
    in
    row [ fullWidth, Styles.bottomBorder, padding 5 ]
        [ text "Roles Assignment:"
        , row [ spacing 10 ] roles
        ]


viewSequence : Model -> Element Msg
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
            |> List.scanl (TypedTime.add) TypedTime.zero

        viewPart index (startTime, ( partName, instructions )) =
            let
                size =
                    relativeLength instructions
                        |> TypedTime.toMilliseconds
                        |> (*) (toFloat numParts)
                        |> round

                partRoles =
                    instructions.instructions
                        |> Dict.keys
                        |> List.filterNot String.isEmpty
                        |> List.map
                            (\role ->
                                case Dict.get role roleAssignments of
                                    -- Non-empty list
                                    Just (_ :: _) ->
                                        el
                                            (Styles.card
                                                ++ [ Background.color Styles.palette.background
                                                   , Font.color Styles.palette.onBackground
                                                   ]
                                            )
                                        <|
                                            text role

                                    _ ->
                                        el
                                            (Styles.card
                                                ++ [ Background.color Styles.palette.background
                                                   , Font.color Styles.palette.error
                                                   ]
                                            )
                                        <|
                                            text role
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


viewAvailableParts : Model -> Element Msg
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

        viewPart : ( String, Instructions ) -> Element Msg
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


viewControls : Model -> Element Msg
viewControls model =
    row [ centerX, alignBottom ]
        [ button Styles.buttonPrimary
            { label = text "Play"
            , onPress = Just PlaySequence
            }
        , button Styles.button
            { label = text "Save"
            , onPress = Just OpenSaveSequenceDialog
            }
        , button Styles.button
            { label = text "Download"
            , onPress = Just DownloadSequence
            }
        , button Styles.button
            { label = text "Upload"
            , onPress = Just RequestSequenceUpload
            }
        ]


viewDialog : Model -> Element Msg
viewDialog { dialog } =
    case dialog of
        NoDialog ->
            none

        LoadFromStorageDialog ->
            column
                (Styles.card
                    ++ [ Border.width 1
                       , Border.color Styles.palette.onBackground
                       , Styles.elevatedShadow
                       ]
                )
                [ row [ fullWidth ]
                    [ el [ centerX ] <| text "Load"
                    , button [ alignRight ] { label = text "✕", onPress = Just CloseDialog }
                    ]
                ]

        SaveSequenceDialog string ->
            column
                (Styles.card
                    ++ [ Border.width 1
                       , Border.color Styles.palette.onBackground
                       , Styles.elevatedShadow
                       ]
                )
                [ row [ fullWidth ]
                    [ el [ centerX ] <| text "Load"
                    , button [ alignRight ] { label = text "✕", onPress = Just CloseDialog }
                    ]
                ]
