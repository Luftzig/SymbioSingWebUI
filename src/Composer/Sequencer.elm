module Composer.Sequencer exposing (IncomingMsg(..), Model, Msg, OutgoingMsg(..), init, send, update, view)

import Array exposing (Array)
import Dict exposing (Dict)
import Element exposing (Element, alignBottom, alignRight, alignTop, centerX, column, el, fill, fillPortion, height, none, padding, row, spaceEvenly, spacing, text, width, wrappedRow)
import Element.Border as Border
import Element.Input exposing (button)
import File exposing (File)
import File.Select
import FlowIO exposing (DeviceId, FlowIODevice)
import Json.Decode as JD
import LocalStorage
import Scheduler exposing (Instructions, RoleName, instructionsDecoder)
import Set exposing (Set)
import Styles exposing (fullWidth)
import Task


type alias Model =
    { availableParts : Dict String Instructions
    , sequence : List ( String, Instructions )
    , roleAssignments : Dict RoleName DeviceId
    , availableDevices : Array FlowIODevice
    , instructionsStorage : Set String
    , dialog : DialogStatus
    }


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


view : Model -> Element Msg
view model =
    column [ fullWidth, height fill, spacing 10 ]
        [ viewRoleAssignments model
        , row [ fullWidth, height fill ]
            [ viewSequence model
            , viewAvailableParts model
            ]
        , viewControls model
        ]


viewRoleAssignments model =
    row [ fullWidth, Styles.bottomBorder, padding 5 ]
        [ text "Roles Assignment:"
        ]


viewSequence : Model -> Element Msg
viewSequence { sequence } =
    let
        viewPart ( partName, instructions ) =
            el [] <| text partName

        partSpacer =
            el [] none
    in
    sequence
        |> List.map viewPart
        |> List.intersperse partSpacer
        |> (::) partSpacer
        |> column
            [ width <| fillPortion 3
            , height fill
            , alignTop
            , padding 5
            , Styles.rightBorder
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

        viewPart ( partName, instructions ) =
            text partName
    in
    column [ width <| fillPortion 2, height fill, padding 5, spacing 10 ]
        [ newPartControls
        , model.availableParts
            |> Dict.toList
            |> List.map viewPart
            |> column [ fullWidth, height fill ]
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
