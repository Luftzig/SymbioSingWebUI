module Composer.Converter exposing (Model, Msg, init, update, view)

import Color.Dracula as Dracula
import Composer.Notation as Notation exposing (ConversionParameters, Dynamic(..), Dynamics, HapticScore, PartID, parseMusicXml)
import Dict exposing (Dict)
import Dict.Extra as Dict
import Element exposing (Element, alignRight, below, column, el, fill, fillPortion, padding, paddingEach, paddingXY, paragraph, px, row, shrink, spacing, table, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onFocus, onLoseFocus)
import Element.Font as Font
import Element.Input as Input exposing (button, labelLeft, radioRow)
import Extra.Resource as Resource exposing (Resource(..))
import File exposing (File)
import File.Download
import File.Select
import FlowIO
import Json.Encode as JE
import Maybe.Extra as Maybe
import Process
import Regex exposing (Regex)
import Result.Extra as Result
import Scheduler exposing (encodeInstructions)
import Set
import Styles
import Task


type alias Model =
    { sourceFile : Maybe File
    , hapticScore : Resource String HapticScore
    , bpm : Int
    , roleMapping : Dict PartID { roleName : Maybe Scheduler.RoleName, port_ : Maybe FlowIO.Port }
    , dynamics : Dynamics
    , showRolesSuggestions : Maybe PartID
    , schedule : Resource String Scheduler.Instructions
    , targetName : String
    }


init : Model
init =
    { sourceFile = Nothing
    , hapticScore = NotLoaded
    , bpm = 80
    , roleMapping = Dict.empty
    , dynamics = defaultDynamics
    , showRolesSuggestions = Nothing
    , schedule = NotLoaded
    , targetName = ""
    }


defaultDynamics : Dynamics
defaultDynamics =
    { pianissimo = 36
    , piano = 73
    , mezzopiano = 109
    , mezzoforte = 146
    , forte = 182
    , fortissimo = 219
    , fortississimo = 255
    }


type Msg
    = SelectScoreFile
    | FileSelected File
    | ParsingCompleted (Result String HapticScore)
    | PortSelected PartID FlowIO.Port
    | PartRoleNameChanged PartID String
    | RoleNameInputFieldFocused PartID
    | RoleNameInputFieldLostFocus PartID
    | LoseFocusDelayWaited PartID
    | RoleSuggestionSelected PartID String
    | ConversionRequested
    | DownloadRequested Scheduler.Instructions
    | BpmChanged String
    | UpdateDynamic Dynamic Float
    | NameChanged String


view : Model -> Element Msg
view model =
    column [ Styles.fullWidth, spacing 10 ]
        [ loadFile model
        , showHapticScore model.hapticScore
        , showConversionControls model
        ]


loadFile { hapticScore } =
    column [ spacing 10 ]
        [ row [ spacing 5 ]
            [ button Styles.button
                { label = el [] <| text "Select Score"
                , onPress = Just SelectScoreFile
                }
            ]
        ]


showHapticScore hapticScore =
    case hapticScore of
        NotLoaded ->
            el [] <| Element.none

        Loaded d ->
            el [] <| text "File parsed successfully!"

        Error e ->
            paragraph []
                [ text "Encountered error parsing file: "
                , text e
                ]

        Processing ->
            el [] <| text "Processing..."


showConversionControls model =
    case model.hapticScore of
        Loaded score ->
            let
                idsToNames : Dict PartID String
                idsToNames =
                    Dict.map (\_ -> .name) score
            in
            row [ spacing 28, Styles.fullWidth ]
                [ column [ spacing 10, width <| fillPortion 1 ]
                    [ showRoleMapping model idsToNames
                    , Styles.spacer
                    , row [ spacing 10 ]
                        [ Input.text (Styles.textFieldStyle ++ [ width <| fillPortion 1 ])
                            { label = labelLeft [ width shrink ] <| text "BPM"
                            , onChange = BpmChanged
                            , text = model.bpm |> String.fromInt
                            , placeholder = Nothing
                            }
                        , Input.text (Styles.textFieldStyle ++ [ width <| fillPortion 3 ])
                            { label = labelLeft [ width shrink ] <| text "Name"
                            , onChange = NameChanged
                            , text = model.targetName
                            , placeholder = Nothing
                            }
                        , button ((width <| fillPortion 1) :: Font.center :: Styles.button)
                            { label = text "Convert", onPress = Just ConversionRequested }
                        ]
                    , case model.schedule of
                        Error e ->
                            paragraph []
                                [ text "error in conversion: "
                                , el [ Font.color Styles.palette.error ] <| text e
                                ]

                        NotLoaded ->
                            Element.none

                        Processing ->
                            text "Processing ⏱"

                        Loaded schedule ->
                            button ((width <| fillPortion 1) :: Styles.button)
                                { label = text "Download", onPress = Just <| DownloadRequested schedule }
                    ]
                , showDynamicsControls model
                ]

        _ ->
            Element.none


showDynamicsControls { dynamics } =
    let
        dynamicsSlider msgType label getter =
            row [ spacing 5, Styles.fullWidth ]
                [ el [ width <| fillPortion 1 ] <| text label
                , Input.slider
                    [ width <| px 263
                    , Background.color Styles.palette.primary
                    , Border.rounded 10
                    , Border.color Styles.palette.onBackground
                    , Border.width 1
                    , width <| fillPortion 10
                    ]
                    { onChange = UpdateDynamic msgType
                    , label = labelLeft [ alignRight, width <| fillPortion 1 ] <| text <| String.fromInt (getter dynamics)
                    , value = toFloat <| getter dynamics
                    , thumb = Input.defaultThumb
                    , step = Just 1.0
                    , min = 0
                    , max = 255
                    }
                ]
    in
    column [ spacing 10, width <| fillPortion 1 ]
        [ el [ Styles.bottomBorder ] <| text "Dynamics"
        , dynamicsSlider Pianissimo "pp" .pianissimo
        , dynamicsSlider Piano "p" .piano
        , dynamicsSlider Mezzopiano "mp" .mezzopiano
        , dynamicsSlider Mezzoforte "mf" .mezzoforte
        , dynamicsSlider Mezzoforte "mf" .mezzoforte
        , dynamicsSlider Forte "f" .forte
        , dynamicsSlider Fortissimo "ff" .fortissimo
        , dynamicsSlider Fortississimo "fff" .fortississimo
        ]


showRoleMapping : Model -> Dict Notation.PartID String -> Element Msg
showRoleMapping { roleMapping, showRolesSuggestions } partNames =
    let
        data =
            partNames
                |> Dict.toList
                |> List.map getRoleAndPort
                |> List.sortBy (.partId >> String.replace "P" "" >> String.toInt >> Maybe.withDefault 0)

        getRoleAndPort ( partId, partName ) =
            { partId = partId
            , partName = partName
            , roleName = Dict.get partId roleMapping |> Maybe.andThen .roleName
            , port_ = Dict.get partId roleMapping |> Maybe.andThen .port_
            }

        error : String -> Element.Attribute Msg
        error message =
            below <|
                el
                    [ Styles.fullWidth
                    , Font.size 10
                    , Background.color Dracula.red
                    , Font.color Styles.palette.onBackground
                    ]
                <|
                    text message

        partNameColumn =
            { header = el [ Font.underline ] <| text "Part"
            , view =
                \{ partName, partId } ->
                    row [ paddingXY 1 4, spacing 5 ]
                        [ el [] <| text partName
                        , el [ Font.size 10, alignRight, paddingXY 4 0 ] <| text ("(" ++ partId ++ ")")
                        ]
            , width = fillPortion 1
            }

        roleNameColumn =
            { header = el [ Font.underline ] <| text "Role"
            , view =
                \{ roleName, partId } ->
                    let
                        otherParts : List ( PartID, { roleName : Maybe Scheduler.RoleName, port_ : Maybe FlowIO.Port } )
                        otherParts =
                            roleMapping
                                |> Dict.toList
                                |> List.filter (\( k, _ ) -> k /= partId)

                        tooManyAssigned : Bool
                        tooManyAssigned =
                            case roleName of
                                Just name ->
                                    roleMapping
                                        |> Dict.toList
                                        |> List.filterMap (Tuple.second >> .roleName)
                                        |> List.filter ((==) name)
                                        |> List.length
                                        |> (\count -> count > 5)

                                Nothing ->
                                    False

                        suggestions : List String
                        suggestions =
                            otherParts
                                |> List.filterMap (Tuple.second >> .roleName)
                                |> Set.fromList
                                |> Set.toList

                        suggestionsList : Element Msg
                        suggestionsList =
                            column
                                [ spacing 5
                                , Font.size 10
                                , Styles.elevatedShadow
                                , Background.color Styles.palette.background
                                ]
                                (suggestions
                                    |> List.map
                                        (\s ->
                                            Input.button Styles.button
                                                { label = text s
                                                , onPress = Just <| RoleSuggestionSelected partId s
                                                }
                                        )
                                )
                    in
                    el
                        [ paddingEach { bottom = 10, left = 1, right = 1, top = 1 }
                        ]
                    <|
                        Input.text
                            (Styles.textFieldStyle
                                ++ [ onFocus (RoleNameInputFieldFocused partId)
                                   , onLoseFocus (RoleNameInputFieldLostFocus partId)
                                   ]
                                ++ (if showRolesSuggestions == Just partId then
                                        [ below <| suggestionsList ]

                                    else
                                        []
                                   )
                                ++ (if tooManyAssigned then
                                        [ error "at most 5 parts allowed per role" ]

                                    else
                                        []
                                   )
                            )
                            { onChange = PartRoleNameChanged partId
                            , label = Input.labelHidden "role name"
                            , text = roleName |> Maybe.withDefault ""
                            , placeholder = Just (Input.placeholder [] (text "Assign role"))
                            }
            , width = fillPortion 2
            }

        portColumn =
            { header = el [ Font.underline ] <| text "Port"
            , view =
                \{ port_, partId, roleName } ->
                    let
                        isLegalAssignment =
                            roleMapping
                                |> Dict.toList
                                |> List.filter
                                    (\( _, v ) ->
                                        Maybe.isJust roleName
                                            && roleName
                                            == v.roleName
                                            && Maybe.isJust port_
                                            && port_
                                            == v.port_
                                    )
                                |> List.length
                                |> (\count -> count <= 1)
                    in
                    radioRow
                        ([ paddingEach { bottom = 10, left = 1, right = 1, top = 1 }, spacing 10 ]
                            ++ (if isLegalAssignment then
                                    []

                                else
                                    [ error "each port can only be assigned once" ]
                               )
                        )
                        { onChange = PortSelected partId
                        , label = Input.labelHidden "select port"
                        , options =
                            [ FlowIO.Port1, FlowIO.Port2, FlowIO.Port3, FlowIO.Port4, FlowIO.Port5 ]
                                |> List.map (\p -> Input.option p (FlowIO.portToIndex p |> String.fromInt |> text))
                        , selected = port_
                        }
            , width = fillPortion 2
            }
    in
    table []
        { data = data
        , columns =
            [ partNameColumn
            , roleNameColumn
            , portColumn
            ]
        }


removeExtension : Maybe Regex
removeExtension =
    Regex.fromString "\\.[^.]+$"


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        updateRoleName partID name =
            Dict.update partID
                (\v ->
                    case v of
                        Just record ->
                            Just { record | roleName = Just name }

                        Nothing ->
                            Just { port_ = Nothing, roleName = Just name }
                )
                model.roleMapping
    in
    case msg of
        SelectScoreFile ->
            ( model
            , File.Select.file
                [ "application/vnd.recordare.musicxml+xml"
                , "application/vnd.recordare.musicxml"
                , "application/xml"
                ]
                FileSelected
            )

        FileSelected file ->
            ( { model
                | sourceFile = Just file
                , hapticScore = Processing
                , targetName =
                    file
                        |> File.name
                        |> (\name ->
                                case removeExtension of
                                    Just re ->
                                        Regex.replace re (Basics.always "") name

                                    _ ->
                                        name
                           )
                        |> (\name -> name ++ "-schedule.json")
              }
            , File.toString file
                |> Task.andThen (parseMusicXml >> Result.toTask)
                |> Task.attempt ParsingCompleted
            )

        ParsingCompleted result ->
            ( { model | hapticScore = Resource.fromResult result }, Cmd.none )

        PortSelected partID port_ ->
            ( { model
                | roleMapping =
                    Dict.update partID
                        (\v ->
                            case v of
                                Just record ->
                                    Just { record | port_ = Just port_ }

                                Nothing ->
                                    Just { port_ = Just port_, roleName = Nothing }
                        )
                        model.roleMapping
              }
            , Cmd.none
            )

        PartRoleNameChanged partID name ->
            ( { model
                | roleMapping =
                    updateRoleName partID name
              }
            , Cmd.none
            )

        RoleNameInputFieldFocused partID ->
            ( { model | showRolesSuggestions = Just partID }, Cmd.none )

        RoleNameInputFieldLostFocus partID ->
            if model.showRolesSuggestions == Just partID then
                ( model, Process.sleep 250 |> Task.perform (\_ -> LoseFocusDelayWaited partID) )

            else
                ( model, Cmd.none )

        LoseFocusDelayWaited partID ->
            if model.showRolesSuggestions == Just partID then
                ( { model | showRolesSuggestions = Nothing }, Cmd.none )

            else
                ( model, Cmd.none )

        RoleSuggestionSelected partID role ->
            ( { model
                | roleMapping = updateRoleName partID role
                , showRolesSuggestions = Nothing
              }
            , Cmd.none
            )

        ConversionRequested ->
            let
                params : ConversionParameters
                params =
                    { bpm = model.bpm
                    , dynamics = model.dynamics
                    , roleMapping =
                        model.roleMapping
                            |> Dict.filterMap (\_ { roleName, port_ } -> Maybe.map2 Tuple.pair roleName port_)
                    }
            in
            case model.hapticScore of
                Loaded score ->
                    ( { model | schedule = Resource.fromResult <| Notation.scoreToSchedule params score }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        DownloadRequested instructions ->
            ( model
            , File.Download.string
                model.targetName
                "application/json"
                (JE.encode 2 <| encodeInstructions instructions)
            )

        BpmChanged string ->
            case String.toInt string of
                Just a ->
                    ( { model | bpm = a }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        UpdateDynamic dynamic value ->
            let
                dynamics =
                    model.dynamics

                newDynamics =
                    case dynamic of
                        Pianississimo ->
                            dynamics

                        Pianissimo ->
                            { dynamics | pianissimo = round value }

                        Piano ->
                            { dynamics | piano = round value }

                        Mezzopiano ->
                            { dynamics | mezzopiano = round value }

                        Mezzoforte ->
                            { dynamics | mezzoforte = round value }

                        Forte ->
                            { dynamics | forte = round value }

                        Fortissimo ->
                            { dynamics | fortissimo = round value }

                        Fortississimo ->
                            { dynamics | fortississimo = round value }
            in
            ( { model | dynamics = newDynamics }, Cmd.none )

        NameChanged newName ->
            ( { model | targetName = newName }, Cmd.none )
