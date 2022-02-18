module Composer.Converter exposing (Model, init, update, view)

import Color.Dracula as Dracula
import Composer.Notation as Notation exposing (ConversionParameters, Dynamics, IntermediateRepr, parseMusicXml)
import Dict exposing (Dict)
import Dict.Extra as Dict
import Element exposing (Element, alignBottom, alignRight, below, column, el, fill, fillPortion, height, maximum, minimum, padding, paddingEach, paddingXY, paragraph, row, scrollbars, shrink, spacing, table, text, width, wrappedRow)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onFocus, onLoseFocus)
import Element.Font as Font
import Element.Input as Input exposing (button, labelAbove, labelHidden, labelRight, radioRow)
import Extra.Resource as Resource exposing (Resource(..))
import Extra.TypedTime as TypedTime exposing (TypedTime)
import File exposing (File)
import File.Download
import File.Select
import FlowIO
import Json.Encode as JE
import LocalStorage
import Maybe.Extra as Maybe
import Messages exposing (..)
import Process
import Regex exposing (Regex)
import Result.Extra as Result
import Scheduler exposing (encodeInstructions)
import Set
import Styles exposing (palette, tabStyle)
import Task


type alias Model =
    { sourceFile : Maybe File
    , hapticScore : Resource String HapticScore
    , bpm : Int
    , roleMapping : Dict PartID { roleName : Maybe RoleName, port_ : Maybe FlowIO.Port }
    , dynamics : Dynamics
    , regulatorSettings : Dynamics
    , displayedDynamics : DynamicsSettings
    , trillInterval : TrillInterval
    , showRolesSuggestions : Maybe PartID
    , schedule : Resource ( List (List IntermediateRepr), String ) Instructions
    , targetName : String
    }


init : Model
init =
    { sourceFile = Nothing
    , hapticScore = NotLoaded
    , bpm = 100
    , roleMapping = Dict.empty
    , dynamics = defaultDynamics
    , regulatorSettings = defaultRegulatorSettings
    , displayedDynamics = PWMValues
    , trillInterval = Absolute <| TypedTime.milliseconds 20
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


defaultRegulatorSettings =
    { pianissimo = 1
    , piano = 2
    , mezzopiano = 2
    , mezzoforte = 4
    , forte = 5
    , fortissimo = 5
    , fortississimo = 6
    }


view : Model -> Element ConverterMsg
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

        Loaded _ ->
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

                trillControl =
                    column [ spacing 5, width <| minimum 200 <| fillPortion 3 ]
                        [ el [ Font.underline ] <| text "Trill Interval"
                        , row [ spacing 10 ]
                            [ Input.text (Styles.textFieldStyle ++ [ width <| fillPortion 3 ])
                                { label = labelRight [] <| text "ms"
                                , onChange =
                                    \t ->
                                        t
                                            |> String.toFloat
                                            |> Maybe.withDefault 0
                                            |> TypedTime.milliseconds
                                            |> Absolute
                                            |> TrillIntervalChanged
                                , text =
                                    case model.trillInterval of
                                        Absolute t ->
                                            TypedTime.toMilliseconds t
                                                |> String.fromFloat

                                        PerBeat f ->
                                            TypedTime.minutes 1
                                                |> TypedTime.divide (toFloat model.bpm)
                                                |> TypedTime.divide f
                                                |> TypedTime.toMilliseconds
                                                |> String.fromFloat
                                , placeholder = Nothing
                                }
                            , el [ Font.italic ] <| text "or"
                            , Input.text (Styles.textFieldStyle ++ [ width <| fillPortion 4 ])
                                { label = labelRight [] <| text "per beat"
                                , onChange =
                                    \t ->
                                        t
                                            |> String.toFloat
                                            |> Maybe.withDefault 0
                                            |> PerBeat
                                            |> TrillIntervalChanged
                                , text =
                                    case model.trillInterval of
                                        Absolute t ->
                                            TypedTime.minutes 1
                                                |> TypedTime.divide (toFloat model.bpm)
                                                |> TypedTime.divideByInterval t
                                                |> String.fromFloat

                                        PerBeat f ->
                                            f |> String.fromFloat
                                , placeholder = Nothing
                                }
                            ]
                        ]
            in
            row [ spacing 28, Styles.fullWidth ]
                [ column [ spacing 10, width <| fillPortion 1 ]
                    [ showRoleMapping model idsToNames
                    , Styles.spacer
                    , wrappedRow [ spacing 10 ]
                        [ Input.text (Styles.textFieldStyle ++ [ width <| minimum 80 <| fillPortion 1 ])
                            { label = labelAbove [ width shrink, Font.underline ] <| text "BPM"
                            , onChange = BpmChanged
                            , text = model.bpm |> String.fromInt
                            , placeholder = Nothing
                            }
                        , trillControl
                        , Input.text (Styles.textFieldStyle ++ [ width <| minimum 320 <| fillPortion 3 ])
                            { label = labelAbove [ width shrink, Font.underline ] <| text "Name"
                            , onChange = NameChanged
                            , text = model.targetName
                            , placeholder = Nothing
                            }
                        , button ((width <| fillPortion 1) :: alignBottom :: Font.center :: Styles.button)
                            { label = text "Convert", onPress = Just ConversionRequested }
                        ]
                    , case model.schedule of
                        Error ( context, error ) ->
                            column [ spacing 10 ]
                                [ paragraph []
                                    [ text "error in conversion: "
                                    , el [ Font.color Styles.palette.error ] <| text error
                                    ]
                                --, context
                                --    |> List.map (\list -> Debug.toString list)
                                --    |> List.map text
                                --    |> column
                                --        [ Font.color Styles.palette.error
                                --        , width <| maximum 500 <| fill
                                --        , scrollbars
                                --        , height <| minimum 120 <| fill
                                --        ]
                                ]

                        NotLoaded ->
                            Element.none

                        Processing ->
                            text "Processing ⏱"

                        Loaded schedule ->
                            row [ Styles.fullWidth, spacing 10 ]
                                [ button ((width <| fillPortion 1) :: Styles.button)
                                    { label = text "Download", onPress = Just <| DownloadRequested schedule }
                                , button ((width <| fillPortion 1) :: Styles.button)
                                    { label = text "Save", onPress = Just <| SaveSchedule model.targetName schedule }
                                ]
                    ]
                , showDynamicsControls model
                ]

        _ ->
            Element.none


showDynamicsControls { dynamics, regulatorSettings, displayedDynamics } =
    let
        dynamicsSlider msgType label getter =
            row [ spacing 5, Styles.fullWidth ]
                [ Input.text
                    [ width <| minimum 32 <| fillPortion 1
                    , Styles.fontSize.small
                    , padding 5
                    , Background.color palette.background
                    , Font.color palette.onBackground
                    ]
                    { label = Input.labelLeft [ width <| minimum 18 <| fillPortion 1 ] <| text label
                    , text = String.fromInt <| getter dynamics
                    , onChange = UpdateDynamic msgType << Maybe.withDefault 0 << String.toFloat
                    , placeholder = Nothing
                    }
                , Input.slider
                    [ width <| minimum 100 <| fillPortion 8
                    , Background.color Styles.palette.primary
                    , Border.rounded 10
                    , Border.color Styles.palette.onBackground
                    , Border.width 1
                    ]
                    { onChange = UpdateDynamic msgType
                    , label = labelHidden label
                    , value = toFloat <| getter dynamics
                    , thumb = Input.defaultThumb
                    , step = Just 1.0
                    , min = 0
                    , max = 255
                    }
                ]

        dynamicsNumerical msgType label getter =
            Input.text
                [ width <| minimum 32 <| fillPortion 3
                , Styles.fontSize.small
                , padding 5
                , Background.color palette.background
                , Font.color palette.onBackground
                ]
                { label = Input.labelLeft [ width <| minimum 18 <| fillPortion 1 ] <| text label
                , text = String.fromInt <| getter regulatorSettings
                , onChange = UpdateDynamic msgType << Maybe.withDefault 0 << String.toFloat
                , placeholder = Nothing
                }
    in
    column [ spacing 10, width <| fillPortion 1 ]
        ([ row [ Styles.bottomBorder ]
            [ el [ padding 10 ] <| text "Dynamics:"
            , button (tabStyle (displayedDynamics == PWMValues))
                { label =
                    el [ padding 5 ] <|
                        text "PWM"
                , onPress = Just <| ChangeSettingsTo PWMValues
                }
            , button (tabStyle (displayedDynamics == RegulatorValues))
                { label =
                    el [ padding 5 ] <|
                        text "Regulator"
                , onPress = Just <| ChangeSettingsTo RegulatorValues
                }
            ]
         ]
            ++ (if displayedDynamics == PWMValues then
                    [ dynamicsSlider Pianissimo "pp" .pianissimo
                    , dynamicsSlider Piano "p" .piano
                    , dynamicsSlider Mezzopiano "mp" .mezzopiano
                    , dynamicsSlider Mezzoforte "mf" .mezzoforte
                    , dynamicsSlider Forte "f" .forte
                    , dynamicsSlider Fortissimo "ff" .fortissimo
                    , dynamicsSlider Fortississimo "fff" .fortississimo
                    ]

                else
                    [ dynamicsNumerical Pianissimo "pp" .pianissimo
                    , dynamicsNumerical Piano "p" .piano
                    , dynamicsNumerical Mezzopiano "mp" .mezzopiano
                    , dynamicsNumerical Mezzoforte "mf" .mezzoforte
                    , dynamicsNumerical Forte "f" .forte
                    , dynamicsNumerical Fortissimo "ff" .fortissimo
                    , dynamicsNumerical Fortississimo "fff" .fortississimo
                    ]
               )
        )


showRoleMapping : Model -> Dict PartID String -> Element ConverterMsg
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

        error : String -> Element.Attribute ConverterMsg
        error message =
            below <|
                el
                    [ Styles.fullWidth
                    , Styles.fontSize.smaller
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
                        , el [ Styles.fontSize.smaller, alignRight, paddingXY 4 0 ] <| text ("(" ++ partId ++ ")")
                        ]
            , width = fillPortion 1
            }

        roleNameColumn =
            { header = el [ Font.underline ] <| text "Role"
            , view =
                \{ roleName, partId } ->
                    let
                        otherParts : List ( PartID, { roleName : Maybe RoleName, port_ : Maybe FlowIO.Port } )
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

                        suggestionsList : Element ConverterMsg
                        suggestionsList =
                            column
                                [ spacing 5
                                , Styles.fontSize.smaller
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


update : ConverterMsg -> Model -> ( Model, Cmd ConverterMsg )
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

        setDynamic dynamics dynamic value =
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
    case msg of
        SelectScoreFile ->
            ( model
            , File.Select.file
                [ "application/vnd.recordare.musicxml+xml"
                , "application/vnd.recordare.musicxml"
                , "application/xml"
                , "*.musicxml"
                , ".musicxml"
                , "musicxml"
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
                trillInterval =
                    case model.trillInterval of
                        Absolute typedTime ->
                            typedTime

                        PerBeat float ->
                            TypedTime.minutes 1
                                |> TypedTime.divide (toFloat model.bpm)
                                |> TypedTime.divide float

                params : ConversionParameters
                params =
                    { bpm = model.bpm
                    , dynamics =
                        case model.displayedDynamics of
                            PWMValues ->
                                model.dynamics

                            RegulatorValues ->
                                model.regulatorSettings
                    , roleMapping =
                        model.roleMapping
                            |> Dict.filterMap (\_ { roleName, port_ } -> Maybe.map2 Tuple.pair roleName port_)
                    , trillInterval = trillInterval
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
                    setDynamic dynamics dynamic value
            in
            ( { model | dynamics = newDynamics }, Cmd.none )

        UpdateRegulator dynamic value ->
            ( { model | dynamics = setDynamic model.regulatorSettings dynamic value }, Cmd.none )

        NameChanged newName ->
            ( { model | targetName = newName }, Cmd.none )

        SaveSchedule key instructions ->
            ( model, LocalStorage.save key (encodeInstructions instructions) )

        ChangeSettingsTo newSettings ->
            ( { model | displayedDynamics = newSettings }, Cmd.none )

        TrillIntervalChanged trillInterval ->
            ( { model | trillInterval = trillInterval }, Cmd.none )
