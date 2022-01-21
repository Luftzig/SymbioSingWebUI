module Styles exposing (..)

import Color exposing (Color)
import Color.Dracula as Dracula
import Element exposing (Attribute, Color, Element, Length, alignTop, centerY, el, fill, height, htmlAttribute, mouseOver, none, paddingXY, px, rgb, rgb255, rgba, shrink, spacing, spacingXY, width)
import Element.Background as Background
import Element.Border as Border
import Element.Events
import Element.Font as Font
import Element.Input as Input exposing (OptionState(..))
import Element.Region as Region
import Html.Attributes
import Maybe.Extra


rust =
    rgb255 183 65 14


grey : Element.Color
grey =
    rgb 0.5 0.5 0.5


lightGrey : Element.Color
lightGrey =
    rgb 0.8 0.8 0.8


darkGrey : Element.Color
darkGrey =
    rgb 0.2 0.2 0.2


colorToCssString : Element.Color -> String
colorToCssString color =
    color
        |> Element.toRgb
        |> Color.fromRgba
        |> Color.toCssString


bottomBorder =
    Border.widthEach { bottom = 1, left = 0, right = 0, top = 0 }


rightBorder =
    Border.widthEach { bottom = 0, left = 0, right = 1, top = 0 }


borderWhite =
    Border.color Dracula.white


elevatedShadow =
    Border.shadow { offset = ( 1, 2 ), size = 1, blur = 3, color = darkGrey }


spacer =
    Element.el
        [ height <| px 0
        , fullWidth
        , Border.color palette.onBackground
        , bottomBorder
        , elevatedShadow
        ]
        Element.none


externClass : String -> Attribute msg
externClass class =
    htmlAttribute <| Html.Attributes.class class


buttonCssIcon : String -> String -> Element msg
buttonCssIcon class description =
    el
        [ height <| px 32
        , width <| px 32
        , alignTop
        , externClass class
        , htmlAttribute <| Html.Attributes.title description
        , Region.description description
        ]
    <|
        none


buttonPadding =
    paddingXY 12 4


button : List (Attribute msg)
button =
    [ buttonPadding
    , Font.color Dracula.white
    , Font.center
    , Background.color Dracula.blue
    , Border.color Dracula.white
    , elevatedShadow
    , Border.rounded 4
    , mouseOver [ Border.innerGlow Dracula.purple 2 ]
    ]


textFieldStyle =
    [ Background.color Dracula.black
    , width <| shrink
    , height <| shrink
    , spacingXY 2 4
    , paddingXY 2 2
    ]


textField :
    List (Attribute msg)
    ->
        { isDisabled : Bool
        , label : String
        , text : String
        , onChange : String -> msg
        , onChangeDisabled : msg
        , placeholder : Maybe (Input.Placeholder msg)
        }
    -> Element msg
textField attrs { isDisabled, label, text, onChange, placeholder, onChangeDisabled } =
    if isDisabled then
        Input.text
            ([ Background.color darkGrey
             , Border.width 1
             , Border.color lightGrey
             , Font.color lightGrey
             , width <| shrink
             , height <| shrink
             , spacingXY 2 4
             , paddingXY 2 2
             ]
                ++ attrs
            )
            { label = Input.labelHidden label
            , onChange = \_ -> onChangeDisabled
            , text = text
            , placeholder = placeholder
            }

    else
        Input.text
            ([ Background.color Dracula.black
             , width <| shrink
             , height <| shrink
             , spacingXY 2 4
             , paddingXY 2 2
             ]
                ++ attrs
            )
            { label = Input.labelHidden label
            , onChange = onChange
            , text = text
            , placeholder = placeholder
            }


clickAndHoldButton : msg -> msg -> Element msg -> String -> Element msg
clickAndHoldButton onMouseDown onMouseUp label description =
    Input.button
        [ Region.description description
        , Element.Events.onMouseDown onMouseDown
        , Element.Events.onMouseUp onMouseUp
        ]
        { label = label, onPress = Nothing }


inflateIcon =
    buttonCssIcon "icon-inflate" "Inflate"


vacuumIcon =
    buttonCssIcon "icon-vacuum" "Vacuum"


releaseIcon =
    buttonCssIcon "icon-release" "Release"


stopIcon =
    buttonCssIcon "icon-stop" "Stop"


inflateButton onMouseDown onMouseUp =
    clickAndHoldButton onMouseDown onMouseUp inflateIcon "Inflate"


vacuumButton onMouseDown onMouseUp =
    clickAndHoldButton onMouseDown onMouseUp vacuumIcon "Vacuum"


releaseButton onMouseDown onMouseUp =
    clickAndHoldButton onMouseDown onMouseUp releaseIcon "Release"


stopButton onMouseDown onMouseUp =
    clickAndHoldButton onMouseDown onMouseUp stopIcon "Stop"


externalClass : String -> Element.Attribute msg
externalClass class =
    htmlAttribute <| Html.Attributes.class class


fullWidth =
    Element.width <| Element.fill


palette =
    { background = Dracula.black
    , primary = Dracula.purple
    , onBackground = Dracula.white
    , onPrimary = Dracula.black
    , secondary = Dracula.green
    , error = Dracula.red
    , onError = Dracula.white
    }


option : Element.Element msg -> OptionState -> Element.Element msg
option content optionState =
    case optionState of
        Idle ->
            el
                [ centerY
                , Background.color palette.background
                , Border.rounded 4
                , Border.width 1
                , Border.color palette.onBackground
                , paddingXY 4 3
                , Font.color palette.onBackground
                ]
                content

        Focused ->
            el
                [ centerY
                , Background.color palette.background
                , Border.rounded 4
                , Border.width 1
                , Border.color palette.onBackground
                , paddingXY 4 3
                , Font.color palette.onBackground
                , Border.innerGlow palette.primary 2
                ]
                content

        Selected ->
            el
                [ centerY
                , Background.color palette.primary
                , Border.rounded 4
                , Border.width 1
                , Border.color palette.onBackground
                , paddingXY 4 3
                , Font.color palette.onPrimary
                ]
                content


optionWithSlider :
    { onChange : Float -> msg
    , labelContent : Element msg
    , label : Element msg -> Input.Label msg
    , min : Float
    , max : Float
    , value : Float
    , thumb : Input.Thumb
    , step : Maybe Float
    , width : Length
    }
    -> OptionState
    -> Element.Element msg
optionWithSlider sliderOptions optionState =
    case optionState of
        Idle ->
            el
                [ centerY
                , Background.color palette.background
                , Border.rounded 4
                , Border.width 1
                , Border.color palette.onBackground
                , paddingXY 4 3
                , Font.color palette.onBackground
                ]
                sliderOptions.labelContent

        Focused ->
            el
                [ centerY
                , Background.color palette.background
                , Border.rounded 4
                , Border.width 1
                , Border.color palette.onBackground
                , paddingXY 4 3
                , Font.color palette.onBackground
                , Border.innerGlow palette.primary 2
                ]
                sliderOptions.labelContent

        Selected ->
            Input.slider
                [ centerY
                , Background.color palette.primary
                , Border.rounded 4
                , Border.width 1
                , Border.color palette.onBackground
                , paddingXY 4 3
                , Font.color palette.onPrimary
                , width sliderOptions.width
                ]
                { onChange = sliderOptions.onChange
                , label = sliderOptions.label sliderOptions.labelContent
                , thumb = sliderOptions.thumb
                , max = sliderOptions.max
                , value = sliderOptions.value
                , step = sliderOptions.step
                , min = sliderOptions.min
                }


colorsNormal =
    [ Background.color palette.background
    , Border.color palette.onBackground
    , Font.color palette.onBackground
    ]


colorsNegative =
    [ Background.color palette.onBackground
    , Border.color palette.primary
    , Font.color palette.background
    ]


colorsPrimary =
    [ Background.color palette.primary
    , Border.color palette.onBackground
    , Font.color palette.onPrimary
    ]


card : List (Attribute msg)
card =
    [ Border.rounded 4
    , Border.width 1
    , paddingXY 4 3
    ]
