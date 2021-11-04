module Styles exposing (..)

import Color.Dracula as Dracula
import Element exposing (Attribute, Color, Element, alignTop, el, fill, height, htmlAttribute, none, paddingXY, px, rgb, rgb255, rgba, shrink, spacing, spacingXY, width)
import Element.Background as Background
import Element.Border as Border
import Element.Events
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Html.Attributes
import Maybe.Extra


rust =
    rgb255 183 65 14


grey : Color
grey =
    rgb 0.5 0.5 0.5


lightGrey : Color
lightGrey =
    rgb 0.8 0.8 0.8


darkGrey : Color
darkGrey =
    rgb 0.2 0.2 0.2


bottomBorder =
    Border.widthEach { bottom = 2, left = 0, right = 0, top = 0 }


rightBorder =
    Border.widthEach { bottom = 0, left = 0, right = 2, top = 0 }


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
