module Styles exposing (..)

import Color.Dracula as Dracula
import Element exposing (Attribute, Color, Element, alignTop, el, fill, height, htmlAttribute, none, px, rgb, rgb255, rgba, spacing, width)
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
            (attrs
                ++ [ Background.color darkGrey
                   , Border.width 1
                   , Border.color lightGrey

                   --, width <| fill
                   , Font.color lightGrey

                   --, height <| fill
                   ]
            )
            { label = Input.labelHidden label
            , onChange = \_ -> onChangeDisabled
            , text = text
            , placeholder = placeholder
            }

    else
        Input.text (attrs ++ [ Background.color Dracula.black ])
            { label = Input.labelHidden label
            , onChange = onChange
            , text = text
            , placeholder = placeholder
            }


inflateIcon =
    buttonCssIcon "icon-inflate" "Inflate"


inflateButton : Maybe msg -> Element msg
inflateButton action =
    Input.button [ Region.description "Inflate" ] { label = inflateIcon, onPress = action }


vacuumIcon =
    buttonCssIcon "icon-vacuum" "Vacuum"


vacuumButton : Maybe msg -> Element msg
vacuumButton action =
    Input.button [ Region.description "Vacuum" ] { label = vacuumIcon, onPress = action }


releaseIcon =
    buttonCssIcon "icon-release" "Release"


releaseButton : Maybe msg -> Element msg
releaseButton action =
    Input.button [ Region.description "Release" ] { label = releaseIcon, onPress = action }


stopIcon =
    buttonCssIcon "icon-stop" "Stop"


stopButton : Maybe msg -> Element msg
stopButton action =
    Input.button [ Region.description "Stop" ] { label = stopIcon, onPress = action }
