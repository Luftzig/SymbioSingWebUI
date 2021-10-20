module Styles exposing (..)

import Element exposing (Attribute, Color, Element, alignTop, el, fill, height, htmlAttribute, none, px, rgb, rgb255, rgba, spacing, width)
import Element.Background as Background
import Element.Border as Border
import Element.Events
import Element.Font as Font
import Element.Input as Input
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


transparent : Color
transparent =
    rgba 0 0 0 0


white : Color
white =
    rgb 1 1 1


bottomBorder =
    Border.widthEach { bottom = 2, left = 0, right = 0, top = 0 }


rightBorder =
    Border.widthEach { bottom = 0, left = 0, right = 2, top = 0 }


externClass : String -> Attribute msg
externClass class =
    htmlAttribute <| Html.Attributes.class class


buttonCssIcon : String -> Element msg
buttonCssIcon class =
    el [ height <| px 32, width <| px 32, alignTop, externClass class ] <| none


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
                ++ [ Background.color lightGrey
                   , Border.width 1
                   , Border.color grey
                   , width <| fill
                   , Font.color grey
                   , height <| fill
                   ]
            )
            { label = Input.labelHidden label
            , onChange = \_ -> onChangeDisabled
            , text = text
            , placeholder = placeholder
            }

    else
        Input.text attrs
            { label = Input.labelHidden label
            , onChange = onChange
            , text = text
            , placeholder = placeholder
            }
