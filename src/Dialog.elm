module Dialog exposing (..)

import Html exposing (Html, div, span, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Ui.Modal


type alias WithModalState a =
    { a | modal : Ui.Modal.Model }


type LabelSide
    = Left
    | Right


asModalIn : WithModalState a -> Ui.Modal.Model -> WithModalState a
asModalIn state modal =
    { state | modal = modal }


labeled : LabelSide -> List (Html.Attribute msg) -> Maybe msg -> String -> Html msg -> Html msg
labeled side attributes onClickMsg labelString content =
    let
        className =
            case side of
                Left ->
                    "Dialog-Label-Left"

                Right ->
                    "Dialog-Label-Right"

        attrs =
            case onClickMsg of
                Nothing ->
                    (class "Default-Cursor") :: attributes

                Just msg ->
                    (onClick msg) :: attributes

        label =
            span (class className :: attrs)
                [ text labelString ]
    in
        orderedLabeling side label content


labeledRight =
    labeled Right


labeledLeft =
    labeled Left


orderedLabeling side label content =
    let
        labelBody =
            case side of
                Left ->
                    [ label, content ]

                Right ->
                    [ content, label ]
    in
        span []
            labelBody
