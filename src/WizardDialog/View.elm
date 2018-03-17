module WizardDialog.View
    exposing
        ( infoText
        , infoTextWithHeader
        , infoTextWithHeaders
        )

import Html exposing (Html, div, span, text)
import Html.Attributes exposing (class)


infoTextLine : String -> Html msg
infoTextLine line =
    span []
        [ text line ]


infoText : List (Html.Attribute msg) -> List String -> Html msg
infoText attrs lines =
    div [ class "InfoText" ]
        [ div attrs
            (List.map infoTextLine lines)
        ]


infoTextWithHeader : List (Html.Attribute msg) -> String -> List String -> Html msg
infoTextWithHeader attrs header lines =
    div [ class "InfoText" ]
        [ div attrs <|
            span [ class "Header" ]
                [ text header ]
                :: List.map infoTextLine lines
        ]


infoTextWithHeaders : List (Html.Attribute msg) -> String -> String -> List String -> Html msg
infoTextWithHeaders attrs header subHeader lines =
    div [ class "InfoText" ]
        [ div attrs <|
            span [ class "Header" ]
                [ text header ]
                :: span [ class "SubHeader" ]
                    [ text subHeader ]
                :: List.map infoTextLine lines
        ]
