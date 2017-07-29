module Animation
    exposing
        ( Animation(..)
        , animation
        , animations
        , loadingSpinner
        , loadingSpinnerIf
        )

import Html exposing (Html, span, div, text)
import Html.Attributes exposing (attribute, class, classList, style)
import Util exposing (Direction)


type Animation
    = SlideIn Direction
    | FadeIn
    | Highlight


animationClass : Animation -> String
animationClass anim =
    case anim of
        SlideIn dir ->
            "Anim-Slide-In-" ++ (toString dir)

        FadeIn ->
            "Anim-Fade-In"

        Highlight ->
            "Anim-Highlight"


animation : Float -> Animation -> Html.Attribute msg
animation duration anim =
    style
        [ ( "animation-name", animationClass anim )
        , ( "animation-duration", (toString duration) ++ "s" )
        ]


animations : Float -> List Animation -> Html.Attribute msg
animations duration anims =
    style
        [ ( "animation-name"
          , anims
                |> List.map animationClass
                |> String.join ", "
          )
        , ( "animation-duration", (toString duration) ++ "s" )
        ]


loadingSpinner : Html msg
loadingSpinner =
    div [ class "Loading-Spinner Anim-Fade-In" ]
        [ text "" ]


loadingSpinnerIf : Bool -> Html msg
loadingSpinnerIf condition =
    div
        [ classList
            [ ( "Loading-Spinner", True )
            , ( "Hidden", not condition )
            ]
        ]
        [ text "" ]