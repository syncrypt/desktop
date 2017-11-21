module Animation
    exposing
        ( Animation(..)
        , LoadingCircleSize(..)
        , animation
        , animations
        , loadingCircle
        , loadingSpinner
        , loadingSpinnerIf
        )

import Date exposing (Date)
import Html exposing (Html, div, span, text)
import Html.Attributes exposing (attribute, class, classList, style)
import Svg exposing (polygon, rect, svg)
import Svg.Attributes as SvgA
import Util exposing (Direction)
import Window


type Animation
    = SlideIn Direction
    | FadeIn
    | FadeInFast
    | Highlight


animationClass : Animation -> String
animationClass anim =
    case anim of
        SlideIn dir ->
            "Anim-SlideIn" ++ toString dir

        name ->
            "Anim-" ++ toString name


animation : Float -> Animation -> Html.Attribute msg
animation duration anim =
    style
        [ ( "animation-name", animationClass anim )
        , ( "animation-duration", toString duration ++ "s" )
        ]


animations : Float -> List Animation -> Html.Attribute msg
animations duration anims =
    style
        [ ( "animation-name"
          , anims
                |> List.map animationClass
                |> String.join ", "
          )
        , ( "animation-duration", toString duration ++ "s" )
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


type LoadingCircleSize
    = SmallCircle
    | MediumCircle
    | LargeCircle


loadingCircle : LoadingCircleSize -> { m | now : Maybe Date, windowSize : Window.Size } -> Html msg
loadingCircle circleSize model =
    let
        secs =
            model.now
                |> Maybe.map Date.second
                |> Maybe.withDefault 0

        ( color1, color2 ) =
            case model.now of
                Just now ->
                    if secs % 10 < 5 then
                        ( "#3AE2E2", "#4D4D4D" )
                    else
                        ( "#4D4D4D", "#3AE2E2" )

                Nothing ->
                    ( "#3AE2E2", "#4D4D4D" )

        rBase =
            sin (toFloat secs) + 9

        sizeFactor =
            case circleSize of
                SmallCircle ->
                    9.0

                MediumCircle ->
                    11.0

                LargeCircle ->
                    19.0

        r1 =
            sizeFactor
                * rBase
                + (5 * tan (toFloat secs))
                + (10 * sin (cos (toFloat secs)))

        r2 =
            (sizeFactor - 1.0)
                * rBase
                + (20 * cos (toFloat secs))

        ( cx, cy ) =
            ( toFloat model.windowSize.width / 2.0
            , toFloat model.windowSize.height / 2.0
            )

        ( widthStr, heightStr ) =
            ( toString model.windowSize.width
            , toString model.windowSize.height
            )

        circle bgColor radius =
            Svg.circle
                [ SvgA.fill bgColor
                , SvgA.class "LoadingCircle"
                , SvgA.cx <| toString cx
                , SvgA.cy <| toString cy
                , SvgA.r <| toString radius
                , SvgA.width widthStr
                , SvgA.height heightStr
                ]
                []
    in
    svg
        [ SvgA.version "1.1"
        , SvgA.x "0"
        , SvgA.y "0"
        , SvgA.viewBox <| "0 0 " ++ widthStr ++ " " ++ heightStr
        ]
        [ circle color1 r1
        , circle color2 r2
        , Svg.text_
            [ SvgA.x <| toString <| cx - 35
            , SvgA.y <| toString <| cy + 5.0
            , SvgA.fontSize "22px"
            , SvgA.fontFamily "Hind"
            , SvgA.fontWeight "500"
            , SvgA.fill color1
            , SvgA.class "LoadingCircle"
            ]
            [ Svg.text "Loading" ]
        ]
