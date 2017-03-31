module View.Css.MainScreen exposing (..)

import Css exposing (..)
import Css.Namespace exposing (namespace)
import Css.Elements exposing (body, h1, h2, a, p, li, div)
import View.Css.Util exposing (..)


namespace : String
namespace =
    "MainScreen-"


type alias CssMixin =
    List Css.Mixin -> Css.Snippet


type CssClass
    = Root
    | MainScreen
    | Expanded
    | Container
    | ModalContent
    | Header
    | Footer
    | FlyingVaultList
    | Buttons
    | Stats


css : Stylesheet
css =
    cssNamespace namespace <|
        general
            ++ [ mainScreen
               , header
               , footer
               , stats
               , buttons
               ]


general =
    [ body
        [ position relative
        , color (hex "333")
        , height (vh 100)
        , backgroundColor (hex "fff")
        , overflowY hidden
        , fontFamilies [ "Hind", "Arial", "Helvetica", "Helvetica Neue" ]
        , margin (px 0)
        , background "linear-gradient(0deg, #404f60, #c17d6f)"
        , backgroundAttachment fixed
        ]
    , h2
        [ margin (px 0)
        , fontSize (Css.rem 2.25)
        , fontWeight bold
        , letterSpacing (Css.rem -0.25)
        , color (hex "333")
        ]
    , p [ fontSize (px 24) ]
    , li [ listStyle none ]
    , a
        [ color (hex "555")
        , opacity (num 0.75)
        , textDecoration none
        , linkHover
        ]
    , Css.id Root (fullSize ++ [ children [ div fullSize ] ])
    , class Container
        [ width (pct 100)
        , paddingTop (px 30)
        , paddingBottom (px 45)
        ]
    , class ModalContent
        [ borderRadius (px 0) ]
    ]


fullSize : List Mixin
fullSize =
    [ height (pct 100), width (pct 100) ]


linkHover : Mixin
linkHover =
    mixin
        [ hover
            [ opacity (num 1)
            , textDecoration none
            , cursor pointer
            ]
        ]



--
-- ::-webkit-input-placeholder {
--    color: #4D4D4D !important;
-- }
--


mainScreen : Snippet
mainScreen =
    class MainScreen
        [ width (pct 70)
        , property "-webkit-transition" "width 0.5s"
        , descendants
            [ class Expanded
                [ width (pct 100)
                , transition "width 0.5s"
                , children
                    [ class Container
                        [ width (pct 100)
                        , transition "width 0.5s"
                        ]
                    ]
                ]
            ]
        , children
            [ class Container
                [ width (pct 70)
                , height (pct 100)
                , position absolute
                , top (px 64)
                , bottom (px 26)
                , overflowY auto
                , transition "width 0.5s"
                ]
            ]
        ]


header : Snippet
header =
    class Header
        [ position fixed
        , top (px 0)
        , zIndex (int 1)
        , height (px 64)
        , width (pct 100)
        , backgroundColor (hex "a07f78")
        , marginBottom (px 10)
        , backgroundImage (url "../assets/logo.png")
        , backgroundRepeat noRepeat
        , backgroundPosition2 (px 20) (px 2)
        , backgroundSize (px 170)
        , descendants
            [ h1
                [ -- fontSize (Css.rem 3.0)
                  fontSize (px 100)
                , color (hex "f00")
                  -- TODO: remove this
                ]
            , a
                [ fontSize (Css.rem 1.4)
                ]
            ]
        ]


stats : Snippet
stats =
    class Stats
        [ position absolute
        , right (px 20)
        , top (px 7)
        ]


buttons : Snippet
buttons =
    class Buttons
        [ position absolute
        , right (px 10)
        , top (px 17)
          -- , display flex
        ]


footer : Snippet
footer =
    class Footer
        [ position fixed
        , zIndex (int 1)
        , fontSize (px 14)
        , left (px 0)
        , right (px 0)
        , bottom (px 0)
        , height (px 20)
        , backgroundColor (hex "4d4d4d")
        , color (hex "fff")
        , padding (px 5)
        ]
