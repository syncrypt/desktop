module View.Css.MainScreen exposing (..)

import Css exposing (..)
import Css.Namespace exposing (namespace)
import Css.Elements exposing (h2)
import Model exposing (..)
import Syncrypt.Vault exposing (Status(..), Vault)
import View.Css.Util exposing (..)


type alias CssMixin =
    List Css.Mixin -> Css.Snippet


type CssClass
    = MainScreen
    | Expanded
    | Container
    | Header
    | Footer
    | FlyingVaultList
    | Buttons
    | Element CssMixin


css : Stylesheet
css =
    (stylesheet << namespace "MainScreenView-")
        [ mainScreen
        , expanded
        , container
        , expandedContainer
        , header
        , footer
        , class [ Header, Element h2 ]
            [ fontSize (Css.rem 3.0)
            ]
        ]


mainScreen : Snippet
mainScreen =
    class MainScreen
        [ width (pct 70)
        , property "-webkit-transition" "width 0.5s"
        ]


expanded : Snippet
expanded =
    class Expanded
        [ width (pct 100)
        , property "-webkit-transition" "width 0.5s"
        ]


container : Snippet
container =
    class Container
        [ width (pct 70)
        , height (pct 100)
        , position absolute
        , top (px 64)
        , bottom (px 26)
        , overflowY auto
        , transition "width 0.5s"
        , background "linear-gradient(0deg, #404f60, #c17d6f)"
        ]


expandedContainer : Snippet
expandedContainer =
    class [ Expanded, Container ]
        [ width (pct 100)
        , transition "width 0.5s"
        ]


header : Snippet
header =
    class Header
        [ height (px 64)
        , position relative
        , backgroundColor (hex "a07f78")
        , marginBottom (px 10)
        , backgroundImage (url "../assets/logo.png")
        , backgroundRepeat noRepeat
        , backgroundPosition2 (px 20) (px 2)
        , backgroundSize (px 170)
        , width (pct 100)
        ]


footer : Snippet
footer =
    class Footer
        []
