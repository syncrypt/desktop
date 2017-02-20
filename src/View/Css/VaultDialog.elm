module View.Css.VaultDialog exposing (..)

import Css exposing (..)
import Css.Colors
import Css.Elements exposing (canvas, hr)
import Css.Namespace exposing (namespace)
import Model exposing (..)
import Syncrypt.Vault exposing (FlyingVault, Status(..), Vault)
import View.Css.Util exposing (..)


classNamespace =
    "VaultDialog-"


type CssClasses
    = Modal
    | Content
    | Close
    | Hidden
    | Visible


type CssIds
    = VaultDialogModal


css : Stylesheet
css =
    (stylesheet << namespace classNamespace) <|
        [ modal
        , hidden
        , visible
        , content
        , close
        ]


modal : Snippet
modal =
    class Modal
        [ position fixed
        , zIndex (int 5)
        , left (px 0)
        , top (px 0)
        , width (pct 100)
        , height (pct 100)
        , overflow auto
        , backgroundColor (rgb 0 0 0)
        , backgroundColor (rgba 0 0 0 0.5)
        ]


hidden : Snippet
hidden =
    class Hidden
        [ display block
        , height (pct 0)
        , opacity (num 0.0)
        ]


visible : Snippet
visible =
    class Visible
        [ display block
        , height (pct 100)
        , opacity (num 1.0)
        , transition "opacity 0.25s"
        ]


content : Snippet
content =
    class Content
        [ backgroundColor (hex "fefefe")
        , margin (pct 5)
        , padding (px 20)
        , border3 (px 1) solid (hex "888")
        , width (pct 87.5)
        , height (pct 75)
        ]


close : Snippet
close =
    let
        onHoverOrFocus =
            [ color Css.Colors.black
            , textDecoration none
            , cursor pointer
            ]
    in
        class Close
            [ color (hex "aaa")
            , float right
            , fontSize (px 28)
            , fontWeight bold
            , backgroundImage (url "../assets/remove_vault.png")
            , position relative
            , right (px -10)
            , top (px -10)
            , width (px 50)
            , height (px 50)
            , backgroundSize2 (px 50) (px 50)
            , backgroundRepeat noRepeat
            , mixin
                [ hover onHoverOrFocus
                , focus onHoverOrFocus
                ]
            ]
