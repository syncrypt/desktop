module View.Css.VaultDialog exposing (..)

import Css exposing (..)
import View.Css.Util exposing (..)


namespace : String
namespace =
    "VaultDialog-"


type CssClasses
    = Modal
    | Content


type CssIds
    = VaultDialogModal


css : Stylesheet
css =
    cssNamespace namespace <|
        [ modal
        , content
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


content : Snippet
content =
    class Content
        [ width (pct 87.5)
        , height (px 500)
        ]
