module VaultDialog.Css exposing (..)

import Css exposing (..)
import View.Css.Util exposing (..)


namespace : String
namespace =
    "VaultDialog-"


type CssClasses
    = Content


type CssIds
    = VaultDialogModal


css : Stylesheet
css =
    cssNamespace namespace <|
        [ content
        ]


content : Snippet
content =
    class Content
        [ width (pct 87.5)
        , height (px 500)
        ]
