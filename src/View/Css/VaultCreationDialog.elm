module View.Css.VaultCreationDialog exposing (..)

import Css exposing (..)
import Css.Colors
import Css.Elements exposing (form, input, label)
import Css.Namespace exposing (namespace)
import View.Css.Util exposing (..)


namespace : String
namespace =
    "VaultCreationDialog-"


type CssClasses
    = Content


css : Stylesheet
css =
    cssNamespace namespace
        [ content
        ]


content : Snippet
content =
    class Content
        [ width (px 1000)
        , height (px 500)
        ]
