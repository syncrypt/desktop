module Css.Util exposing (..)

import Css
import Css.Namespace
import Html.CssHelpers


namespacedHelpers namespace =
    Html.CssHelpers.withNamespace namespace


transition =
    Css.property "transition"


borderWidth =
    Css.property "border-width"


background =
    Css.property "background"


animation =
    Css.property "animation"


cssNamespace : String -> List Css.Snippet -> Css.Stylesheet
cssNamespace namespace snippets =
    (Css.stylesheet << Css.Namespace.namespace namespace) <| snippets



--keyframes : String -> List String -> Snippet
--keyframes name props =
--    Snippet
--        [ Keyframes name props
--        ]
