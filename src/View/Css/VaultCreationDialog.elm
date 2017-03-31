module View.Css.VaultCreationDialog exposing (..)

import Css exposing (..)
import Css.Colors
import Css.Elements exposing (label, input)
import Css.Namespace exposing (namespace)
import View.Css.Util exposing (..)


namespace : String
namespace =
    "VaultCreationDialog-"


type CssClasses
    = Content
    | FormInput
    | FormLabel


css : Stylesheet
css =
    cssNamespace namespace
        [ content
        , formInput
        ]


content : Snippet
content =
    class Content
        [ width (px 1000)
        , height (px 500)
        ]


formFontSize =
    px 18


formInput : Snippet
formInput =
    class FormInput
        [ fontSize formFontSize
        , descendants
            [ input
                [ fontSize formFontSize
                ]
            , class FormLabel
                [ marginRight (px 10)
                , width (px 300)
                , display inline
                ]
            ]
        ]
