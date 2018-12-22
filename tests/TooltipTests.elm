module TooltipTests exposing (..)

import Expect
import Fuzz
import Test exposing (..)
import Tooltip exposing (Tooltip, TooltipLength(..))
import Util exposing (Position(..))


type Text
    = Foo
    | Bar
    | Baz


translate : Text -> String
translate t =
    toString t


ttip : String -> Text -> Tooltip Text
ttip id text =
    Tooltip.init id
        { text = text
        , visibleTime = 100
        , position = Top
        , length = Auto
        }


text : Test
text =
    let
        tip =
            ttip "test1" Foo
    in
    describe "Tutorial.id"
        [ test "goes to next step" <|
            \_ ->
                tip
                    |> Tooltip.text
                    |> Expect.equal Foo
        ]
