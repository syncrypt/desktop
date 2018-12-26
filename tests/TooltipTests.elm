module TooltipTests exposing (..)

import Dict
import Expect
import Fuzz
import Language
import Test exposing (..)
import Tooltip exposing (Length(..), Tooltip)
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


baseModel =
    { tooltips = Tooltip.emptyTooltips, language = Language.German }


text : Test
text =
    let
        tip =
            ttip "test1" Foo
    in
    describe "Tutorial.text"
        [ test "returns the Tooltip's text" <|
            \_ ->
                tip
                    |> Tooltip.text
                    |> Expect.equal Foo
        ]


activateAndDeactivate : Test
activateAndDeactivate =
    let
        tip1 =
            ttip "test1" Foo

        tip2 =
            ttip "test2" Bar

        tipId1 =
            Tooltip.id tip1

        tipId2 =
            Tooltip.id tip2

        model =
            baseModel
                |> Tooltip.add tip1
                |> Tooltip.add tip2
    in
    describe "Tutorial.activate & Tooltip.deactivate"
        [ test "activates a Tooltip" <|
            \_ ->
                model
                    |> Tooltip.deactivate tipId1
                    |> Tooltip.activate tipId1
                    |> Tooltip.isActive tipId1
                    |> Expect.true "tooltip isActive"
        , test "deactivates a Tooltip" <|
            \_ ->
                model
                    |> Tooltip.activate tipId2
                    |> Tooltip.deactivate tipId2
                    |> Tooltip.isActive tipId2
                    |> Expect.false "tooltip isActive"
        ]


hasTooltip : Test
hasTooltip =
    let
        tip1 =
            ttip "test1" Baz

        tip2 =
            ttip "test2" Foo
    in
    describe "Tutorial.hasTooltip"
        [ test "returns true if the given model has a tooltip with the given id" <|
            \_ ->
                baseModel
                    |> Tooltip.add tip1
                    |> Tooltip.hasTooltip (Tooltip.id tip1)
                    |> Expect.true "hasTooltip"
        , test "returns false if the given model doesn't have any tooltips" <|
            \_ ->
                ( baseModel
                    |> Tooltip.hasTooltip (Tooltip.id tip1)
                , baseModel
                    |> Tooltip.hasTooltip (Tooltip.id tip2)
                )
                    |> Expect.equal ( False, False )
        , test "returns false if the given model doesn't have a tooltip with the given id" <|
            \_ ->
                baseModel
                    |> Tooltip.add tip1
                    |> Tooltip.hasTooltip (Tooltip.id tip2)
                    |> Expect.false "hasTooltip"
        ]


length : Test
length =
    describe "Tooltip.length"
        [ test "returns the assigned length" <|
            \_ ->
                let
                    lengths =
                        [ Small, Medium, Large, XLarge, Fit, Auto ]
                in
                lengths
                    |> List.indexedMap
                        (\i length ->
                            Tooltip.init ("test" ++ toString i)
                                { text = Bar
                                , visibleTime = Util.forever
                                , position = Top
                                , length = length
                                }
                        )
                    |> List.map Tooltip.length
                    |> Expect.equal lengths
        ]
