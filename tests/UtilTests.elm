module UtilTests exposing (..)

import Test exposing (..)
import Expect
import Util


skipCharsWhile : Test
skipCharsWhile =
    describe "Util.skipCharsWhile"
        [ test "skips all whitespace" <|
            \_ ->
                "   foo  bar  baz"
                    |> Util.skipCharsWhile (\c -> c == ' ')
                    |> Expect.equal "foo  bar  baz"
        ]


removeLeading : Test
removeLeading =
    describe "Util.removeLeading"
        [ test "removes leading character from string" <|
            \_ ->
                "afoo"
                    |> Util.removeLeading 'a'
                    |> Expect.equal "foo"
        , test "removes leading whitespace" <|
            \_ ->
                "  hello  "
                    |> Util.removeLeading ' '
                    |> Expect.equal "hello  "
        ]


removeTrailing : Test
removeTrailing =
    describe "Util.removeTrailing"
        [ test "removes trailing character from string" <|
            \_ ->
                "afoo"
                    |> Util.removeTrailing 'o'
                    |> Expect.equal "af"
        , test "removes trailing whitespace" <|
            \_ ->
                "  hello   "
                    |> Util.removeTrailing ' '
                    |> Expect.equal "  hello"
        ]
