module UtilTests exposing (..)

import Test exposing (..)
import Expect
import Util
import Fuzz


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
        , fuzz Fuzz.string "skips all leading whitespace from random string" <|
            \str ->
                ("    " ++ str)
                    |> Util.removeLeading ' '
                    |> Expect.equal (str |> Util.removeLeading ' ')
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
        , fuzz Fuzz.string "skips all trailing whitespace from random string" <|
            \str ->
                (str ++ "    ")
                    |> Util.removeTrailing ' '
                    |> Expect.equal (str |> Util.removeTrailing ' ')
        ]
