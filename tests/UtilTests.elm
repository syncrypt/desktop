module UtilTests exposing (..)

import Expect
import Fuzz
import Test exposing (..)
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


bytesReadable : Test
bytesReadable =
    describe "Util.bytesReadable"
        [ test "returns bytes strings" <|
            \_ ->
                999
                    |> Util.bytesReadable
                    |> Expect.equal "999 Bytes"
        , test "returns kB strings" <|
            \_ ->
                (1024.0 * 10.234)
                    |> round
                    |> Util.bytesReadable
                    |> Expect.equal "10 kB"
        , test "returns MB strings" <|
            \_ ->
                (1024.0 * 1024.0 * 10.234)
                    |> round
                    |> Util.bytesReadable
                    |> Expect.equal "10.2 MB"
        , test "returns GB strings" <|
            \_ ->
                (1024.0 * 1024.0 * 1024.0 * 10.234)
                    |> round
                    |> Util.bytesReadable
                    |> Expect.equal "10.23 GB"
        ]
