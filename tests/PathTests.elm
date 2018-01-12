module PathTests exposing (..)

import Expect
import Path
import Test exposing (..)


isNestedUnder : Test
isNestedUnder =
    describe "Path.isNestedUnder"
        [ test "returns True if a Path is nested under another Path" <|
            \_ ->
                [ "foo", "bar", "baz" ]
                    |> Path.isNestedUnder [ "foo", "bar" ]
                    |> Expect.equal True
        , test "returns True if paths are identical" <|
            \_ ->
                [ "foo", "bar", "baz" ]
                    |> Path.isNestedUnder [ "foo", "bar", "baz" ]
                    |> Expect.equal True
        , test "returns False if a Path is not nested under another Path" <|
            \_ ->
                [ "foo", "bar", "baz" ]
                    |> Path.isNestedUnder [ "foo", "baz" ]
                    |> Expect.equal False
        , test "returns False if a Path is empty " <|
            \_ ->
                []
                    |> Path.isNestedUnder [ "foo", "baz" ]
                    |> Expect.equal False
        ]
