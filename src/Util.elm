module Util
    exposing
        ( ByteUnit
        , ByteUnitPrecision
        , delay
        , attemptDelayed
        , skipCharsWhile
        , removeTrailingZeroes
        , bytesReadable
        )

import Time exposing (Time)
import Task exposing (Task, andThen, attempt)
import Process
import Model exposing (Msg)
import Result exposing (Result)
import Round


{-| Creates a new `Task` that delays a given `Task` by a given time.

    delay 1000 (Task.succeed "Done!")
-}
delay : Time -> Task err a -> Task err a
delay time task =
    Process.sleep 1000
        |> andThen (\x -> task)


{-| Attempts to perform a `Task` after a given delay.

    let
        request = Daemon.getVaults model.config
    in
        attemptDelayed 1000 UpdatedVaultsFromApi request
-}
attemptDelayed : Time -> (Result err a -> Msg) -> Task err a -> Cmd Msg
attemptDelayed time msg task =
    task
        |> delay time
        |> attempt msg


type alias ByteUnit =
    String


type alias ByteUnitPrecision =
    Int


byteUnitsWithPrecision : List ( ByteUnit, ByteUnitPrecision )
byteUnitsWithPrecision =
    [ ( "Bytes", 0 )
    , ( "kB", 0 )
    , ( "MB", 1 )
    , ( "GB", 2 )
    , ( "TB", 2 )
    , ( "PB", 2 )
    , ( "EB", 2 )
    , ( "ZB", 2 )
    , ( "YB", 2 )
    ]


skipCharsWhile : (Char -> Bool) -> String -> String
skipCharsWhile f string =
    case string |> String.uncons of
        Nothing ->
            ""

        Just ( c, rest ) ->
            if f c then
                rest
            else
                String.cons c (skipCharsWhile f rest)


removeLeading : Char -> String -> String
removeLeading char string =
    string
        |> skipCharsWhile (\c -> c == char)


removeTrailing : Char -> String -> String
removeTrailing char string =
    string
        |> String.reverse
        |> removeLeading char
        |> String.reverse


removeTrailingZeroes : String -> String
removeTrailingZeroes =
    removeTrailing '0'


{-| Returns a human readable version of a storage size in bytes.

    bytesReadable 100  -- -> "100 bytes"
    bytesReadable 1024 -- -> "1 kB"
    bytesReadable 2048 -- -> "2 kb"
    mb n = 1024.0 * 1024.0 * n
    bytesReadable (round (mb 42.3)) -- -> "42.3 MB"
-}
bytesReadable : Int -> String
bytesReadable x =
    let
        ( sizeStr, unit ) =
            bytesReadable_ (toFloat x) byteUnitsWithPrecision

        trimmedSizeStr =
            case String.split "." sizeStr of
                [ a, b ] ->
                    case (removeTrailingZeroes b) of
                        "" ->
                            a

                        b ->
                            a ++ "." ++ b

                _ ->
                    sizeStr
    in
        case unit of
            "" ->
                trimmedSizeStr

            _ ->
                trimmedSizeStr ++ " " ++ unit


bytesReadable_ : Float -> List ( ByteUnit, ByteUnitPrecision ) -> ( String, String )
bytesReadable_ size units =
    -- find first unit where
    case units of
        [] ->
            -- I doubt this will happen any time soon
            ( Round.round 2 size, "YB" )

        [ ( unit, precision ) ] ->
            -- only one unit left, pick it
            ( Round.round precision size, unit )

        ( unit, precision ) :: units ->
            let
                rem =
                    size / 1024
            in
                if size < 1 || rem < 1 then
                    ( Round.round precision size, unit )
                else
                    bytesReadable_ rem units
