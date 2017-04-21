module Util
    exposing
        ( ByteUnit
        , ByteUnitPrecision
        , delay
        , attemptDelayed
        , performDelayed
        , skipCharsWhile
        , removeTrailingZeroes
        , bytesReadable
        , surround
        , last
        , allButLast
        )

import Time exposing (Time)
import Task exposing (Task, andThen, attempt, perform)
import Process
import Result exposing (Result)
import Round


{-| Creates a new `Task` that delays a given `Task` by a given time.

    delay 1000 (Task.succeed "Done!")
-}
delay : Time -> Task err a -> Task err a
delay time task =
    Process.sleep time
        |> andThen (\_ -> task)


{-| Attempts to perform a `Task` after a given delay.

    let
        request = Daemon.getVaults model.config
    in
        attemptDelayed 1000 UpdatedVaultsFromApi request
-}
attemptDelayed : Time -> (Result err a -> msg) -> Task err a -> Cmd msg
attemptDelayed time msg task =
    task
        |> delay time
        |> attempt msg


{-| Attempts to perform a `Task` after a given delay.

    Util.performDelayed 1000 SetDate Date.now
-}
performDelayed : Time -> (a -> msg) -> Task Never a -> Cmd msg
performDelayed time msg task =
    task
        |> delay time
        |> perform msg


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


surround : (List a -> List b) -> (List b -> List a) -> List a -> List b
surround f body val =
    val
        |> f
        |> body
        |> f


last : Int -> List a -> List a
last amount list =
    list
        |> surround List.reverse (List.take amount)


allButLast : List a -> List a
allButLast list =
    list
        |> surround List.reverse (List.take 1)
