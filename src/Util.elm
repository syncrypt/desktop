module Util
    exposing
        ( (~>)
        , ButtonSettings
        , ByteUnit
        , ByteUnitPrecision
        , CustomButtonSettings
        , Direction(..)
        , Position(..)
        , TooltipLength(..)
        , allButLast
        , andAlso
        , animatedDots
        , attemptDelayed
        , button
        , bytesReadable
        , customButton
        , dateDecoder
        , dateParts
        , delay
        , delayMsg
        , findFirst
        , findIndex
        , fullDateString
        , last
        , monthNumber
        , onAnyKeyDown
        , onEnter
        , onKeyDown
        , padNumber
        , performDelayed
        , removeLeading
        , removeTrailing
        , removeTrailingZeroes
        , sendMsg
        , shortDateString
        , shortenString
        , skipCharsWhile
        , surround
        , tooltipItem
        )

import Date exposing (Date)
import Html exposing (Html, div, span, text)
import Html.Attributes exposing (attribute, class, classList, style)
import Html.Events
import Json.Decode as Json
import Language exposing (Language)
import Process
import Round
import Task exposing (Task, andThen, attempt, perform)
import Time exposing (Time)


sendMsg : msg -> Cmd msg
sendMsg msg =
    Task.succeed msg
        |> Task.perform identity


delayMsg : Time -> msg -> Cmd msg
delayMsg time msg =
    Process.sleep time
        |> Task.perform (\_ -> msg)


{-| Creates a new `Task` that delays a given `Task` by a given time.

    delay 1000 (Task.succeed "Done!")

-}
delay : Time -> Task err a -> Task err a
delay time task =
    Process.sleep time
        |> andThen (\_ -> task)


{-| Attempts to perform a `Task` after a given delay.

    let
        request = Daemon.getVaults model
    in
        attemptDelayed 1000 UpdatedVaultsFromApi request

-}
attemptDelayed : Time -> (Result err a -> msg) -> Task err a -> Cmd msg
attemptDelayed time msg task =
    task
        |> delay time
        |> attempt msg


{-| Attempts to perform a `Task` after a given delay.

    Util.performDelayed 1000 SetTime Time.now

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
                skipCharsWhile f rest
            else
                String.cons c rest


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
                    case removeTrailingZeroes b of
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


bytesReadable_ :
    Float
    -> List ( ByteUnit, ByteUnitPrecision )
    -> ( String, String )
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


surround : (a -> b) -> (b -> a) -> a -> b
surround f g =
    f >> g >> f


last : Int -> List a -> List a
last amount list =
    list
        |> surround List.reverse (List.take amount)


allButLast : List a -> List a
allButLast list =
    list
        |> surround List.reverse (List.drop 1)


findFirst : (a -> Bool) -> List a -> Maybe a
findFirst check list =
    case list of
        [] ->
            Nothing

        val :: rest ->
            if check val then
                Just val
            else
                findFirst check rest


findIndex : (a -> Bool) -> List a -> Maybe Int
findIndex check list =
    list
        |> List.indexedMap (,)
        |> findFirst (\( idx, name ) -> check name)
        |> Maybe.map Tuple.first


{-| Composes multiple (model, Cmd msg) pairs with each other.
This is somewhat similar to Haskell's >>=

    doStuff : Model -> ( Model, Cmd Msg )
    doStuff model =
        ( model, fetchData model )
            ~> doSomething
            ~> doAnotherThing

    fetchData : Model -> Cmd Msg

    doSomething : Model -> ( Model, Cmd Msg )

    doAnotherThing : Model -> ( Model, Cmd Msg )

-}
(~>) : ( m1, Cmd msg ) -> (m1 -> ( m2, Cmd msg )) -> ( m2, Cmd msg )
(~>) modelAndCmd f =
    modelAndCmd
        |> andAlso f
infixl 0 ~>


andAlso : (m1 -> ( m2, Cmd msg )) -> ( m1, Cmd msg ) -> ( m2, Cmd msg )
andAlso f ( m1, cmd1 ) =
    let
        ( m2, cmd2 ) =
            f m1
    in
    ( m2
    , Cmd.batch [ cmd1, cmd2 ]
    )


onEnter : msg -> Html.Attribute msg
onEnter msg =
    onKeyDown 13 msg


onKeyDown : Int -> msg -> Html.Attribute msg
onKeyDown keyCode msg =
    let
        isKey code =
            if code == keyCode then
                Json.succeed msg
            else
                Json.fail ""
    in
    Html.Events.on "keydown" (Json.andThen isKey Html.Events.keyCode)


onAnyKeyDown : msg -> Html.Attribute msg
onAnyKeyDown msg =
    Html.Events.on "keydown"
        (Html.Events.keyCode
            |> Json.andThen (\_ -> Json.succeed msg)
        )


type Direction
    = Up
    | Down
    | ToRight
    | ToLeft


type Position
    = Top
    | Bottom
    | Right
    | Left


type TooltipLength
    = Small
    | Medium
    | Large
    | XLarge
    | Fit
    | Auto


type alias TooltipConfig =
    { position : Position, length : TooltipLength, text : String }


tooltipItem : TooltipConfig -> List (Html msg) -> Html msg
tooltipItem { position, length, text } body =
    let
        baseAttrs =
            [ attribute "data-balloon" text
            , attribute "data-balloon-pos" (tooltipPositionString position)
            , class "Tooltip"
            ]

        attributes =
            case length of
                Auto ->
                    baseAttrs

                _ ->
                    attribute "data-balloon-length"
                        (tooltipLengthString length)
                        :: baseAttrs
    in
    span attributes body


tooltipPositionString : Position -> String
tooltipPositionString position =
    case position of
        Top ->
            "up"

        Bottom ->
            "down"

        Left ->
            "left"

        Right ->
            "right"


tooltipLengthString : TooltipLength -> String
tooltipLengthString length =
    case length of
        Small ->
            "small"

        Medium ->
            "medium"

        Large ->
            "large"

        XLarge ->
            "xlarge"

        Fit ->
            "fit"

        Auto ->
            ""


{-| Decodes an `Maybe Date` from a string.
-}
dateDecoder : Json.Decoder (Maybe Date)
dateDecoder =
    let
        convert : String -> Json.Decoder (Maybe Date)
        convert raw =
            case Date.fromString raw of
                Ok date ->
                    Json.succeed (Just date)

                Err error ->
                    Json.succeed Nothing
    in
    Json.string
        |> Json.andThen convert


shortenString maxSize string =
    if String.length string > maxSize then
        String.left maxSize string ++ "..."
    else
        string


padNumber : Int -> String
padNumber val =
    if val >= 10 then
        toString val
    else
        "0" ++ toString val


dateParts date =
    ( Date.year date
    , Date.month date
    , Date.day date
    , Date.hour date
    , Date.minute date
    , Date.second date
    )


monthNumber : Date.Month -> Int
monthNumber month =
    case month of
        Date.Jan ->
            1

        Date.Feb ->
            2

        Date.Mar ->
            3

        Date.Apr ->
            4

        Date.May ->
            5

        Date.Jun ->
            6

        Date.Jul ->
            7

        Date.Aug ->
            8

        Date.Sep ->
            9

        Date.Oct ->
            10

        Date.Nov ->
            11

        Date.Dec ->
            12


fullDateString date =
    let
        ( year, month, day, hour, minute, second ) =
            dateParts date
    in
    (year |> toString)
        ++ "/"
        ++ (month |> monthNumber |> padNumber)
        ++ "/"
        ++ (day |> padNumber)
        ++ " "
        ++ (hour |> padNumber)
        ++ ":"
        ++ (minute |> padNumber)
        ++ ":"
        ++ (second |> padNumber)


shortDateString date =
    let
        ( _, _, _, hour, minute, second ) =
            dateParts date
    in
    (hour |> padNumber)
        ++ ":"
        ++ (minute |> padNumber)
        ++ ":"
        ++ (second |> padNumber)


animatedDots : Maybe Date -> String
animatedDots maybeNow =
    case maybeNow of
        Just now ->
            case Date.second now % 4 of
                0 ->
                    ""

                1 ->
                    "."

                2 ->
                    ".."

                _ ->
                    "..."

        Nothing ->
            "."


type alias ButtonSettings msg =
    { label : String
    , onClick : msg
    }


button : List (Html.Attribute msg) -> ButtonSettings msg -> Html msg
button attrs settings =
    span attrs
        [ Html.button [ Html.Events.onClick settings.onClick ]
            [ text settings.label ]
        ]


type alias CustomButtonSettings msg =
    { label : String, onClick : msg, disabled : Bool }


customButton : List (Html.Attribute msg) -> CustomButtonSettings msg -> Html msg
customButton attributes { label, onClick, disabled } =
    let
        attrs =
            if disabled then
                class "Disabled" :: attributes
            else
                attributes
    in
    button attrs
        { label = label
        , onClick = onClick
        }
