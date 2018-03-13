module Data.Daemon
    exposing
        ( DaemonConfig
        , GUIConfig
        , KeyState(..)
        , Stats
        , daemonConfigDecoder
        , guiConfigDecoder
        , languageDecoder
        )

import Data.Vault
import Dict exposing (Dict)
import Json.Decode as Json exposing (fail, succeed)
import Json.Decode.Pipeline exposing (decode, required)
import Language exposing (Language(..))


type KeyState
    = Uninitialized
    | Initializing
    | Initialized


type alias Stats =
    { stats : Int
    , downloads : Int
    , uploads : Int
    , userKeyState : KeyState
    , totalSlots : Int
    , busySlots : Int
    , idleSlots : Int
    , closedSlots : Int
    , states : Dict String Data.Vault.Status
    }


type alias GUIConfig =
    { isFirstLaunch : Bool
    , language : Language
    }


type alias DaemonConfig =
    { gui : GUIConfig
    }


daemonConfigDecoder : Json.Decoder DaemonConfig
daemonConfigDecoder =
    decode DaemonConfig
        |> required "gui" guiConfigDecoder


guiConfigDecoder : Json.Decoder GUIConfig
guiConfigDecoder =
    decode GUIConfig
        |> required "is_first_launch" Json.bool
        |> required "language" languageDecoder


languageDecoder : Json.Decoder Language
languageDecoder =
    let
        convert : String -> Json.Decoder Language
        convert language =
            case String.toLower language of
                "english" ->
                    succeed English

                "german" ->
                    succeed German

                val ->
                    fail <|
                        "Invalid language configured in syncrypt config: "
                            ++ toString val
    in
    Json.string
        |> Json.andThen convert
