module Data.Daemon
    exposing
        ( DaemonConfig
        , GUIConfig
        , KeyState(..)
        , LogItem
        , Stats
        , daemonConfigDecoder
        , guiConfigDecoder
        , languageDecoder
        , logItemDecoder
        )

import Data.Vault exposing (LogLevel, VaultId)
import Date exposing (Date)
import Dict exposing (Dict)
import Json.Decode as Json exposing (fail, succeed)
import Json.Decode.Pipeline exposing (decode, optional, required)
import Language exposing (Language(..))
import Util


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


type alias LogItem =
    { level : LogLevel
    , createdAt : Maybe Date
    , message : String
    , vaultId : Maybe VaultId
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


logItemDecoder : Json.Decoder LogItem
logItemDecoder =
    decode LogItem
        |> required "level" Data.Vault.logLevelDecoder
        |> required "createdAt" Util.dateDecoder
        |> required "message" Json.string
        |> optional "vault_id" (Json.maybe Json.string) Nothing
