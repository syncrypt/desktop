module Data.Daemon
    exposing
        ( DaemonConfig
        , Error
        , ErrorStatus
        , GUIConfig
        , KeyState(..)
        , LogItem
        , Stats
        , daemonConfigDecoder
        , errorDecoder
        , errorStatus
        , guiConfigDecoder
        , languageDecoder
        , logItemDecoder
        )

import Data.Vault exposing (VaultId)
import Date exposing (Date)
import Json.Decode as Json exposing (andThen, fail, succeed)
import Json.Decode.Pipeline exposing (decode, optional, required)
import Language exposing (Language(..))
import Util exposing (LogLevel, andLog)


type alias Error =
    { status : ErrorStatus
    , reason : String
    }


type ErrorStatus
    = ErrorStatus String


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


errorStatus : ErrorStatus -> String
errorStatus (ErrorStatus s) =
    s


errorDecoder : Json.Decoder Error
errorDecoder =
    decode Error
        |> required "status" errorStatusDecoder
        |> required "reason" Json.string


errorStatusDecoder : Json.Decoder ErrorStatus
errorStatusDecoder =
    Json.string
        |> andThen (succeed << ErrorStatus)


daemonConfigDecoder : Json.Decoder DaemonConfig
daemonConfigDecoder =
    decode DaemonConfig
        |> required "gui" guiConfigDecoder


guiConfigDecoder : Json.Decoder GUIConfig
guiConfigDecoder =
    decode GUIConfig
        |> optional "is_first_launch" Json.bool True
        |> optional "language" languageDecoder Language.English


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
                    succeed English
                        |> andLog "Invalid language configured in syncrypt config, defaulting to English" val
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
