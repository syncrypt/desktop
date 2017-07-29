module Syncrypt.Daemon exposing (..)

import Json.Decode as Json exposing (andThen, succeed)
import Json.Decode.Pipeline exposing (decode, optional, required, requiredAt, optionalAt)


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
    , language : String
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
        |> required "language" Json.string
