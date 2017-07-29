module Syncrypt.Stats exposing (..)


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
