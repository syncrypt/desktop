module Syncrypt.User exposing (..)


type alias User =
    { firstName : String
    , lastName : String
    , email : String
    , keys : List UserKey
    }


type alias Fingerprint =
    String


type alias UserKey =
    { fingerprint : Fingerprint }


type alias EmailWithFingerPrint =
    ( String, Fingerprint )
