module Syncrypt.User exposing (..)

import Date exposing (Date)


type alias User =
    { firstName : String
    , lastName : String
    , email : Email
    , accessGrantedAt : Maybe Date

    --, keys : List UserKey
    }


type alias Fingerprint =
    String


type alias Email =
    String


type alias UserKey =
    { fingerprint : Fingerprint
    , description : String
    , createdAt : Maybe Date
    }


type alias EmailWithFingerPrint =
    ( String, Fingerprint )
