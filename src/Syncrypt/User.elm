module Syncrypt.User exposing (..)

import Date exposing (Date)
import Json.Encode
import Json.Decode as Json exposing (andThen, fail, succeed)
import Json.Decode.Pipeline exposing (decode, optional, required)
import Util exposing (dateDecoder)


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


type alias Password =
    String


type alias UserKey =
    { fingerprint : Fingerprint
    , description : String
    , createdAt : Maybe Date
    }


type alias EmailWithFingerPrint =
    ( String, Fingerprint )



-- Decoders


{-| Decodes a `Syncrypt.User.User`.
-}
decoder : Json.Decoder User
decoder =
    decode User
        |> required "first_name" Json.string
        |> required "last_name" Json.string
        |> required "email" Json.string
        |> optional "access_granted_at" dateDecoder Nothing


{-| Decodes a `Syncrypt.User.UserKey`.
-}
keyDecoder : Json.Decoder UserKey
keyDecoder =
    decode UserKey
        |> required "fingerprint" Json.string
        |> required "description" Json.string
        |> required "created_at" dateDecoder


loginEncoder email password =
    Json.Encode.object
        [ ( "email", Json.Encode.string email )
        , ( "password", Json.Encode.string password )
        ]



-- Model functions


fullName : User -> String
fullName { firstName, lastName } =
    firstName ++ " " ++ lastName
