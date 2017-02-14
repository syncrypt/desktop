module Api exposing (..)

import Config exposing (..)
import Json.Decode as Json exposing (succeed, andThen, fail)
import Json.Decode.Pipeline exposing (decode, required, optional, optionalAt, hardcoded, custom)
import Http
import Model exposing (..)
import Syncrypt.Vault exposing (..)
import Date exposing (Date)
import String


getVaults : Config -> Cmd Msg
getVaults config =
    Http.send UpdatedVaultsFromApi (apiRequest config Get "vault" decodeVaults)


getFlyingVaults : Config -> Cmd Msg
getFlyingVaults config =
    Http.send UpdatedFlyingVaultsFromApi (apiRequest config Get "flying-vault" decodeFlyingVaults)


type alias Path =
    String


type alias Url =
    String


type RequestMethod
    = Get
    | Put
    | Post
    | Patch
    | Delete


requestMethod : RequestMethod -> String
requestMethod rm =
    case rm of
        Get ->
            "GET"

        Put ->
            "PUT"

        Post ->
            "POST"

        Patch ->
            "PATCH"

        Delete ->
            "DELETE"


apiRequest : Config -> RequestMethod -> Path -> Json.Decoder a -> Http.Request a
apiRequest config method path decoder =
    Http.request
        { method = requestMethod method
        , headers = apiHeaders config
        , url = apiUrl config path
        , body = Http.emptyBody
        , expect = Http.expectJson decoder
        , timeout = Nothing
        , withCredentials = False
        }


apiUrl : Config -> Path -> Url
apiUrl config path =
    let
        rootUrl =
            case ( String.endsWith "/" config.apiUrl, String.startsWith "/" path ) of
                ( True, False ) ->
                    config.apiUrl

                ( True, True ) ->
                    String.dropRight 1 config.apiUrl

                ( False, True ) ->
                    config.apiUrl

                ( False, False ) ->
                    config.apiUrl ++ "/"
    in
        -- the daemon API expects requests URLs to end with "/"
        -- e.g. /v1/vault/ or /v1/vault/id/ and not /v1/vault or /v1/vault/id
        if String.endsWith "/" path then
            rootUrl ++ path
        else
            rootUrl ++ path ++ "/"


apiHeaders : Config -> List Http.Header
apiHeaders config =
    [ Http.header "X-Authtoken" config.apiAuthToken
    ]


decodeVaults : Json.Decoder (List Syncrypt.Vault.Vault)
decodeVaults =
    Json.list vaultDecoder


decodeFlyingVaults : Json.Decoder (List Syncrypt.Vault.FlyingVault)
decodeFlyingVaults =
    Json.list flyingVaultDecoder


vaultDecoder : Json.Decoder Syncrypt.Vault.Vault
vaultDecoder =
    decode Syncrypt.Vault.Vault
        |> required "id" Json.string
        |> optionalAt [ "metadata", "name" ] (Json.maybe Json.string) Nothing
        |> required "size" Json.int
        |> required "status" vaultStatus
        |> required "user_count" Json.int
        |> required "file_count" Json.int
        |> required "revision_count" Json.int
        |> required "resource_uri" Json.string
        |> required "folder" Json.string
        |> required "modification_date" date


flyingVaultDecoder : Json.Decoder Syncrypt.Vault.FlyingVault
flyingVaultDecoder =
    decode Syncrypt.Vault.FlyingVault
        |> required "id" Json.string
        |> optionalAt [ "metadata", "name" ] (Json.maybe Json.string) Nothing
        |> optional "size" (Json.maybe Json.int) Nothing
        |> required "user_count" Json.int
        |> required "file_count" Json.int
        |> required "revision_count" Json.int
        |> required "resource_uri" Json.string
        |> required "modification_date" date


vaultStatus : Json.Decoder Status
vaultStatus =
    let
        convert : String -> Json.Decoder Status
        convert raw =
            case raw of
                "unsynced" ->
                    succeed Syncrypt.Vault.Unsynced

                "syncing" ->
                    succeed Syncrypt.Vault.Syncing

                "initializing" ->
                    succeed Syncrypt.Vault.Initializing

                "synced" ->
                    succeed Syncrypt.Vault.Synced

                "ready" ->
                    succeed Syncrypt.Vault.Ready

                val ->
                    fail ("Invalid vault status: " ++ val)
    in
        Json.string |> andThen convert


date : Json.Decoder (Maybe Date)
date =
    let
        convert : String -> Json.Decoder (Maybe Date)
        convert raw =
            case Date.fromString raw of
                Ok date ->
                    succeed (Just date)

                Err error ->
                    succeed Nothing
    in
        Json.string |> andThen convert
