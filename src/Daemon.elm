module Daemon exposing (..)

import Config exposing (..)
import Json.Decode as Json exposing (succeed, andThen, fail)
import Json.Decode.Pipeline exposing (decode, required, optional, optionalAt, hardcoded, custom)
import Http
import Model exposing (..)
import Syncrypt.Vault exposing (..)
import Date exposing (Date)
import String
import Task exposing (Task)
import Util


getVaults : Config -> Http.Request (List Vault)
getVaults config =
    apiRequest config Get "vault" vaultsDecoder


getFlyingVaults : Config -> Http.Request (List FlyingVault)
getFlyingVaults config =
    apiRequest config Get "flying-vault" flyingVaultsDecoder


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


{-| Converts `RequestMethod` into `String`.

    requestMethod Get  -- -> "GET"
    requestMethod Post -- -> "POST"
-}
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


{-| Creates an syncrypt daemon API compatible `Http.Request`.

    let
      config = {apiUrl = "http://localhost:28080/", apiAuthToken="123"}
    in
      apiRequest config Get "vault" vaultsDecoder
-}
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


{-| Create a `Task.Task` from an api request (`Http.Request`)
-}
task : Http.Request a -> Task Http.Error a
task =
    Http.toTask


attempt msg request =
    request
        |> task
        |> Task.attempt msg


attemptDelayed time msg request =
    request
        |> task
        |> Util.attemptDelayed time msg


{-| Returns the api url for a given `Config` and `Path`.

    let
      config = {apiUrl = "http://localhost:28080/", apiAuthToken="123"}
    in
      apiUrl config "foo"  -- -> "http://localhost:28080/foo/"
      apiUrl config "/bar" -- -> "http://localhost:28080/bar/"
-}
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


{-| Returns the required `Http.Header`s required by the daemon JSON API.
-}
apiHeaders : Config -> List Http.Header
apiHeaders config =
    [ Http.header "X-Authtoken" config.apiAuthToken
    ]


{-| Decodes an array of `Syncrypt.Vault.Vault`.
-}
vaultsDecoder : Json.Decoder (List Syncrypt.Vault.Vault)
vaultsDecoder =
    Json.list vaultDecoder


{-| Decodes an array of `Syncrypt.Vault.FlyingVault`.
-}
flyingVaultsDecoder : Json.Decoder (List Syncrypt.Vault.FlyingVault)
flyingVaultsDecoder =
    Json.list flyingVaultDecoder


{-| Decodes a `Syncrypt.Vault.Vault`.
-}
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


{-| Decodes a `Syncrypt.Vault.FlyingVault`.
-}
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


{-| Decodes a `Syncrypt.Vault.Status`.
-}
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


{-| Decodes an `Maybe Date` from a string.
-}
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
