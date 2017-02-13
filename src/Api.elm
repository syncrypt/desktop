module Api exposing (..)

import Config exposing (..)
import Json.Decode as Json exposing (succeed, andThen, fail)
import Json.Decode.Pipeline exposing (decode, required, optional, optionalAt, hardcoded, custom)
import Http
import Model exposing (..)
import Syncrypt.Vault exposing (..)
import Date exposing (Date)
import String


type alias Path =
    String


type alias Url =
    String


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
        if String.endsWith "/" path then
            rootUrl ++ path
        else
            rootUrl ++ path ++ "/"


apiHeaders : Config -> List Http.Header
apiHeaders config =
    [ Http.header "X-Authtoken" config.apiAuthToken
    ]


getVaults : Config -> Cmd Msg
getVaults config =
    let
        url =
            apiUrl config "vault"

        request =
            Http.request
                { method = "GET"
                , headers = apiHeaders config
                , url = url
                , body = Http.emptyBody
                , expect = Http.expectJson decodeVaults
                , timeout = Nothing
                , withCredentials = False
                }
    in
        Http.send UpdatedVaultsFromApi request


decodeVaults : Json.Decoder (List Syncrypt.Vault.Vault)
decodeVaults =
    Json.list vaultDecoder


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
