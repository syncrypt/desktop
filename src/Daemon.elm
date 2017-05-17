module Daemon exposing (..)

import Config exposing (..)
import Date exposing (Date)
import Http
import Json.Encode
import Json.Decode as Json exposing (andThen, fail, succeed)
import Json.Decode.Pipeline exposing (custom, decode, hardcoded, optional, optionalAt, required, requiredAt)
import Model exposing (..)
import Syncrypt.User exposing (Email, Password, User, UserKey, Fingerprint)
import Syncrypt.Vault exposing (..)
import Task exposing (Task)
import Time exposing (Time)
import Util


type ApiPath
    = Stats
    | Vaults
    | FlyingVaults
    | Vault VaultId
    | DeleteVault VaultId
    | FlyingVault VaultId
    | VaultUsers VaultId
    | VaultUser VaultId Email
    | UserKeys Email
    | VaultFingerprints VaultId
    | User
    | Feedback
    | Version
    | Login
    | LoginCheck
    | Logout


getStats : Config -> Http.Request Stats
getStats config =
    apiRequest config Get Stats Nothing statsDecoder


getVaults : Config -> Http.Request (List Vault)
getVaults config =
    apiRequest config Get Vaults Nothing vaultsDecoder


getFlyingVaults : Config -> Http.Request (List FlyingVault)
getFlyingVaults config =
    apiRequest config Get FlyingVaults Nothing flyingVaultsDecoder


getVault : VaultId -> Config -> Http.Request Vault
getVault vaultId config =
    apiRequest config Get (Vault vaultId) Nothing vaultDecoder


updateVaultMetadata : VaultId -> Metadata -> Config -> Http.Request Vault
updateVaultMetadata vaultId metadata config =
    let
        metadataJson =
            Json.Encode.object [ ( "name", Json.Encode.string metadata.name ) ]

        json =
            Json.Encode.object [ ( "metadata", metadataJson ) ]
    in
        apiRequest config Put (Vault vaultId) (Just (Http.jsonBody json)) vaultDecoder


getFlyingVault : VaultId -> Config -> Http.Request FlyingVault
getFlyingVault vaultId config =
    apiRequest config Get (FlyingVault vaultId) Nothing flyingVaultDecoder


getVaultUsers : VaultId -> Config -> Http.Request (List User)
getVaultUsers vaultId config =
    apiRequest config Get (VaultUsers vaultId) Nothing usersDecoder


getVaultUser : VaultId -> Email -> Config -> Http.Request User
getVaultUser vaultId email config =
    apiRequest config Get (VaultUser vaultId email) Nothing userDecoder


addVaultUser : VaultId -> Email -> List UserKey -> Config -> Http.Request Email
addVaultUser vaultId email keys config =
    let
        fingerprints =
            List.map .fingerprint keys

        json =
            Json.Encode.object
                [ ( "email", Json.Encode.string email )
                , ( "fingerprints", Json.Encode.list (List.map Json.Encode.string fingerprints) )
                ]
    in
        apiRequest config
            Post
            (VaultUsers vaultId)
            (Just (Http.jsonBody json))
            (decode identity |> required "email" Json.string)


removeVaultUser : VaultId -> Email -> Config -> Http.Request Email
removeVaultUser vaultId email config =
    -- TODO: check response data type
    apiRequest config Delete (VaultUser vaultId email) Nothing Json.string


getUserKeys : Email -> Config -> Http.Request (List UserKey)
getUserKeys email config =
    apiRequest config Get (UserKeys email) Nothing userKeysDecoder


getUser : Email -> Config -> Http.Request User
getUser email config =
    apiRequest config Get User Nothing userDecoder


getVaultFingerprints : VaultId -> Config -> Http.Request (List Fingerprint)
getVaultFingerprints vaultId config =
    apiRequest config Get (VaultFingerprints vaultId) Nothing (Json.list Json.string)


updateVault : VaultOptions -> Config -> Http.Request Vault
updateVault options config =
    apiRequest config
        Post
        Vaults
        (Just (Http.jsonBody (jsonOptions config options)))
        vaultDecoder


removeVault : VaultId -> Config -> Http.Request VaultId
removeVault vaultId config =
    apiRequest config
        Delete
        (Vault vaultId)
        (Just (Http.jsonBody (jsonOptions config (Syncrypt.Vault.Remove vaultId))))
        (decodeToVal vaultId)


deleteVault : VaultId -> Config -> Http.Request VaultId
deleteVault vaultId config =
    apiRequest config
        Delete
        (DeleteVault vaultId)
        Nothing
        (decodeToVal vaultId)


sendFeedback : String -> Config -> Http.Request String
sendFeedback text config =
    apiRequest config
        Post
        Feedback
        (Just (Http.jsonBody (Json.Encode.string text)))
        (decodeToVal "")


getVersion : Config -> Http.Request String
getVersion config =
    apiRequest config Get Version Nothing Json.string


login : Email -> Password -> Config -> Http.Request String
login email password config =
    let
        json =
            (Json.Encode.object
                [ ( "email", Json.Encode.string email )
                , ( "password", Json.Encode.string password )
                ]
            )
    in
        apiRequest config
            Post
            Login
            (Just (Http.jsonBody json))
            Json.string


loginCheck : Config -> Http.Request String
loginCheck config =
    apiRequest config Post LoginCheck Nothing Json.string


logout : Config -> Http.Request String
logout config =
    apiRequest config Post Logout Nothing Json.string


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


{-| Converts `ApiPath` into `Path` (`String`).

    apiPath Vaults  -- -> "vaults"
    apiPath (Vault "foobaruuid") -- -> "vaults/foobaruuid"

-}
apiPath : ApiPath -> Path
apiPath apiPath =
    case apiPath of
        Stats ->
            "stats"

        Vaults ->
            "vault"

        FlyingVaults ->
            "flying-vault"

        Vault vaultId ->
            "vault/" ++ vaultId

        DeleteVault vaultId ->
            "vault/" ++ vaultId ++ "?wipe=1"

        FlyingVault vaultId ->
            "flying-vault/" ++ vaultId

        VaultUsers vaultId ->
            "vault/" ++ vaultId ++ "/users"

        VaultUser vaultId email ->
            "vault/" ++ vaultId ++ "/users/" ++ email

        VaultFingerprints vaultId ->
            "vault/" ++ vaultId ++ "/fingerprints"

        UserKeys email ->
            "user/" ++ email ++ "/keys"

        User ->
            "auth/user"

        Feedback ->
            "feedback"

        Version ->
            "version"

        Login ->
            "auth/login"

        LoginCheck ->
            "auth/check"

        Logout ->
            "auth/logout"


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
apiRequest : Config -> RequestMethod -> ApiPath -> Maybe Http.Body -> Json.Decoder a -> Http.Request a
apiRequest config method path maybeBody decoder =
    let
        body =
            case maybeBody of
                Nothing ->
                    Http.emptyBody

                Just body ->
                    body
    in
        Http.request
            { method = requestMethod method
            , headers = apiHeaders config
            , url = apiUrl config (apiPath path)
            , body = body
            , expect = Http.expectJson decoder
            , timeout = Nothing
            , withCredentials = False
            }


{-| Create a `Task.Task` from an api request (`Http.Request`)
-}
task : Http.Request a -> Task Http.Error a
task =
    Http.toTask


attempt : (Result Http.Error a -> Msg) -> Http.Request a -> Cmd Msg
attempt msg request =
    request
        |> task
        |> Task.attempt msg


attemptDelayed : Time -> (Result Http.Error a -> Msg) -> Http.Request a -> Cmd Msg
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

        hasQuery =
            String.contains "?"

        isSpecialPath =
            (==) "stats"
    in
        -- the daemon API expects requests URLs to end with "/"
        -- e.g. /v1/vault/ or /v1/vault/id/ and not /v1/vault or /v1/vault/id
        if String.endsWith "/" path || hasQuery path || isSpecialPath path then
            rootUrl ++ path
        else
            rootUrl ++ path ++ "/"


{-| Returns the required `Http.Header`s required by the daemon JSON API.
-}
apiHeaders : Config -> List Http.Header
apiHeaders config =
    [ Http.header "X-Authtoken" config.apiAuthToken
    ]


decodeToVal : a -> Json.Decoder a
decodeToVal val =
    Json.succeed val


statsDecoder : Json.Decoder Model.Stats
statsDecoder =
    decode Model.Stats
        |> requiredAt [ "stats", "stats" ] Json.int
        |> requiredAt [ "stats", "downloads" ] Json.int
        |> requiredAt [ "stats", "uploads" ] Json.int


{-| Decodes an array of `Syncrypt.Vault.Vault`.
-}
vaultsDecoder : Json.Decoder (List Vault)
vaultsDecoder =
    Json.list vaultDecoder


usersDecoder : Json.Decoder (List User)
usersDecoder =
    Json.list userDecoder


userKeysDecoder : Json.Decoder (List Syncrypt.User.UserKey)
userKeysDecoder =
    Json.list userKeyDecoder


{-| Decodes an array of `Syncrypt.Vault.FlyingVault`.
-}
flyingVaultsDecoder : Json.Decoder (List FlyingVault)
flyingVaultsDecoder =
    Json.list flyingVaultDecoder


{-| Decodes a `Syncrypt.Vault.Vault`.
-}
vaultDecoder : Json.Decoder Vault
vaultDecoder =
    decode Syncrypt.Vault.Vault
        |> required "id" Json.string
        |> optionalAt [ "metadata", "name" ] (Json.maybe Json.string) Nothing
        |> required "size" Json.int
        |> required "state" vaultStatus
        |> required "user_count" Json.int
        |> required "file_count" Json.int
        |> required "revision_count" Json.int
        |> required "resource_uri" Json.string
        |> required "folder" Json.string
        |> optional "modification_date" date Nothing


{-| Decodes a `Syncrypt.Vault.FlyingVault`.
-}
flyingVaultDecoder : Json.Decoder FlyingVault
flyingVaultDecoder =
    decode Syncrypt.Vault.FlyingVault
        |> required "id" Json.string
        |> optionalAt [ "metadata", "name" ] (Json.maybe Json.string) Nothing
        |> optional "size" (Json.maybe Json.int) Nothing
        |> required "user_count" Json.int
        |> required "file_count" Json.int
        |> required "revision_count" Json.int
        |> required "resource_uri" Json.string
        |> optional "modification_date" date Nothing


{-| Decodes a `Syncrypt.User.User`.
-}
userDecoder : Json.Decoder User
userDecoder =
    decode Syncrypt.User.User
        |> required "first_name" Json.string
        |> required "last_name" Json.string
        |> required "email" Json.string
        |> optional "access_granted_at" date Nothing


{-| Decodes a `Syncrypt.User.UserKey`.
-}
userKeyDecoder : Json.Decoder Syncrypt.User.UserKey
userKeyDecoder =
    decode Syncrypt.User.UserKey
        |> required "fingerprint" Json.string
        |> required "description" Json.string
        |> required "created_at" date


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
