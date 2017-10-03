module Daemon exposing (..)

import Config exposing (..)
import Http
import Json.Encode
import Json.Decode as Json exposing (andThen, fail, succeed)
import Json.Decode.Pipeline
    exposing
        ( custom
        , decode
        , hardcoded
        , optional
        , optionalAt
        , required
        , requiredAt
        )
import Model exposing (..)
import Data.User exposing (Email, Password, User, UserKey, Fingerprint)
import Data.Vault exposing (..)
import Data.Daemon exposing (daemonConfigDecoder)
import Task exposing (Task)
import Time exposing (Time)
import Util exposing (dateDecoder)
import RemoteData exposing (RemoteData(..), WebData)
import WebSocket


type ApiPath
    = Stats
    | Vaults
    | FlyingVaults
    | Vault VaultId
    | DeleteVault VaultId
    | ExportVault VaultId
    | FlyingVault VaultId
    | VaultUsers VaultId
    | VaultUser VaultId Email
    | UserKeys Email
    | VaultFingerprints VaultId
    | VaultHistory VaultId
    | Stream ApiStreamPath
    | User
    | DaemonConfig
    | Feedback
    | Version
    | Login
    | LoginCheck
    | Logout


type ApiStreamPath
    = VaultLogStream VaultId


getStats : Model -> Cmd Msg
getStats { config } =
    config
        |> apiRequest
            Get
            Stats
            Nothing
            statsDecoder
        |> Cmd.map UpdatedStatsFromApi


getVaults : Model -> Cmd Msg
getVaults { config } =
    config
        |> apiRequest
            Get
            Vaults
            Nothing
            (Json.list Data.Vault.decoder)
        |> Cmd.map UpdatedVaultsFromApi


getFlyingVaults : Model -> Cmd Msg
getFlyingVaults { config } =
    config
        |> apiRequest
            Get
            FlyingVaults
            Nothing
            (Json.list Data.Vault.flyingVaultDecoder)
        |> Cmd.map UpdatedFlyingVaultsFromApi


getVault : VaultId -> Config -> Cmd (WebData Vault)
getVault vaultId config =
    config
        |> apiRequest
            Get
            (Vault vaultId)
            Nothing
            Data.Vault.decoder


getFlyingVault : VaultId -> Config -> Cmd (WebData FlyingVault)
getFlyingVault vaultId config =
    config
        |> apiRequest
            Get
            (FlyingVault vaultId)
            Nothing
            Data.Vault.flyingVaultDecoder


updateVaultMetadata : VaultId -> Metadata -> Model -> Cmd Msg
updateVaultMetadata vaultId metadata { config } =
    let
        jsonBody =
            metadata
                |> Data.Vault.metadataEncoder
                |> Http.jsonBody
    in
        config
            |> apiRequest
                Put
                (Vault vaultId)
                (Just jsonBody)
                Data.Vault.decoder
            |> Cmd.map (Model.VaultMetadataUpdated vaultId)


getVaultUsers : VaultId -> Config -> Cmd (WebData (List User))
getVaultUsers vaultId config =
    config
        |> apiRequest
            Get
            (VaultUsers vaultId)
            Nothing
            (Json.list Data.User.decoder)


getVaultHistory : VaultId -> Config -> Cmd (WebData (List HistoryItem))
getVaultHistory vaultId config =
    config
        |> apiRequest
            Get
            (VaultHistory vaultId)
            Nothing
            Data.Vault.historyItemsDecoder


subscribeVaultLogStream :
    VaultId
    -> (Result String Data.Vault.LogItem -> msg)
    -> Config
    -> Sub msg
subscribeVaultLogStream vaultId toMsg config =
    let
        parseMsg : String -> msg
        parseMsg json =
            toMsg <| Json.decodeString logItemDecoder json

        url =
            apiWSUrl config (apiPath (Stream (VaultLogStream vaultId)))
    in
        WebSocket.listen url parseMsg


getVaultUser : VaultId -> Email -> Config -> Cmd (WebData User)
getVaultUser vaultId email config =
    config
        |> apiRequest
            Get
            (VaultUser vaultId email)
            Nothing
            Data.User.decoder


addVaultUser : VaultId -> Email -> List UserKey -> Config -> Cmd Msg
addVaultUser vaultId email keys config =
    let
        jsonBody =
            Data.Vault.addVaultUserEncoder email keys
                |> Http.jsonBody
    in
        config
            |> apiRequest
                Post
                (VaultUsers vaultId)
                (Just jsonBody)
                (decode identity |> required "email" Json.string)
            |> Cmd.map (Model.VaultUserAdded vaultId email)


removeVaultUser : VaultId -> Email -> Config -> Cmd (WebData Email)
removeVaultUser vaultId email config =
    -- TODO: check response data type
    config
        |> apiRequest
            Delete
            (VaultUser vaultId email)
            Nothing
            Json.string


getUserKeys : Email -> Config -> Cmd (WebData (List UserKey))
getUserKeys email config =
    config
        |> apiRequest
            Get
            (UserKeys email)
            Nothing
            (Json.list Data.User.keyDecoder)


getUser : Email -> Config -> Cmd (WebData User)
getUser email config =
    config
        |> apiRequest
            Get
            User
            Nothing
            Data.User.decoder


getConfig : Model -> Cmd Msg
getConfig { config } =
    config
        |> apiRequest
            Get
            DaemonConfig
            Nothing
            daemonConfigDecoder
        |> Cmd.map UpdatedDaemonConfig


invalidateFirstLaunch : Model -> Cmd Msg
invalidateFirstLaunch { config } =
    let
        jsonBody =
            Json.Encode.object
                [ ( "gui"
                  , (Json.Encode.object
                        [ ( "is_first_launch", (Json.Encode.bool False) )
                        ]
                    )
                  )
                ]
                |> Http.jsonBody
    in
        config
            |> apiRequest
                Patch
                DaemonConfig
                (Just jsonBody)
                daemonConfigDecoder
            |> Cmd.map UpdatedDaemonConfig


getVaultFingerprints : VaultId -> Config -> Cmd (WebData (List Fingerprint))
getVaultFingerprints vaultId config =
    config
        |> apiRequest
            Get
            (VaultFingerprints vaultId)
            Nothing
            (Json.list Json.string)


updateVault : VaultOptions -> Config -> Cmd (WebData Vault)
updateVault options config =
    let
        jsonBody =
            options
                |> jsonOptions config
                |> Http.jsonBody
    in
        config
            |> apiRequest
                Post
                Vaults
                (Just jsonBody)
                Data.Vault.decoder


removeVault : VaultId -> Model -> Cmd Msg
removeVault vaultId { config } =
    let
        jsonBody =
            Data.Vault.Remove vaultId
                |> jsonOptions config
                |> Http.jsonBody
    in
        config
            |> apiRequest
                Delete
                (Vault vaultId)
                (Just jsonBody)
                (succeed vaultId)
            |> Cmd.map RemovedVaultFromSync


deleteVault : VaultId -> Config -> Cmd (WebData VaultId)
deleteVault vaultId config =
    config
        |> apiRequest
            Delete
            (DeleteVault vaultId)
            Nothing
            (succeed vaultId)


sendFeedback : String -> Config -> Cmd (WebData String)
sendFeedback text config =
    let
        jsonBody =
            text
                |> Json.Encode.string
                |> Http.jsonBody
    in
        config
            |> apiRequest
                Post
                Feedback
                (Just jsonBody)
                (succeed "")


getVersion : Config -> Cmd (WebData String)
getVersion config =
    config
        |> apiRequest
            Get
            Version
            Nothing
            Json.string


getLoginState : Model -> Cmd Msg
getLoginState { config } =
    config
        |> apiRequest
            Get
            User
            Nothing
            loginStateDecoder
        |> Cmd.map UpdatedLoginState


login : Email -> Password -> Model -> Cmd Msg
login email password { config } =
    let
        jsonBody =
            Data.User.loginEncoder email password
                |> Http.jsonBody
    in
        config
            |> apiRequest
                Post
                Login
                (Just jsonBody)
                statusResponseDecoder
            |> Cmd.map (LoginResult email)


loginCheck : Config -> Cmd (WebData String)
loginCheck config =
    config
        |> apiRequest
            Get
            LoginCheck
            Nothing
            Json.string


logout : Model -> Cmd Msg
logout { config } =
    config
        |> apiRequest
            Get
            Logout
            Nothing
            statusResponseDecoder
        |> Cmd.map LogoutResult


exportVault : VaultId -> String -> Model -> Cmd Msg
exportVault vaultId path { config } =
    let
        jsonBody =
            Json.Encode.object [ ( "path", Json.Encode.string path ) ]
                |> Http.jsonBody
    in
        config
            |> apiRequest
                Post
                (ExportVault vaultId)
                (Just jsonBody)
                exportStatusResponseDecoder
            |> Cmd.map (ExportedVault vaultId)


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

        ExportVault vaultId ->
            "vault/" ++ vaultId ++ "/export"

        FlyingVault vaultId ->
            "flying-vault/" ++ vaultId

        VaultUsers vaultId ->
            "vault/" ++ vaultId ++ "/users"

        VaultUser vaultId email ->
            "vault/" ++ vaultId ++ "/users/" ++ email

        VaultFingerprints vaultId ->
            "vault/" ++ vaultId ++ "/fingerprints"

        VaultHistory vaultId ->
            "vault/" ++ vaultId ++ "/history/"

        Stream (VaultLogStream vaultId) ->
            "vault/" ++ vaultId ++ "/logstream"

        UserKeys email ->
            "user/" ++ email ++ "/keys"

        User ->
            "auth/user"

        DaemonConfig ->
            "config"

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
        apiRequest config Get "vault" (Json.list Data.Vault.decoder)

-}
apiRequest :
    RequestMethod
    -> ApiPath
    -> Maybe Http.Body
    -> Json.Decoder a
    -> Config
    -> Cmd (WebData a)
apiRequest method path maybeBody decoder config =
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
            |> RemoteData.sendRequest


{-| Create a `Task.Task` from an api request (`Http.Request`)
-}
task : Http.Request a -> Task Http.Error a
task =
    Http.toTask


attemptDelayed :
    Time
    -> (Result Http.Error a -> Msg)
    -> Http.Request a
    -> Cmd Msg
attemptDelayed time msg request =
    request
        |> task
        |> Util.attemptDelayed time msg


rootUrl : Config -> Path -> Path
rootUrl config path =
    case ( String.endsWith "/" config.apiUrl, String.startsWith "/" path ) of
        ( True, False ) ->
            config.apiUrl

        ( True, True ) ->
            String.dropRight 1 config.apiUrl

        ( False, True ) ->
            config.apiUrl

        ( False, False ) ->
            config.apiUrl ++ "/"


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
        hasQuery =
            String.contains "?"
    in
        -- the daemon API expects requests URLs to end with "/"
        -- e.g. /v1/vault/ or /v1/vault/id/ and not /v1/vault or /v1/vault/id
        if String.endsWith "/" path || hasQuery path then
            rootUrl config path ++ path
        else
            rootUrl config path ++ path ++ "/"


apiWSUrl : Config -> Path -> Url
apiWSUrl config path =
    let
        url =
            case String.split "://" (apiUrl config path) of
                [ _, url ] ->
                    url

                _ ->
                    path
    in
        "ws://" ++ url


{-| Returns the required `Http.Header`s required by the daemon JSON API.
-}
apiHeaders : Config -> List Http.Header
apiHeaders config =
    [ Http.header "X-Authtoken" config.apiAuthToken ]
