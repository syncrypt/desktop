module Daemon exposing (..)

import Config exposing (..)
import Data.Daemon exposing (GUIConfig, daemonConfigDecoder)
import Data.User exposing (Email, Fingerprint, Password, User, UserKey)
import Data.Vault exposing (..)
import Http
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
import Json.Encode
import Language exposing (Language(..))
import Model exposing (..)
import RemoteData exposing (RemoteData(..), WebData)
import Task exposing (Task)
import Time exposing (Time)
import Util exposing (dateDecoder)
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
            EmptyBody
            statsDecoder
        |> Cmd.map UpdatedStatsFromApi


getVaults : Model -> Cmd Msg
getVaults { config } =
    config
        |> apiRequest
            Get
            Vaults
            EmptyBody
            (Json.list Data.Vault.decoder)
        |> Cmd.map UpdatedVaultsFromApi


getFlyingVaults : Model -> Cmd Msg
getFlyingVaults { config } =
    config
        |> apiRequest
            Get
            FlyingVaults
            EmptyBody
            (Json.list Data.Vault.flyingVaultDecoder)
        |> Cmd.map UpdatedFlyingVaultsFromApi


getVault : VaultId -> Config -> Cmd (WebData Vault)
getVault vaultId config =
    config
        |> apiRequest
            Get
            (Vault vaultId)
            EmptyBody
            Data.Vault.decoder


getFlyingVault : VaultId -> Config -> Cmd (WebData FlyingVault)
getFlyingVault vaultId config =
    config
        |> apiRequest
            Get
            (FlyingVault vaultId)
            EmptyBody
            Data.Vault.flyingVaultDecoder


updateVaultMetadata : VaultId -> Metadata -> Model -> Cmd Msg
updateVaultMetadata vaultId metadata { config } =
    config
        |> apiRequest
            Put
            (Vault vaultId)
            (Json <| Data.Vault.metadataEncoder metadata)
            Data.Vault.decoder
        |> Cmd.map (Model.VaultMetadataUpdated vaultId)


getVaultUsers : VaultId -> Config -> Cmd (WebData (List User))
getVaultUsers vaultId config =
    config
        |> apiRequest
            Get
            (VaultUsers vaultId)
            EmptyBody
            (Json.list Data.User.decoder)


getVaultHistory : VaultId -> Config -> Cmd (WebData (List HistoryItem))
getVaultHistory vaultId config =
    config
        |> apiRequest
            Get
            (VaultHistory vaultId)
            EmptyBody
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
            EmptyBody
            Data.User.decoder


addVaultUser : VaultId -> Email -> List UserKey -> Config -> Cmd Msg
addVaultUser vaultId email keys config =
    config
        |> apiRequest
            Post
            (VaultUsers vaultId)
            (Json <| Data.Vault.addVaultUserEncoder email keys)
            (decode identity |> required "email" Json.string)
        |> Cmd.map (Model.VaultUserAdded vaultId email)


removeVaultUser : VaultId -> Email -> Config -> Cmd (WebData Email)
removeVaultUser vaultId email config =
    -- TODO: check response data type
    config
        |> apiRequest
            Delete
            (VaultUser vaultId email)
            EmptyBody
            Json.string


getUserKeys : Email -> Config -> Cmd (WebData (List UserKey))
getUserKeys email config =
    config
        |> apiRequest
            Get
            (UserKeys email)
            EmptyBody
            (Json.list Data.User.keyDecoder)


getUser : Email -> Config -> Cmd (WebData User)
getUser email config =
    config
        |> apiRequest
            Get
            User
            EmptyBody
            Data.User.decoder


getConfig : Model -> Cmd Msg
getConfig { config } =
    config
        |> apiRequest
            Get
            DaemonConfig
            EmptyBody
            daemonConfigDecoder
        |> Cmd.map UpdatedDaemonConfig


invalidateFirstLaunch : Model -> Cmd Msg
invalidateFirstLaunch { config } =
    let
        json =
            Json.Encode.object
                [ ( "gui"
                  , Json.Encode.object
                        [ ( "is_first_launch", Json.Encode.bool False )
                        ]
                  )
                ]
    in
    config
        |> apiRequest
            Patch
            DaemonConfig
            (Json json)
            daemonConfigDecoder
        |> Cmd.map UpdatedDaemonConfig


updateGUIConfig :
    Model
    -> GUIConfig
    -> Cmd Msg
updateGUIConfig { config } guiConfig =
    let
        json =
            Json.Encode.object
                [ ( "gui"
                  , Json.Encode.object
                        [ ( "is_first_launch", Json.Encode.bool guiConfig.isFirstLaunch )
                        , ( "language", Json.Encode.string <| toString guiConfig.language )
                        ]
                  )
                ]
    in
    config
        |> apiRequest
            Patch
            DaemonConfig
            (Json json)
            daemonConfigDecoder
        |> Cmd.map UpdatedDaemonConfig


getVaultFingerprints : VaultId -> Config -> Cmd (WebData (List Fingerprint))
getVaultFingerprints vaultId config =
    config
        |> apiRequest
            Get
            (VaultFingerprints vaultId)
            EmptyBody
            (Json.list Json.string)


updateVault : VaultOptions -> Config -> Cmd (WebData Vault)
updateVault options config =
    config
        |> apiRequest
            Post
            Vaults
            (Json <| jsonOptions config options)
            Data.Vault.decoder


removeVault : VaultId -> Model -> Cmd Msg
removeVault vaultId { config } =
    let
        json =
            Data.Vault.Remove vaultId
                |> jsonOptions config
    in
    config
        |> apiRequest
            Delete
            (Vault vaultId)
            (Json json)
            (succeed vaultId)
        |> Cmd.map RemovedVaultFromSync


deleteVault : VaultId -> Config -> Cmd (WebData VaultId)
deleteVault vaultId config =
    config
        |> apiRequest
            Delete
            (DeleteVault vaultId)
            EmptyBody
            (succeed vaultId)


sendFeedback : String -> Config -> Cmd Msg
sendFeedback text config =
    config
        |> apiRequest
            Post
            Feedback
            (Json <|
                Json.Encode.object
                    [ ( "feedback_text", Json.Encode.string text ) ]
            )
            (succeed "Ok")
        |> Cmd.map SentFeedback


getVersion : Config -> Cmd (WebData String)
getVersion config =
    config
        |> apiRequest
            Get
            Version
            EmptyBody
            Json.string


getLoginState : Model -> Cmd Msg
getLoginState { config } =
    config
        |> apiRequest
            Get
            User
            EmptyBody
            loginStateDecoder
        |> Cmd.map UpdatedLoginState


login : Email -> Password -> Model -> Cmd Msg
login email password { config } =
    config
        |> apiRequest
            Post
            Login
            (Json <| Data.User.loginEncoder email password)
            statusResponseDecoder
        |> Cmd.map (LoginResult email)


loginCheck : Config -> Cmd (WebData String)
loginCheck config =
    config
        |> apiRequest
            Get
            LoginCheck
            EmptyBody
            Json.string


logout : Model -> Cmd Msg
logout { config } =
    config
        |> apiRequest
            Get
            Logout
            EmptyBody
            statusResponseDecoder
        |> Cmd.map LogoutResult


exportVault : VaultId -> String -> Model -> Cmd Msg
exportVault vaultId path { config } =
    let
        json =
            Json.Encode.object [ ( "path", Json.Encode.string path ) ]
    in
    config
        |> apiRequest
            Post
            (ExportVault vaultId)
            (Json json)
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


type ApiRequestBody
    = Json Json.Encode.Value
    | Body Http.Body
    | EmptyBody


{-| Creates an syncrypt daemon API compatible `Http.Request`.

    let
        config = {apiUrl = "http://localhost:28080/", apiAuthToken="123"}
    in
        apiRequest config Get "vault" (Json.list Data.Vault.decoder)

-}
apiRequest :
    RequestMethod
    -> ApiPath
    -> ApiRequestBody
    -> Json.Decoder a
    -> Config
    -> Cmd (WebData a)
apiRequest method path body decoder config =
    Http.request
        { method = requestMethod method
        , headers = apiHeaders config
        , url = apiUrl config (apiPath path)
        , body = apiRequestBody body
        , expect = Http.expectJson decoder
        , timeout = Nothing
        , withCredentials = False
        }
        |> RemoteData.sendRequest


apiRequestBody : ApiRequestBody -> Http.Body
apiRequestBody body =
    case body of
        Json json ->
            Http.jsonBody json

        Body body ->
            body

        EmptyBody ->
            Http.emptyBody


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
