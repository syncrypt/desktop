module Daemon exposing (..)

import Config exposing (..)
import Http
import Json.Encode
import Json.Decode as Json exposing (andThen, fail, succeed)
import Json.Decode.Pipeline exposing (custom, decode, hardcoded, optional, optionalAt, required, requiredAt)
import Model exposing (..)
import Syncrypt.User exposing (Email, Password, User, UserKey, Fingerprint)
import Syncrypt.Vault exposing (..)
import Task exposing (Task)
import Time exposing (Time)
import Util exposing (dateDecoder)
import RemoteData exposing (RemoteData(..), WebData)


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
    | User
    | Feedback
    | Version
    | Login
    | LoginCheck
    | Logout


getStats : Model -> Cmd Msg
getStats { config } =
    apiRequest config Get Stats Nothing statsDecoder
        |> Cmd.map UpdatedStatsFromApi


getVaults : Model -> Cmd Msg
getVaults { config } =
    apiRequest config Get Vaults Nothing (Json.list Syncrypt.Vault.decoder)
        |> Cmd.map UpdatedVaultsFromApi


getFlyingVaults : Model -> Cmd Msg
getFlyingVaults { config } =
    apiRequest config Get FlyingVaults Nothing (Json.list Syncrypt.Vault.flyingVaultDecoder)
        |> Cmd.map UpdatedFlyingVaultsFromApi


getVault : VaultId -> Config -> Cmd (WebData Vault)
getVault vaultId config =
    apiRequest config Get (Vault vaultId) Nothing Syncrypt.Vault.decoder


getFlyingVault : VaultId -> Config -> Cmd (WebData FlyingVault)
getFlyingVault vaultId config =
    apiRequest config Get (FlyingVault vaultId) Nothing Syncrypt.Vault.flyingVaultDecoder


updateVaultMetadata : VaultId -> Metadata -> Model -> Cmd Msg
updateVaultMetadata vaultId metadata { config } =
    let
        jsonBody =
            metadata
                |> Syncrypt.Vault.metadataEncoder
                |> Http.jsonBody
    in
        apiRequest config Put (Vault vaultId) (Just jsonBody) Syncrypt.Vault.decoder
            |> Cmd.map (Model.VaultMetadataUpdated vaultId)


getVaultUsers : VaultId -> Config -> Cmd (WebData (List User))
getVaultUsers vaultId config =
    apiRequest config Get (VaultUsers vaultId) Nothing (Json.list Syncrypt.User.decoder)


getVaultUser : VaultId -> Email -> Config -> Cmd (WebData User)
getVaultUser vaultId email config =
    apiRequest config Get (VaultUser vaultId email) Nothing Syncrypt.User.decoder


addVaultUser : VaultId -> Email -> List UserKey -> Config -> Cmd Msg
addVaultUser vaultId email keys config =
    let
        jsonBody =
            Syncrypt.Vault.addVaultUserEncoder email keys
                |> Http.jsonBody
    in
        apiRequest config
            Post
            (VaultUsers vaultId)
            (Just jsonBody)
            (decode identity |> required "email" Json.string)
            |> Cmd.map (Model.VaultUserAdded vaultId email)


removeVaultUser : VaultId -> Email -> Config -> Cmd (WebData Email)
removeVaultUser vaultId email config =
    -- TODO: check response data type
    apiRequest config Delete (VaultUser vaultId email) Nothing Json.string


getUserKeys : Email -> Config -> Cmd (WebData (List UserKey))
getUserKeys email config =
    apiRequest config Get (UserKeys email) Nothing (Json.list Syncrypt.User.keyDecoder)


getUser : Email -> Config -> Cmd (WebData User)
getUser email config =
    apiRequest config Get User Nothing Syncrypt.User.decoder


getVaultFingerprints : VaultId -> Config -> Cmd (WebData (List Fingerprint))
getVaultFingerprints vaultId config =
    apiRequest config Get (VaultFingerprints vaultId) Nothing (Json.list Json.string)


updateVault : VaultOptions -> Config -> Cmd (WebData Vault)
updateVault options config =
    apiRequest config
        Post
        Vaults
        (Just (Http.jsonBody (jsonOptions config options)))
        Syncrypt.Vault.decoder


removeVault : VaultId -> Model -> Cmd Msg
removeVault vaultId { config } =
    apiRequest config
        Delete
        (Vault vaultId)
        (Just (Http.jsonBody (jsonOptions config (Syncrypt.Vault.Remove vaultId))))
        (succeed vaultId)
        |> Cmd.map RemovedVaultFromSync


deleteVault : VaultId -> Config -> Cmd (WebData VaultId)
deleteVault vaultId config =
    apiRequest config
        Delete
        (DeleteVault vaultId)
        Nothing
        (succeed vaultId)


sendFeedback : String -> Config -> Cmd (WebData String)
sendFeedback text config =
    apiRequest config
        Post
        Feedback
        (Just (Http.jsonBody (Json.Encode.string text)))
        (succeed "")


getVersion : Config -> Cmd (WebData String)
getVersion config =
    apiRequest config Get Version Nothing Json.string


getLoginState : Model -> Cmd Msg
getLoginState { config } =
    apiRequest config Get User Nothing loginStateDecoder
        |> Cmd.map UpdatedLoginState


login : Email -> Password -> Model -> Cmd Msg
login email password { config } =
    let
        jsonBody =
            Syncrypt.User.loginEncoder email password
                |> Http.jsonBody
    in
        apiRequest config
            Post
            Login
            (Just jsonBody)
            statusResponseDecoder
            |> Cmd.map (LoginResult email)


loginCheck : Config -> Cmd (WebData String)
loginCheck config =
    apiRequest config Get LoginCheck Nothing Json.string


logout : Model -> Cmd Msg
logout { config } =
    apiRequest config Get Logout Nothing statusResponseDecoder
        |> Cmd.map LogoutResult


exportVault : VaultId -> String -> Model -> Cmd Msg
exportVault vaultId path { config } =
    let
        json =
            Json.Encode.object [ ( "path", Json.Encode.string path ) ]
    in
        apiRequest config
            Post
            (ExportVault vaultId)
            (Just (Http.jsonBody json))
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
        apiRequest config Get "vault" (Json.list Syncrypt.Vault.decoder)

-}
apiRequest : Config -> RequestMethod -> ApiPath -> Maybe Http.Body -> Json.Decoder a -> Cmd (WebData a)
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
            |> RemoteData.sendRequest


{-| Create a `Task.Task` from an api request (`Http.Request`)
-}
task : Http.Request a -> Task Http.Error a
task =
    Http.toTask



--attempt : (Result Http.Error a -> Msg) -> Http.Request a -> Cmd (WebData a)


attempt msg request =
    request
        -- |> task
        -- |> Task.attempt msg
        |>
            RemoteData.sendRequest


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
    [ Http.header "X-Authtoken" config.apiAuthToken ]
