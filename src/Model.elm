module Model exposing (..)

import Config exposing (Config)
import Data.Daemon exposing (DaemonConfig, KeyState(..), Stats)
import Data.User exposing (Email)
import Data.Vault exposing (FlyingVault, Status, Vault, VaultId)
import Date exposing (Date)
import Dict exposing (Dict)
import Json.Decode as Json exposing (andThen, succeed)
import Json.Decode.Pipeline
    exposing
        ( decode
        , optional
        , optionalAt
        , required
        , requiredAt
        )
import Language exposing (Language(..))
import LoginDialog.Model
import RemoteData exposing (RemoteData(..), WebData)
import SettingsDialog.Model
import Ui.NotificationCenter
import Util exposing (findFirst)
import VaultDialog.Model
import WizardDialog.Model


type alias Model =
    { config : Config
    , vaults : WebData (List Vault)
    , flyingVaults : WebData (List FlyingVault)
    , state : State
    , stats : WebData Stats
    , sidebarOpen : Bool
    , isFirstLaunch : Bool
    , now : Maybe Date
    , loginDialog : LoginDialog.Model.State
    , vaultDialogs : Dict VaultId VaultDialog.Model.State
    , notificationCenter : Ui.NotificationCenter.Model Msg
    , login : LoginState
    , language : Language
    , wizardDialog : Maybe (WizardDialog.Model.State Msg)
    , settingsDialog : SettingsDialog.Model.State
    , emailCompletionList : List Email
    , feedback : Maybe String
    , setupWizard : SetupWizardState
    , daemonLogItems : List Data.Daemon.LogItem
    }


type alias SetupWizardState =
    { email : Maybe String
    , password : Maybe String
    , passwordResetSent : Bool
    }


type alias CurrentUser =
    { firstName : String
    , lastName : String
    , email : Data.User.Email
    }


type LoginState
    = Unknown
    | LoggedOut
    | LoggedIn CurrentUser


type alias StatusResponse =
    { success : Bool, text : Maybe String }


type alias ExportStatusResponse =
    { success : Bool, filename : String }


type State
    = LoadingVaults
    | UpdatingVaults
    | ShowingAllVaults
    | ShowingVaultDetails Vault
    | ShowingFlyingVaultDetails FlyingVault
    | CreatingNewVault
    | CloningVault VaultId
    | ShowingDaemonLog


type Msg
    = SetTime Date
    | UpdateLoginState
    | UpdateVaults
    | UpdateFlyingVaults
    | UpdateStats
    | UpdatedLoginState (WebData LoginState)
    | UpdatedVaultsFromApi (WebData (List Vault))
    | UpdatedFlyingVaultsFromApi (WebData (List FlyingVault))
    | UpdatedStatsFromApi (WebData Stats)
    | UpdateDaemonConfig
    | UpdatedDaemonConfig (WebData DaemonConfig)
    | OpenVaultDetails Vault
    | OpenVaultFolder Vault
    | OpenFlyingVaultDetails FlyingVault
    | CloneVault VaultId
    | ClonedVault VaultId (WebData Vault)
    | CloseVaultDetails VaultId
    | SaveVaultDetails VaultId
    | DeleteVaultDialog VaultId
    | OpenSettingsDialog
    | CloseSettingsDialog
    | Logout
    | CreateNewVault
    | CreatedVault VaultDialog.Model.State (WebData Vault)
    | ExportedVault VaultId (WebData ExportStatusResponse)
    | VaultDialogMsg VaultId VaultDialog.Model.Msg
    | FocusOn String
    | NotificationCenterMsg Ui.NotificationCenter.Msg
    | RemoveVaultFromSync VaultId
    | RemovedVaultFromSync (WebData VaultId)
    | DeletedVault (WebData VaultId)
    | VaultUserAdded VaultId Email (WebData Email)
    | VaultMetadataUpdated VaultId (WebData Vault)
    | Login
    | LoginResult Email (WebData StatusResponse)
    | LogoutResult (WebData StatusResponse)
    | LoginDialogMsg LoginDialog.Model.Msg
    | WizardDialogMsg WizardDialog.Model.Msg
    | SettingsDialogMsg SettingsDialog.Model.Msg
    | OpenSetupWizardDialog
    | SetupWizardFinished
    | EmailCompletionList (List Email)
    | OpenFeedbackWizard
    | SentFeedback (WebData String)
    | FeedbackEntered String
    | SendFeedback
    | SetLanguage Language
    | SendPasswordResetLink
    | SetupWizardEmail String
    | SetupWizardPassword String
    | OpenUserKeyExportDialog
    | SelectedUserKeyExportFile String
    | ExportedUserKey (WebData ExportStatusResponse)
    | OpenDaemonLogDialog
    | CloseDaemonLogDialog
    | DaemonLogStream (Result String Data.Daemon.LogItem)



-- JSON Decoders


statsDecoder : Json.Decoder Stats
statsDecoder =
    decode Stats
        |> requiredAt [ "stats", "stats" ] Json.int
        |> requiredAt [ "stats", "downloads" ] Json.int
        |> requiredAt [ "stats", "uploads" ] Json.int
        |> requiredAt [ "user_key_state" ] keyStateDecoder
        |> requiredAt [ "slots", "total" ] Json.int
        |> optionalAt [ "slots", "busy" ] Json.int 0
        |> optionalAt [ "slots", "idle" ] Json.int 0
        |> optionalAt [ "slots", "closed" ] Json.int 0
        |> required "states" (Json.dict Data.Vault.vaultStatusDecoder)


keyStateDecoder : Json.Decoder KeyState
keyStateDecoder =
    let
        parseKeyState s =
            succeed <|
                case s of
                    "initializing" ->
                        Initializing

                    "initialized" ->
                        Initialized

                    _ ->
                        Uninitialized
    in
    Json.string
        |> andThen parseKeyState


loginStateDecoder : Json.Decoder LoginState
loginStateDecoder =
    decode CurrentUser
        |> required "first_name" Json.string
        |> required "last_name" Json.string
        |> required "email" Json.string
        |> andThen (succeed << LoggedIn)


statusResponseDecoder : Json.Decoder StatusResponse
statusResponseDecoder =
    let
        parseStatus =
            Json.string
                |> andThen (\s -> succeed (s == "ok"))
    in
    decode StatusResponse
        |> required "status" parseStatus
        |> optional "text" (Json.maybe Json.string) Nothing


exportStatusResponseDecoder : Json.Decoder ExportStatusResponse
exportStatusResponseDecoder =
    let
        parseStatus =
            Json.string
                |> andThen (\s -> succeed (s == "ok"))
    in
    decode ExportStatusResponse
        |> required "status" parseStatus
        |> required "filename" Json.string



-- Model functions


init : Config -> Model
init config =
    { config = config
    , vaults = NotAsked
    , flyingVaults = NotAsked
    , state = LoadingVaults
    , stats = NotAsked
    , sidebarOpen = False
    , isFirstLaunch = False
    , now = Nothing
    , loginDialog = LoginDialog.Model.init
    , vaultDialogs = Dict.fromList [ ( "", VaultDialog.Model.init ) ]
    , notificationCenter =
        Ui.NotificationCenter.init ()
            |> Ui.NotificationCenter.timeout 2500
            |> Ui.NotificationCenter.duration 2500
    , login = Unknown
    , language = Language.English
    , wizardDialog = Nothing
    , settingsDialog = SettingsDialog.Model.init
    , emailCompletionList = []
    , feedback = Nothing
    , setupWizard =
        { email = Nothing
        , password = Nothing
        , passwordResetSent = False
        }
    , daemonLogItems = []
    }


vaultWithId : VaultId -> Model -> Vault
vaultWithId vaultId { vaults, flyingVaults } =
    let
        hasId =
            \v -> v.id == vaultId
    in
    case findFirst hasId (RemoteData.withDefault [] vaults) of
        Nothing ->
            case findFirst hasId (RemoteData.withDefault [] flyingVaults) of
                Nothing ->
                    Data.Vault.init vaultId

                Just fv ->
                    fv |> Data.Vault.asVault

        Just v ->
            v


vaultIds : Model -> List VaultId
vaultIds { vaults, flyingVaults } =
    let
        idsOf =
            List.map .id

        orEmpty =
            RemoteData.withDefault []
    in
    idsOf (vaults |> orEmpty) ++ idsOf (flyingVaults |> orEmpty)


retryOnFailure : WebData a -> msg -> Model -> ( Model, Cmd msg )
retryOnFailure data msg model =
    case data of
        Failure reason ->
            let
                _ =
                    Debug.log "Retrying due to failure: " ( msg, reason )
            in
            ( model, Util.delayMsg model.config.updateInterval msg )

        _ ->
            ( model, Cmd.none )


hasVaultWithId : VaultId -> Model -> Bool
hasVaultWithId id model =
    model.vaults
        |> RemoteData.withDefault []
        |> List.any (\v -> v.remoteId == id)


type alias HasVaultId a =
    { a | id : VaultId, remoteId : VaultId }


unclonedFlyingVaults : List (HasVaultId a) -> Model -> List (HasVaultId a)
unclonedFlyingVaults flyingVaults model =
    flyingVaults
        |> List.filter (\fv -> isClonedVault fv model)


isClonedVault : HasVaultId a -> Model -> Bool
isClonedVault { remoteId } model =
    not <| hasVaultWithId remoteId model


vaultStatus : Status -> HasVaultId a -> Model -> Status
vaultStatus default { id, remoteId } model =
    case model.stats of
        Success stats ->
            case Dict.get ("/v1/vault/" ++ id ++ "/") stats.states of
                Just foundStatus ->
                    foundStatus

                Nothing ->
                    let
                        _ =
                            Debug.log "Failed to get vault status: " ( id, remoteId, stats )
                    in
                    default

        _ ->
            default


addDaemonLogItem : Data.Daemon.LogItem -> Model -> Model
addDaemonLogItem item model =
    case model.now of
        Just now ->
            { model
                | daemonLogItems =
                    item
                        :: model.daemonLogItems
                        -- TODO: fix this code / refactor and reuse the similar code from VaultDialog
                        |> List.sortBy (.createdAt >> Maybe.withDefault now >> Date.toTime)
                        |> List.reverse
                        |> List.take 500
            }

        Nothing ->
            model
