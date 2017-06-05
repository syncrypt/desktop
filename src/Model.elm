module Model exposing (..)

import Config exposing (Config)
import Date exposing (Date)
import VaultDialog.Model
import Syncrypt.Vault exposing (VaultId)
import Dict exposing (Dict)
import Http
import Process
import RemoteData exposing (RemoteData(..), WebData)
import Syncrypt.User exposing (Email)
import Syncrypt.Vault exposing (FlyingVault, Vault, VaultId)
import Ui.NotificationCenter
import Util exposing (findFirst)
import VaultDialog.Model


type alias Stats =
    { stats : Int, downloads : Int, uploads : Int }


type alias CurrentUser =
    { firstName : String
    , lastName : String
    , email : Syncrypt.User.Email
    }


type LoginState
    = Unknown
    | LoggedOut
    | LoggedIn CurrentUser


type alias StatusResponse =
    { success : Bool, text : Maybe String }


type alias Model =
    { config : Config
    , vaults : WebData (List Vault)
    , flyingVaults : WebData (List FlyingVault)
    , state : State
    , stats : WebData Stats
    , sidebarOpen : Bool
    , now : Maybe Date
    , loggedIn : Bool
    , loginDialog : LoginDialog.Model.State
    , vaultDialogs : Dict VaultId VaultDialog.Model.State
    , notificationCenter : Ui.NotificationCenter.Model Msg
    , login : LoginState
    }


type State
    = LoadingVaults
    | UpdatingVaults
    | ShowingAllVaults
    | ShowingVaultDetails Vault
    | ShowingFlyingVaultDetails FlyingVault
    | CreatingNewVault
    | CloningVault VaultId


type Msg
    = SetDate Date
    | GetLoginState
    | UpdateVaults
    | UpdateFlyingVaults
    | UpdateStats
    | UpdatedLoginState (WebData LoginState)
    | UpdatedVaultsFromApi (WebData (List Vault))
    | UpdatedFlyingVaultsFromApi (WebData (List FlyingVault))
    | UpdatedStatsFromApi (WebData Stats)
    | OpenVaultDetails Vault
    | OpenVaultFolder Vault
    | OpenFlyingVaultDetails FlyingVault
    | CloneVault VaultId
    | ClonedVault VaultId (WebData Vault)
    | CloseVaultDetails VaultId
    | SaveVaultDetails VaultId
    | OpenProgramSettings
    | OpenAccountSettings
    | Logout
    | CreateNewVault
    | CreatedVault VaultDialog.Model.State (WebData Vault)
    | VaultDialog VaultId VaultDialog.Model.Msg
    | FocusOn String
    | NotificationCenter Ui.NotificationCenter.Msg
    | RemoveVaultFromSync VaultId
    | RemovedVaultFromSync (WebData VaultId)
    | DeletedVault (WebData VaultId)
    | VaultUserAdded VaultId Email (WebData Email)
    | VaultMetadataUpdated VaultId (WebData Vault)
    | Login
    | LoginResult Email (WebData StatusResponse)
    | LogoutResult (WebData StatusResponse)
    | LoginDialog LoginDialog.Model.Msg


init : Config -> Model
init config =
    { config = config
    , vaults = NotAsked
    , flyingVaults = NotAsked
    , state = LoadingVaults
    , stats = NotAsked
    , sidebarOpen = False
    , now = Nothing
    , loginDialog = LoginDialog.Model.init
    , vaultDialogs = Dict.fromList [ ( "", VaultDialog.Model.init ) ]
    , notificationCenter =
        Ui.NotificationCenter.init ()
            |> Ui.NotificationCenter.timeout 2500
            |> Ui.NotificationCenter.duration 2500
    , login = Unknown
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
                        Syncrypt.Vault.init vaultId

                    Just fv ->
                        fv |> Syncrypt.Vault.asVault

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
        (idsOf (vaults |> orEmpty)) ++ (idsOf (flyingVaults |> orEmpty))



-- retryOnFailure : WebData a -> Model -> ( Model, Cmd msg )


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
