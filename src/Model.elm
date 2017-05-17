module Model exposing (..)

import Config exposing (Config)
import Date exposing (Date)
import Dict exposing (Dict)
import Http
import Syncrypt.User exposing (Email)
import Syncrypt.Vault exposing (VaultId, Vault, FlyingVault)
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
    = LoggedOut
    | LoggedIn CurrentUser


type alias Model =
    { config : Config
    , vaults : List Vault
    , flyingVaults : List FlyingVault
    , state : State
    , stats : Stats
    , sidebarOpen : Bool
    , now : Maybe Date
    , vaultDialogs : Dict VaultId VaultDialog.Model.State
    , notificationCenter : Ui.NotificationCenter.Model Msg
    , login : LoginState
    }


type State
    = LoadingVaults
    | UpdatingVaults (List Vault)
    | ShowingAllVaults
    | ShowingVaultDetails Vault
    | ShowingFlyingVaultDetails FlyingVault
    | CreatingNewVault
    | CloningVault VaultId


type Msg
    = SetDate Date
    | UpdateVaults
    | UpdateFlyingVaults
    | FetchedVaultsFromApi (Result Http.Error (List Vault))
    | FetchedLoginState (Result Http.Error LoginState)
    | UpdatedVaultsFromApi (Result Http.Error (List Vault))
    | UpdatedFlyingVaultsFromApi (Result Http.Error (List FlyingVault))
    | UpdatedStatsFromApi (Result Http.Error Stats)
    | OpenVaultDetails Vault
    | OpenVaultFolder Vault
    | OpenFlyingVaultDetails FlyingVault
    | CloneVault VaultId
    | ClonedVault VaultId (Result Http.Error Vault)
    | CloseVaultDetails VaultId
    | SaveVaultDetails VaultId
    | OpenProgramSettings
    | OpenAccountSettings
    | Logout
    | CreateNewVault
    | CreatedVault (Result Http.Error Vault)
    | VaultDialog VaultId VaultDialog.Model.Msg
    | FocusOn String
    | NotificationCenter Ui.NotificationCenter.Msg
    | RemoveVaultFromSync VaultId
    | RemovedVaultFromSync (Result Http.Error VaultId)
    | DeletedVault (Result Http.Error VaultId)
    | VaultUserAdded VaultId Email (Result Http.Error Email)
    | VaultMetadataUpdated VaultId (Result Http.Error Vault)


init : Config -> Model
init config =
    { config = config
    , vaults = []
    , flyingVaults = []
    , state = LoadingVaults
    , stats = { stats = 0, downloads = 0, uploads = 0 }
    , sidebarOpen = False
    , now = Nothing
    , vaultDialogs = Dict.fromList [ ( "", VaultDialog.Model.init ) ]
    , notificationCenter =
        Ui.NotificationCenter.init ()
            |> Ui.NotificationCenter.timeout 2500
            |> Ui.NotificationCenter.duration 2500
    , login = LoggedOut
    }


vaultWithId : VaultId -> Model -> Vault
vaultWithId vaultId { vaults, flyingVaults } =
    let
        hasId =
            \v -> v.id == vaultId
    in
        case findFirst hasId vaults of
            Nothing ->
                case findFirst hasId flyingVaults of
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
    in
        (idsOf vaults) ++ (idsOf flyingVaults)
