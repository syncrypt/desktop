module Model exposing (..)

import Syncrypt.User exposing (User)
import Syncrypt.Vault exposing (Vault, FlyingVault)
import Config exposing (Config)
import Http
import Date exposing (Date)
import VaultDialog.Model
import VaultDialog.Model
import Syncrypt.Vault exposing (VaultId)
import Dict exposing (Dict)
import Util exposing (findFirst)
import Ui.NotificationCenter


type alias Stats =
    { stats : Int, downloads : Int, uploads : Int }


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
    }


type State
    = LoadingVaults
    | UpdatingVaults (List Vault)
    | ShowingAllVaults
    | ShowingVaultDetails Vault
    | ShowingFlyingVaultDetails FlyingVault
    | CreatingNewVault


type Msg
    = SetDate Date
    | UpdateVaults
    | UpdateFlyingVaults
    | FetchedVaultsFromApi (Result Http.Error (List Vault))
    | UpdatedVaultsFromApi (Result Http.Error (List Vault))
    | UpdatedFlyingVaultsFromApi (Result Http.Error (List FlyingVault))
    | UpdatedStatsFromApi (Result Http.Error Stats)
    | OpenVaultDetails Vault
    | OpenVaultFolder Vault
    | OpenFlyingVaultDetails FlyingVault
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
