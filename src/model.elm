module Model exposing (..)

import Syncrypt.Vault exposing (Vault)
import Config exposing (Config)


type alias Model =
    { config : Config
    , vaults : List Vault
    , state : State
    }


type State
    = LoadingVaults
    | UpdatingVaults (List Vault)
    | ShowingAllVaults (List Vault)
    | ShowingVaultDetails Vault


type Action
    = UpdateVaults
    | OpenVaultDetails Vault
    | CloseVaultDetails Vault
    | OpenProgramSettings
