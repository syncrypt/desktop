module Syncrypt.Vault exposing (..)

import Dict exposing (Dict)
import Date exposing (Date)


type State
    = Unsynced
    | Syncing
    | Initializing
    | Synced


type alias Vault =
    { id : String
    , description : String
    , metadata : Dict String String
    , state : State
    , modificationDate : Maybe Date
    }


type alias VaultID =
    String


init : VaultID -> Vault
init id =
    { id = id
    , description = ""
    , metadata = Dict.empty
    , state = Initializing
    , modificationDate = Nothing
    }


vaultName : Vault -> String
vaultName vault =
    case Dict.get "name" vault.metadata of
        Just name ->
            name

        Nothing ->
            vault.id
