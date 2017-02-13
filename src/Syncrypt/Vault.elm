module Syncrypt.Vault exposing (..)

import Dict exposing (Dict)
import Date exposing (Date)


type alias VaultId =
    String


type Status
    = Unsynced
    | Syncing
    | Initializing
    | Synced
    | Ready


type alias Vault =
    { id : VaultId
    , name : Maybe String
    , size : Int
    , status : Status
    , userCount : Int
    , fileCount : Int
    , revisionCount : Int
    , resourceUri : String
    , folderPath : String
    , modificationDate : Maybe Date
    }


init : VaultId -> Vault
init vaultId =
    { id = vaultId
    , name = Nothing
    , size = 0
    , status = Initializing
    , userCount = 0
    , fileCount = 0
    , revisionCount = 0
    , resourceUri = ""
    , folderPath = ""
    , modificationDate = Nothing
    }


vaultName : Vault -> String
vaultName vault =
    case vault.name of
        Just name ->
            name

        Nothing ->
            vault.id
