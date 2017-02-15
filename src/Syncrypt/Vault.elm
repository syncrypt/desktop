module Syncrypt.Vault exposing (..)

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


type alias FlyingVault =
    { id : VaultId
    , name : Maybe String
    , size : Maybe Int
    , userCount : Int
    , fileCount : Int
    , revisionCount : Int
    , resourceUri : String
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



-- This is pretty cool and will work with `Vault` & `FlyingVault` because both
-- Types have the correctly typed `name` and `id` fields.


nameOrId : { v | name : Maybe String, id : String } -> String
nameOrId vault =
    vault.name
        |> Maybe.withDefault vault.id


asVault : FlyingVault -> Vault
asVault fv =
    { id = fv.id
    , name = fv.name
    , size = Maybe.withDefault 0 fv.size
    , status = Unsynced
    , userCount = fv.userCount
    , fileCount = fv.fileCount
    , revisionCount = fv.revisionCount
    , resourceUri = fv.resourceUri
    , folderPath = ""
    , modificationDate = fv.modificationDate
    }
