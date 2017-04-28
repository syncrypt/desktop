module Syncrypt.Vault exposing (..)

import Date exposing (Date)
import Syncrypt.User exposing (User, UserKey, Fingerprint, EmailWithFingerPrint)
import Json.Encode as Json
import Config exposing (Config)


type alias VaultId =
    String


{-| Record type with an id and optional name field.
-}
type alias NameOrId vault =
    { vault | name : Maybe String, id : VaultId }


{-| Vault status as returned from Daemon API.
-}
type Status
    = Unsynced
    | Syncing
    | Initializing
    | Synced
    | Ready


type VaultOptions
    = Create
        { folder : List String
        , userKeys : List Fingerprint
        , ignorePaths : List (List String)
        }
    | Clone VaultId String


{-| Main vault type. Represents all vaults cloned & synced on current computer.
-}
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


{-| Remote vaults that haven't been cloned or synced to this computer.
-}
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


nameOrId : NameOrId a -> String
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


jsonOptions : Config -> VaultOptions -> Json.Value
jsonOptions config options =
    case options of
        Create { folder, userKeys, ignorePaths } ->
            Json.object
                [ ( "folder", Json.string (String.join config.pathSeparator folder) )
                , ( "userKeys", Json.list (List.map Json.string userKeys) )
                , ( "ignorePaths"
                  , Json.list
                        (List.map
                            (\p -> Json.list (List.map Json.string p))
                            ignorePaths
                        )
                  )
                ]

        Clone vaultId folder ->
            Json.object
                [ ( "id", Json.string vaultId )
                , ( "folder", Json.string folder )
                ]
