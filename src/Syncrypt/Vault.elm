module Syncrypt.Vault exposing (..)

import Date exposing (Date)
import Syncrypt.User exposing (User, UserKey, Email, Fingerprint, EmailWithFingerPrint)
import Config exposing (Config)
import Json.Encode
import Json.Decode as Json exposing (andThen, fail, succeed)
import Json.Decode.Pipeline exposing (custom, decode, hardcoded, optional, optionalAt, required, requiredAt)
import Util exposing (dateDecoder)
import Path exposing (Path)


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
        { folder : Path
        , ignorePaths : List Path
        }
    | Clone
        { id : String
        , folder : Path
        , ignorePaths : List Path
        }
    | Remove VaultId
    | Delete VaultId


type alias Metadata =
    { name : String, icon : Maybe String }


type alias CryptoInfo =
    { aesKeyLength : Int
    , rsaKeyLength : Int
    , keyAlgorithm : String
    , transferAlgorithm : String
    , hashAlgorithm : String
    , fingerprint : Maybe String
    }


type alias VaultLogItems =
    { items : List VaultLogItem }


type alias VaultLogItem =
    { createdAt : String
    , operation : String
    , path : String
    , email : String
    }


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
    , icon : Maybe String
    , crypto : CryptoInfo
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
    , icon : Maybe String
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
    , icon = Nothing
    , crypto =
        { aesKeyLength = 0
        , rsaKeyLength = 0
        , keyAlgorithm = ""
        , transferAlgorithm = ""
        , hashAlgorithm = ""
        , fingerprint = Nothing
        }
    }



-- This is pretty cool and will work with `Vault` & `FlyingVault` because both
-- Types have the correctly typed `name` and `id` fields.


nameOrId : NameOrId a -> String
nameOrId vault =
    vault.name
        |> Maybe.withDefault vault.id


asVault : FlyingVault -> Vault
asVault fv =
    let
        v =
            init fv.id
    in
        { v
            | id = fv.id
            , name = fv.name
            , size = Maybe.withDefault 0 fv.size
            , status = Unsynced
            , userCount = fv.userCount
            , fileCount = fv.fileCount
            , revisionCount = fv.revisionCount
            , resourceUri = fv.resourceUri
            , folderPath = ""
            , modificationDate = fv.modificationDate
            , icon = fv.icon
        }


jsonOptions : Config -> VaultOptions -> Json.Encode.Value
jsonOptions config options =
    let
        pathString path =
            String.join config.pathSeparator path
    in
        case options of
            Create { folder, ignorePaths } ->
                Json.Encode.object
                    [ ( "folder", Json.Encode.string (pathString folder) )
                    , ( "ignorePaths"
                      , Json.Encode.list
                            (List.map (pathString >> Json.Encode.string) ignorePaths)
                      )
                    ]

            Clone { id, folder } ->
                Json.Encode.object
                    [ ( "id", Json.Encode.string id )
                    , ( "folder", Json.Encode.string (pathString folder) )
                    ]

            Remove id ->
                Json.Encode.null

            Delete id ->
                Json.Encode.null


{-| Decodes a `Syncrypt.Vault.Vault`.
-}
decoder : Json.Decoder Vault
decoder =
    decode Vault
        |> required "id" Json.string
        |> optionalAt [ "metadata", "name" ] (Json.maybe Json.string) Nothing
        |> required "size" Json.int
        |> required "state" vaultStatusDecoder
        |> required "user_count" Json.int
        |> required "file_count" Json.int
        |> required "revision_count" Json.int
        |> required "resource_uri" Json.string
        |> required "folder" Json.string
        |> optional "modification_date" dateDecoder Nothing
        |> optionalAt [ "metadata", "icon" ] (Json.maybe Json.string) Nothing
        |> required "crypt_info" cryptoInfoDecoder


cryptoInfoDecoder : Json.Decoder CryptoInfo
cryptoInfoDecoder =
    decode CryptoInfo
        |> required "aes_key_len" Json.int
        |> required "rsa_key_len" Json.int
        |> required "key_algo" Json.string
        |> required "transfer_algo" Json.string
        |> required "hash_algo" Json.string
        |> required "fingerprint" (Json.maybe Json.string)


logItemsDecoder : Json.Decoder (List VaultLogItem)
logItemsDecoder =
    decode VaultLogItems
        |> required "items" (Json.list logItemDecoder)
        |> andThen (\{ items } -> Json.succeed items)


logItemDecoder : Json.Decoder VaultLogItem
logItemDecoder =
    decode VaultLogItem
        |> required "created_at" Json.string
        |> required "operation" Json.string
        |> required "path" Json.string
        |> required "user_email" Json.string


{-| Decodes a `Syncrypt.Vault.FlyingVault`.
-}
flyingVaultDecoder : Json.Decoder FlyingVault
flyingVaultDecoder =
    decode FlyingVault
        |> required "id" Json.string
        |> optionalAt [ "metadata", "name" ] (Json.maybe Json.string) Nothing
        |> optional "size" (Json.maybe Json.int) Nothing
        |> required "user_count" Json.int
        |> required "file_count" Json.int
        |> required "revision_count" Json.int
        |> required "resource_uri" Json.string
        |> optional "modification_date" dateDecoder Nothing
        |> optional "icon" (Json.maybe Json.string) Nothing


{-| Decodes a `Syncrypt.Vault.Status`.
-}
vaultStatusDecoder : Json.Decoder Status
vaultStatusDecoder =
    let
        convert : String -> Json.Decoder Status
        convert raw =
            case raw of
                "unsynced" ->
                    succeed Unsynced

                "syncing" ->
                    succeed Syncing

                "initializing" ->
                    succeed Initializing

                "synced" ->
                    succeed Synced

                "ready" ->
                    succeed Ready

                val ->
                    fail ("Invalid vault status: " ++ val)
    in
        Json.string |> andThen convert


metadataEncoder : Metadata -> Json.Encode.Value
metadataEncoder metadata =
    let
        metadataJson =
            case metadata.icon of
                Nothing ->
                    Json.Encode.object
                        [ ( "name", Json.Encode.string metadata.name ) ]

                Just iconUrl ->
                    Json.Encode.object
                        [ ( "name", Json.Encode.string metadata.name )
                        , ( "icon", Json.Encode.string iconUrl )
                        ]
    in
        Json.Encode.object [ ( "metadata", metadataJson ) ]


addVaultUserEncoder : Email -> List UserKey -> Json.Encode.Value
addVaultUserEncoder email keys =
    let
        fingerprints =
            List.map .fingerprint keys
    in
        Json.Encode.object
            [ ( "email", Json.Encode.string email )
            , ( "fingerprints", Json.Encode.list (List.map Json.Encode.string fingerprints) )
            ]
