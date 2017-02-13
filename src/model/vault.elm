module Model.Vault exposing (..)

import Dict exposing (Dict)


type alias Vault =
    { id : String
    , description : String
    , metadata : Dict String String
    }


vaultName : Vault -> String
vaultName vault =
    case Dict.get "name" vault.metadata of
        Just name ->
            name

        Nothing ->
            vault.id
