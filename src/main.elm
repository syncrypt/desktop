module Main exposing (..)

import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Http
import Config exposing (Config)
import Syncrypt.User exposing (User)
import Syncrypt.Vault exposing (Vault)
import Dict exposing (Dict)
import View.VaultList
import Model exposing (..)


main =
    Html.beginnerProgram
        { model = initialModel
        , view = view
        , update = update
        }


initialModel : Model
initialModel =
    { config =
        { apiUrl = "http://localhost:28080/api/v1/"
        , apiAuthToken = "my API token here"
        }
    , vaults =
        [ Syncrypt.Vault.init "12312-asadssad-asdasdasd-123231123" ]
    , state = LoadingVaults
    }


view : Model -> Html.Html Action
view { config, state } =
    case state of
        LoadingVaults ->
            div [ class "vault-list", onClick UpdateVaults ]
                [ Html.text "Loading Vaults" ]

        UpdatingVaults vaults ->
            text ("Updating vaults: " ++ (vaults |> List.length |> toString))

        ShowingAllVaults vaults ->
            div []
                (text ("Showing vaults: " ++ (vaults |> List.length |> toString))
                    :: List.map View.VaultList.vaultItem vaults
                )

        ShowingVaultDetails { id } ->
            text ("Vault details = " ++ id)


update : Action -> Model -> Model
update action model =
    case action of
        UpdateVaults ->
            { model | state = UpdatingVaults model.vaults }

        _ ->
            { model | state = LoadingVaults }
