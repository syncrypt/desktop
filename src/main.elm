module Main exposing (..)

import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Http
import Model.Config exposing (Config)
import Model.User exposing (User)
import Model.Vault exposing (Vault)
import Dict exposing (Dict)


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
        [ { id = "12312-asadssad-asdasdasd-123231123"
          , description = "foobar"
          , metadata = Dict.empty
          }
        ]
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
            text ("Showing vaults: " ++ (vaults |> List.length |> toString))

        ShowingVaultDetails { id } ->
            text ("Vault details = " ++ id)


update : Action -> Model -> Model
update action model =
    case action of
        UpdateVaults ->
            { model | state = UpdatingVaults model.vaults }

        _ ->
            { model | state = LoadingVaults }
