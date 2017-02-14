module Main exposing (..)

import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Config exposing (Config)
import Syncrypt.User exposing (User)
import Syncrypt.Vault exposing (Vault)
import Dict exposing (Dict)
import View.VaultList
import Model exposing (..)
import Daemon exposing (task, attemptDelayed)
import Debug
import Task exposing (attempt)
import Platform.Cmd exposing (batch)


main =
    Html.program
        { init = init
        , subscriptions = subscriptions
        , view = view
        , update = update
        }


init : ( Model, Cmd Msg )
init =
    let
        model =
            initialModel
    in
        ( model
        , batch
            [ model.config
                |> Daemon.getVaults
                |> task
                |> attempt UpdatedVaultsFromApi
            , model.config
                |> Daemon.getFlyingVaults
                |> task
                |> attempt UpdatedFlyingVaultsFromApi
            ]
        )


initialModel : Model
initialModel =
    { config =
        { apiUrl = "http://localhost:28080/v1/"
        , apiAuthToken =
            -- set this to your actual api auth token
            "my API token here"
        }
    , vaults = []
    , flyingVaults = []
    , state = LoadingVaults
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html.Html Msg
view model =
    case model.state of
        LoadingVaults ->
            div [ class "vault-list", onClick UpdateVaults ]
                [ Html.text "Loading Vaults" ]

        UpdatingVaults vaults ->
            text ("Updating vaults: " ++ (vaults |> List.length |> toString))

        ShowingAllVaults ->
            div []
                (text ("Showing vaults: " ++ (model.vaults |> List.length |> toString))
                    :: List.map View.VaultList.vaultItem model.vaults
                )

        ShowingVaultDetails { id } ->
            text ("Vault details = " ++ id)


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        UpdateVaults ->
            ( { model | state = UpdatingVaults model.vaults }
            , model.config
                |> Daemon.getVaults
                |> task
                |> attempt UpdatedVaultsFromApi
            )

        UpdateFlyingVaults ->
            ( model
            , model.config
                |> Daemon.getFlyingVaults
                |> task
                |> attempt UpdatedFlyingVaultsFromApi
            )

        UpdatedVaultsFromApi (Ok vaults) ->
            ( { model
                | state = ShowingAllVaults
                , vaults = vaults
              }
            , Cmd.none
            )

        UpdatedVaultsFromApi (Err reason) ->
            -- retry to get vaults if request failed
            ( model
            , model.config
                |> Daemon.getVaults
                |> attemptDelayed 1000 UpdatedVaultsFromApi
            )

        UpdatedFlyingVaultsFromApi (Ok vaults) ->
            ( { model
                | state = ShowingAllVaults
                , flyingVaults = vaults
              }
            , Cmd.none
            )

        UpdatedFlyingVaultsFromApi (Err reason) ->
            ( model
            , model.config
                |> Daemon.getFlyingVaults
                |> attemptDelayed 1000 UpdatedFlyingVaultsFromApi
            )

        OpenVaultDetails vault ->
            ( { model | state = ShowingVaultDetails vault }, Cmd.none )

        _ ->
            ( { model | state = LoadingVaults }, Cmd.none )
