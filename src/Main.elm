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
import Api
import Debug


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
        , Cmd.batch
            [ Api.getVaults model.config
            , Api.getFlyingVaults model.config
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
            , Api.getVaults model.config
            )

        UpdateFlyingVaults ->
            ( model
            , Api.getFlyingVaults model.config
            )

        UpdatedVaultsFromApi (Ok vaults) ->
            ( { model
                | state = ShowingAllVaults
                , vaults = vaults
              }
            , Cmd.none
            )

        UpdatedFlyingVaultsFromApi (Ok vaults) ->
            ( { model
                | state = ShowingAllVaults
                , flyingVaults = vaults
              }
            , Cmd.none
            )

        UpdatedFlyingVaultsFromApi (Err reason) ->
            let
                _ =
                    Debug.crash (toString reason)
            in
                ( model, Cmd.none )

        UpdatedVaultsFromApi (Err reason) ->
            let
                _ =
                    Debug.crash (toString reason)
            in
                -- retry to get vaults if request failed
                ( model, Api.getVaults model.config )

        OpenVaultDetails vault ->
            ( { model | state = ShowingVaultDetails vault }, Cmd.none )

        _ ->
            ( { model | state = LoadingVaults }, Cmd.none )
