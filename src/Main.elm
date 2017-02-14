module Main exposing (..)

import Html exposing (Html, button, div, text, h1)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Config exposing (Config)
import Syncrypt.User exposing (User)
import Syncrypt.Vault exposing (Vault)
import Dict exposing (Dict)
import View.VaultList
import Model exposing (..)
import Daemon exposing (attempt, attemptDelayed)
import Debug
import Platform.Cmd exposing (batch)


main =
    Html.program
        { init = init
        , subscriptions = subscriptions
        , view = view
        , update = update
        }



-- INIT


init : ( Model, Cmd Msg )
init =
    let
        model =
            initialModel
    in
        ( model, updateAllVaults model.config )


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



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        UpdateVaults ->
            ( { model | state = UpdatingVaults model.vaults }
            , updateAllVaults model.config
            )

        UpdateFlyingVaults ->
            ( model
            , model.config
                |> Daemon.getFlyingVaults
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


updateAllVaults : Config -> Cmd Msg
updateAllVaults config =
    batch
        [ config
            |> Daemon.getVaults
            |> attempt UpdatedVaultsFromApi
        , config
            |> Daemon.getFlyingVaults
            |> attempt UpdatedFlyingVaultsFromApi
        ]



-- VIEW


view : Model -> Html.Html Msg
view model =
    case model.state of
        LoadingVaults ->
            div [ class "vault-list" ]
                [ Html.text "Loading Vaults" ]

        UpdatingVaults vaults ->
            text ("Updating vaults: " ++ (vaults |> List.length |> toString))

        ShowingAllVaults ->
            viewAllVaults model

        ShowingVaultDetails { id } ->
            text ("Vault details = " ++ id)


viewAllVaults model =
    let
        vaultsHeader =
            h1 [] [ text "Local Vaults:" ]

        flyingVaultsHeader =
            case model.flyingVaults of
                [] ->
                    text ""

                vs ->
                    h1 []
                        [ text "Remote Vaults:" ]

        vaultCards =
            div [ class "vault-list" ]
                (List.map View.VaultList.vaultItem model.vaults)

        flyingVaultCards =
            div [ class "flying-vault-list" ]
                (List.map View.VaultList.flyingVaultItem model.flyingVaults)
    in
        div []
            [ vaultsHeader
            , vaultCards
            , flyingVaultsHeader
            , flyingVaultCards
            ]
