module View.MainScreen exposing (..)

import Config exposing (Config)
import Daemon exposing (attempt, attemptDelayed)
import Date exposing (Date)
import Html exposing (Html, button, div, h1, node, span, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Model exposing (..)
import Util
import View.Css.MainScreen exposing (..)
import View.Css.Util
import View.VaultCreationDialog
import View.VaultDialog
import View.VaultList
import Time exposing (Time)
import Task


{-| Custom HTML helpers using our CSS types
-}
{ id, class, classList } =
    View.Css.Util.namespacedHelpers View.Css.MainScreen.namespace



-- INIT


init : Config -> ( Model, Cmd Msg )
init config =
    let
        model =
            Model.init config

        initialActions =
            [ updateNow
            , model.config
                |> Daemon.getVaults
                |> attempt FetchedVaultsFromApi
            , model.config
                |> Daemon.getFlyingVaults
                |> attempt UpdatedFlyingVaultsFromApi
            ]
    in
        model ! initialActions



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        SetDate date ->
            { model | now = Just date } ! [ updateNowIn 1000 ]

        UpdateVaults ->
            { model | state = UpdatingVaults model.vaults }
                ! [ model.config
                        |> Daemon.getVaults
                        |> attempt UpdatedVaultsFromApi
                  , model.config
                        |> Daemon.getFlyingVaults
                        |> attempt UpdatedFlyingVaultsFromApi
                  ]

        UpdateFlyingVaults ->
            model
                ! [ model.config
                        |> Daemon.getFlyingVaults
                        |> attempt UpdatedFlyingVaultsFromApi
                  ]

        FetchedVaultsFromApi (Ok vaults) ->
            { model | state = ShowingAllVaults, vaults = vaults }
                ! [ model.config
                        |> Daemon.getVaults
                        |> attemptDelayed model.config.updateInterval UpdatedVaultsFromApi
                  ]

        FetchedVaultsFromApi (Err reason) ->
            model
                ! [ model.config
                        |> Daemon.getVaults
                        |> attemptDelayed 1000 FetchedVaultsFromApi
                  ]

        UpdatedVaultsFromApi (Ok vaults) ->
            { model | vaults = vaults }
                ! [ model.config
                        |> Daemon.getVaults
                        |> attemptDelayed model.config.updateInterval UpdatedVaultsFromApi
                  ]

        UpdatedVaultsFromApi (Err reason) ->
            -- retry to get vaults if request failed
            model
                ! [ model.config
                        |> Daemon.getVaults
                        |> attemptDelayed 1000 UpdatedVaultsFromApi
                  ]

        UpdatedFlyingVaultsFromApi (Ok vaults) ->
            { model | flyingVaults = vaults }
                ! []

        UpdatedFlyingVaultsFromApi (Err reason) ->
            model
                ! [ model.config
                        |> Daemon.getFlyingVaults
                        |> attemptDelayed 1000 UpdatedFlyingVaultsFromApi
                  ]

        OpenVaultDetails vault ->
            { model | state = ShowingVaultDetails vault }
                ! []

        OpenFlyingVaultDetails flyingVault ->
            { model | state = ShowingFlyingVaultDetails flyingVault }
                ! []

        CloseVaultDetails ->
            { model | state = ShowingAllVaults } ! []

        CreateNewVault ->
            { model
                | state = CreatingNewVault
                , vaultCreationDialog =
                    View.VaultCreationDialog.open model.vaultCreationDialog
            }
                ! []

        VaultCreationDialog msg ->
            let
                ( state, cmd ) =
                    View.VaultCreationDialog.update msg model.vaultCreationDialog
            in
                { model | vaultCreationDialog = state }
                    ! [ Cmd.map VaultCreationDialog cmd ]

        _ ->
            model
                ! []


updateNow : Cmd Msg
updateNow =
    Task.perform SetDate Date.now


updateNowIn : Time -> Cmd Msg
updateNowIn time =
    Util.performDelayed time SetDate Date.now



-- VIEW


view : Model -> Html Msg
view model =
    let
        _ =
            Debug.log "state: " model.state
    in
        case model.state of
            ShowingVaultDetails vault ->
                layout model
                    []

            _ ->
                layout model []


currentClass : Model -> List CssClass
currentClass model =
    if model.sidebarOpen then
        [ Container ]
    else
        [ Container, Expanded ]


layout : Model -> List (Html Msg) -> Html Msg
layout model nodes =
    div [ class (currentClass model) ]
        [ header
        , div [ class [ Container ] ]
            (nodes ++ [ View.VaultList.view model ])
        , footer model
        , View.VaultDialog.view model
        , Html.map VaultCreationDialog (View.VaultCreationDialog.view model.vaultCreationDialog)
        ]


header : Html Msg
header =
    div [ class [ Header ] ]
        [ div [ class [ Buttons ] ]
            [ node "ReactTooltip"
                [ attribute "delayShow" "250"
                , attribute "effect" "solid"
                , id "header-tooltip"
                , attribute "place" "bottom"
                , attribute "type" "dark"
                ]
                []
            , text "    "
            , node "IconButton"
                [ attribute
                    "data-for"
                    "header-tooltip"
                , attribute "data-tip" "Account Settings"
                , attribute "icon" "settings"
                , onClick OpenAccountSettings
                ]
                []
            , text "    "
            , node "IconButton"
                [ attribute "data-for" "header-tooltip"
                , attribute "data-tip" "Logout"
                , attribute "icon" "logout"
                , onClick Logout
                ]
                []
            , text "  "
            ]
        ]


footer : Model -> Html Msg
footer { stats, vaults } =
    let
        statsStr =
            [ stats.stats, stats.downloads, stats.uploads ]
                |> List.map toString
                |> String.join " / "
    in
        div [ class [ Footer ] ]
            [ span [ class [ Stats ] ]
                [ text <|
                    (vaults |> List.length |> toString)
                        ++ " Vault(s) / "
                        ++ statsStr
                ]
            ]
