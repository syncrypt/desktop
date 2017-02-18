module Main exposing (..)

import Config exposing (Config)
import Daemon exposing (attempt, attemptDelayed)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import Model exposing (..)
import View.MainScreen


main : Program Never Model Msg
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
        model ! (updateAllVaults model.config)


initialModel : Model
initialModel =
    { config = Config.initialConfig
    , vaults = []
    , flyingVaults = []
    , state = LoadingVaults
    , stats =
        -- TODO: get these from stats api
        { stats = 0, downloads = 0, uploads = 0 }
    , sidebarOpen = False
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
            { model | state = UpdatingVaults model.vaults }
                ! (updateAllVaults model.config)

        UpdateFlyingVaults ->
            model
                ! [ model.config
                        |> Daemon.getFlyingVaults
                        |> attempt UpdatedFlyingVaultsFromApi
                  ]

        UpdatedVaultsFromApi (Ok vaults) ->
            { model | state = ShowingAllVaults, vaults = vaults }
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
            { model | state = ShowingAllVaults, flyingVaults = vaults }
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

        _ ->
            { model | state = LoadingVaults }
                ! []


updateAllVaults : Config -> List (Cmd Msg)
updateAllVaults config =
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

        _ ->
            View.MainScreen.view model
