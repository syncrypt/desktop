module MainScreen exposing (..)

import Config exposing (Config)
import Daemon exposing (attempt, attemptDelayed)
import Date exposing (Date)
import Html exposing (Html, button, div, h1, node, span, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Model exposing (..)
import Ports
import Time exposing (Time)
import Ui.NotificationCenter
import Util exposing (andAlso)
import VaultDialog
import VaultDialog.Update exposing (dialogState)
import VaultList
import Syncrypt.Vault exposing (VaultOptions(..))
import Set


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
    VaultDialog.subscriptions model



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
            (model
                ! [ model.config
                        |> Daemon.getVaults
                        |> attemptDelayed 1000 FetchedVaultsFromApi
                  ]
            )
                |> Util.andAlso (notifyText ("Error fetching vaults: " ++ toString (reason)))

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
                |> VaultDialog.Update.open

        OpenFlyingVaultDetails flyingVault ->
            { model | state = ShowingFlyingVaultDetails flyingVault }
                ! []

        CloseVaultDetails vaultId ->
            { model | state = ShowingAllVaults }
                |> VaultDialog.Update.cancel vaultId

        SaveVaultDetails vaultId ->
            { model | state = ShowingAllVaults }
                |> VaultDialog.Update.close vaultId
                |> andAlso (saveVault vaultId)

        CreateNewVault ->
            { model | state = CreatingNewVault }
                |> VaultDialog.Update.openNew

        CreatedVault (Ok vault) ->
            model
                |> notifyText ("Vault created: " ++ vault.id)

        CreatedVault (Err reason) ->
            model
                |> notifyText ("Vault creation failed: " ++ (toString reason))

        RemoveVault vaultId ->
            model
                ! [ model.config
                        |> Daemon.removeVault vaultId
                        |> attempt RemovedVault
                  ]

        RemovedVault (Ok vaultId) ->
            model
                |> notifyText ("Vault removed from sync: " ++ vaultId)

        RemovedVault (Err reason) ->
            model
                |> notifyText ("Vault removal failed: " ++ (toString reason))

        DeleteVault vaultId ->
            model
                ! [ model.config
                        |> Daemon.deleteVault vaultId
                        |> attempt DeletedVault
                  ]

        DeletedVault (Ok vaultId) ->
            let
                newModel =
                    case model.state of
                        ShowingVaultDetails v ->
                            if v.id == vaultId then
                                { model | state = ShowingAllVaults }
                            else
                                model

                        _ ->
                            model
            in
                newModel
                    |> VaultDialog.Update.close vaultId
                    |> andAlso (notifyText ("Vault deleted from server: " ++ vaultId))

        DeletedVault (Err reason) ->
            model
                |> notifyText ("Vault deletion failed: " ++ (toString reason))

        VaultDialog vaultId msg ->
            model
                |> VaultDialog.Update.update msg vaultId

        OpenVaultFolder _ ->
            -- TODO: open folder in file browser
            model ! []

        OpenProgramSettings ->
            -- TODO
            model ! []

        OpenAccountSettings ->
            -- TODO
            model ! []

        RemoveVaultFromSync _ ->
            -- TODO
            model ! []

        Logout ->
            -- TODO
            model ! []

        FocusOn id ->
            model
                ! [ Ports.focusOn id ]

        NotificationCenter msg ->
            let
                ( state, cmd ) =
                    Ui.NotificationCenter.update msg model.notificationCenter
            in
                { model | notificationCenter = state }
                    ! [ Cmd.map NotificationCenter cmd ]


updateNow : Cmd Msg
updateNow =
    updateNowIn 0


updateNowIn : Time -> Cmd Msg
updateNowIn time =
    Util.performDelayed time SetDate Date.now


notify : Html Msg -> Model -> ( Model, Cmd Msg )
notify body model =
    let
        content =
            div [ class "MainScreen-NotificationCenter" ] [ body ]

        ( state, cmd ) =
            Ui.NotificationCenter.notify content model.notificationCenter
    in
        { model | notificationCenter = state }
            ! [ Cmd.map NotificationCenter cmd ]


notifyText : String -> Model -> ( Model, Cmd Msg )
notifyText message model =
    notify (text message) model


saveVault : Syncrypt.Vault.VaultId -> Model -> ( Model, Cmd Msg )
saveVault vaultId model =
    let
        ds =
            dialogState vaultId model
    in
        if ds.isNew then
            case ds.localFolderPath of
                Just folderPath ->
                    model
                        ! [ model.config
                                |> Daemon.createVault
                                    (Create
                                        { folder = folderPath
                                        , ignorePaths = Set.toList ds.ignoredFolderItems
                                        , userKeys = [] -- TODO
                                        }
                                    )
                                |> attempt CreatedVault
                          ]

                Nothing ->
                    model
                        |> notifyText "No path selected - Vault not created"
        else
            model
                |> notifyText "Vault updated"



-- VIEW


view : Model -> Html Msg
view model =
    let
        _ =
            Debug.log "state: " model.state
    in
        layout model
            []


currentClass : Model -> String
currentClass model =
    if model.sidebarOpen then
        "MainScreen-Container"
    else
        "MainScreen-Container MainScreen-Expanded"


layout : Model -> List (Html Msg) -> Html Msg
layout model nodes =
    div [ class "MainScreen" ]
        [ div [ class (currentClass model) ] <|
            [ header
            , div [ class "MainScreen-Container" ]
                (nodes ++ [ VaultList.view model ])
            , footer model
            , viewNotificationCenter model
            ]
                ++ VaultDialog.viewAll model
        ]


viewNotificationCenter : Model -> Html Msg
viewNotificationCenter { notificationCenter } =
    Ui.NotificationCenter.view NotificationCenter notificationCenter


header : Html Msg
header =
    div [ class "MainScreen-Header" ]
        [ div [ class "MainScreen-Buttons" ]
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
        div [ class "MainScreen-Footer" ]
            [ span [ class "MainScreen-Stats" ]
                [ text <|
                    (vaults |> List.length |> toString)
                        ++ " Vault(s) / "
                        ++ statsStr
                ]
            ]
