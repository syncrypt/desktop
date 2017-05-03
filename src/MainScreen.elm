module MainScreen exposing (..)

import Animation exposing (..)
import Config exposing (Config)
import Daemon exposing (attempt, attemptDelayed)
import Date exposing (Date)
import Html exposing (Html, div, node, span, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Model exposing (..)
import Ports
import RemoteData exposing (RemoteData(..), WebData)
import Set
import Syncrypt.Vault exposing (Vault, FlyingVault, VaultId, VaultOptions(..))
import Time exposing (Time)
import Ui.NotificationCenter
import Util exposing (Direction(..), andAlso)
import VaultDialog
import VaultDialog.Model exposing (CloneStatus(..))
import VaultDialog.Update exposing (dialogState)
import LoginDialog
import LoginDialog.Model
import VaultList
import Syncrypt.Vault exposing (VaultOptions(..))
import Set
import Ui.Input


-- INIT


init : Config -> ( Model, Cmd Msg )
init config =
    let
        model =
            Model.init config

        initialActions =
            [ updateNow
            , Daemon.getVaults model
            , Daemon.getFlyingVaults model
            , updateStats model
            , Daemon.getLoginState model.config
            ]
    in
        model ! initialActions



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ VaultDialog.subscriptions model
        , Time.every model.config.updateInterval (\t -> UpdateVaults)
        ]



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        SetDate date ->
            { model | now = Just date }
                ! [ updateNowIn 1000 ]

        GetLoginState ->
            model
                ! [ Daemon.getLoginState model.config ]

        UpdateVaults ->
            { model | state = UpdatingVaults }
                ! [ Daemon.getVaults model ]

        UpdateFlyingVaults ->
            model
                ! [ Daemon.getFlyingVaults model ]

        UpdateStats ->
            model
                ! [ updateStats model ]

        UpdatedLoginState loginState ->
            { model | login = loginState }
                |> Model.retryOnFailure loginState GetLoginState

        UpdatedVaultsFromApi vaults ->
            { model | vaults = vaults }
                ! []

        UpdatedFlyingVaultsFromApi vaults ->
            model
                |> fetchedFlyingVaults vaults

        OpenVaultDetails vault ->
            { model | state = ShowingVaultDetails vault }
                |> VaultDialog.Update.open

        OpenFlyingVaultDetails flyingVault ->
            { model | state = ShowingFlyingVaultDetails flyingVault }
                |> VaultDialog.Update.open

        CloneVault vaultId ->
            { model | state = CloningVault vaultId }
                |> VaultDialog.Update.close vaultId
                |> andAlso (cloneVault vaultId)

        ClonedVault vaultId (Success vault) ->
            { model | state = ShowingAllVaults }
                ! [ Daemon.getVaults model ]

        ClonedVault vaultId data ->
            let
                _ =
                    Debug.log "ClonedVault failed: " data
            in
                model
                    |> notifyText ("Something went wrong while cloning the vault " ++ vaultId ++ " : " ++ (toString data))

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

        CreatedVault dialogState (Success vault) ->
            model
                |> VaultDialog.Update.saveVaultChanges vault.id dialogState
                |> andAlso (notifyText <| "Vault created: " ++ vault.id)
                |> andAlso (\model -> ( model, Daemon.getVaults model ))

        CreatedVault _ (Failure reason) ->
            model
                |> notifyText ("Vault creation failed: " ++ toString reason)

        CreatedVault _ _ ->
            model
                ! []

        RemoveVaultFromSync vaultId ->
            model
                ! [ model.config
                        |> Daemon.removeVault vaultId
                        |> Cmd.map RemovedVaultFromSync
                  ]

        RemovedVaultFromSync (Success vaultId) ->
            model
                |> VaultDialog.Update.cancel vaultId
                |> andAlso (notifyText ("Vault removed from sync: " ++ vaultId))

        RemovedVaultFromSync data ->
            model
                |> notifyText ("Vault removal failed: " ++ (toString data))

        DeletedVault (Success vaultId) ->
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

        DeletedVault data ->
            model
                |> notifyText ("Vault deletion failed: " ++ (toString data))

        VaultDialog vaultId msg ->
            model
                |> VaultDialog.Update.update msg vaultId

        OpenVaultFolder vault ->
            -- TODO: open folder in file browser
            model ! [ Ports.openVaultFolder vault.folderPath ]

        OpenProgramSettings ->
            -- TODO
            model ! []

        OpenAccountSettings ->
            -- TODO
            model ! []

        Logout ->
            -- TODO
            model ! []

        Login ->
            let
                _ =
                    Debug.log "logging in with email" model.loginDialog.emailInput.value

                _ =
                    Debug.log "logging in with password" model.loginDialog.passwordInput.value
            in
                model ! []

        Model.LoginDialog loginMsg ->
            case loginMsg of
                LoginDialog.Model.EmailInput msg ->
                    let
                        ( emailInput, cmd ) =
                            Ui.Input.update msg model.loginDialog.emailInput

                        loginDialog =
                            model.loginDialog
                    in
                        ({ model | loginDialog = { loginDialog | emailInput = emailInput } })
                            ! []

                LoginDialog.Model.PasswordInput msg ->
                    let
                        ( passwordInput, cmd ) =
                            Ui.Input.update msg model.loginDialog.passwordInput

                        loginDialog =
                            model.loginDialog
                    in
                        ({ model | loginDialog = { loginDialog | passwordInput = passwordInput } })
                            ! []

                LoginDialog.Model.Modal _ ->
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

        UpdatedStatsFromApi stats ->
            { model | stats = stats }
                ! []

        VaultUserAdded vaultId email (Success _) ->
            model
                ! []

        VaultUserAdded vaultId email _ ->
            let
                _ =
                    Debug.log "Could not add user to vault: " ( vaultId, email )
            in
                model
                    |> notifyText ("Failed to add user " ++ email ++ " to vault " ++ vaultId)

        VaultMetadataUpdated vaultId (Success _) ->
            model
                ! [ Daemon.getVaults model ]

        VaultMetadataUpdated vaultId _ ->
            model
                |> notifyText ("Failed to update metadata for vault " ++ vaultId)


fetchedFlyingVaults : WebData (List FlyingVault) -> Model -> ( Model, Cmd Msg )
fetchedFlyingVaults flyingVaults model =
    let
        cmds =
            case flyingVaults of
                Failure reason ->
                    [ Daemon.getFlyingVaults model ]

                _ ->
                    []
    in
        { model | flyingVaults = flyingVaults }
            ! cmds


updateNow : Cmd Msg
updateNow =
    updateNowIn 0


updateNowIn : Time -> Cmd Msg
updateNowIn time =
    Util.performDelayed time SetDate Date.now


updateStats : Model -> Cmd Msg
updateStats model =
    model.config
        |> Daemon.getStats
        |> Cmd.map UpdatedStatsFromApi


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
        state =
            dialogState vaultId model
    in
        case state.cloneStatus of
            New ->
                model
                    |> createVault state

            _ ->
                model
                    |> VaultDialog.Update.saveVaultChanges vaultId state
                    |> andAlso (notifyText "Vault updated")


createVault state model =
    case state.localFolderPath of
        Just folderPath ->
            model
                ! [ model.config
                        |> Daemon.updateVault
                            (Create
                                { folder = folderPath
                                , ignorePaths = Set.toList state.ignoredFolderItems
                                }
                            )
                        |> Cmd.map (CreatedVault state)
                  ]

        Nothing ->
            model
                |> notifyText "No path selected - Vault not created"


cloneVault : VaultId -> Model -> ( Model, Cmd Msg )
cloneVault vaultId model =
    let
        state =
            dialogState vaultId model
    in
        case state.localFolderPath of
            Nothing ->
                model
                    |> notifyText "Could not clone vault - no folder specified"

            Just folderPath ->
                model
                    ! [ model.config
                            |> Daemon.updateVault
                                (Clone
                                    { id = vaultId
                                    , folder = folderPath
                                    , ignorePaths = Set.toList state.ignoredFolderItems
                                    }
                                )
                            |> Cmd.map (ClonedVault vaultId)
                      ]



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "MainScreen" ]
        [ div [ class (currentClass model), animations 2.5 [ SlideIn Top, FadeIn ] ] <|
            [ header
            , div [ class "MainScreen-Container" ]
                [ VaultList.view model ]
            , footer model
            , viewNotificationCenter model
            ]
                ++ VaultDialog.viewAll model
        ]


currentClass : Model -> String
currentClass model =
    if model.sidebarOpen then
        "MainScreen-Container"
    else
        "MainScreen-Container MainScreen-Expanded"


viewNotificationCenter : Model -> Html Msg
viewNotificationCenter { notificationCenter } =
    Ui.NotificationCenter.view NotificationCenter notificationCenter


header : Html Msg
header =
    div [ class "MainScreen-Header" ]
        [ div [ class "MainScreen-Buttons" ]
            [ node "IconButton"
                [ attribute
                    "data-for"
                    "header-tooltip"
                , attribute "data-tip" "Account Settings"
                , attribute "icon" "settings"
                , onClick OpenAccountSettings
                ]
                []
            , node "IconButton"
                [ attribute "data-for" "header-tooltip"
                , attribute "data-tip" "Logout"
                , attribute "icon" "logout"
                , onClick Logout
                ]
                []
            ]
        ]


footer : Model -> Html Msg
footer { stats, vaults } =
    let
        statsStr =
            case stats of
                Success s ->
                    (toString s.stats)
                        ++ " File Stats / "
                        ++ (toString s.downloads)
                        ++ " Downloads / "
                        ++ (toString s.uploads)
                        ++ " Uploads"

                Loading ->
                    "Stats loading ..."

                NotAsked ->
                    "Stats N/A"

                Failure reason ->
                    "Stats failed to load: " ++ (toString reason)

        syncedVaults =
            case vaults of
                Success vaults ->
                    if List.length vaults == 1 then
                        " 1 Synced Vault / "
                    else
                        (vaults |> List.length |> toString) ++ " Synced Vaults / "

                Loading ->
                    "..."

                NotAsked ->
                    "N/A"

                Failure reason ->
                    "Error: " ++ (toString reason)
    in
        div [ class "MainScreen-Footer" ]
            [ span [ class "MainScreen-Stats" ]
                [ text <| syncedVaults ++ statsStr ]
            ]
