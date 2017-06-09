module MainScreen exposing (..)

import Animation exposing (..)
import Config exposing (Config)
import Daemon exposing (attempt, attemptDelayed)
import Date exposing (Date)
import Html exposing (Html, div, node, span, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import LoginDialog
import LoginDialog.Model
import LoginDialog.Update
import Model exposing (..)
import Ports
import RemoteData exposing (RemoteData(..), WebData)
import Set
import Syncrypt.User exposing (Email)
import Syncrypt.Vault exposing (FlyingVault, Vault, VaultId, VaultOptions(..))
import Time exposing (Time)
import Ui.Input
import Ui.NotificationCenter
import Util exposing (Direction(..), andAlso)
import VaultDialog
import VaultDialog.Model exposing (CloneStatus(..))
import VaultDialog.Update exposing (dialogState)
import VaultList


-- INIT


init : Config -> ( Model, Cmd Msg )
init config =
    let
        model =
            Model.init config

        initialActions =
            [ Daemon.getLoginState model
            , Daemon.getVaults model
            , Daemon.getStats model
            ]
    in
        model ! initialActions



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.login of
        LoggedIn _ ->
            Sub.batch
                [ VaultDialog.subscriptions model
                , Time.every model.config.updateInterval (\t -> UpdateVaults)
                , Time.every Time.second SetTime
                ]

        _ ->
            Sub.none



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        SetTime time ->
            { model | now = Just time }
                ! []

        UpdateLoginState ->
            model
                |> updateLoginState

        UpdateVaults ->
            model
                |> updateVaults

        UpdateFlyingVaults ->
            model
                |> updateFlyingVaults

        UpdateStats ->
            model
                |> updateStats

        UpdatedLoginState data ->
            model
                |> updatedLoginState data

        UpdatedVaultsFromApi vaults ->
            model
                |> updatedVaults vaults

        UpdatedFlyingVaultsFromApi vaults ->
            model
                |> updatedFlyingVaults vaults

        OpenVaultDetails vault ->
            model
                |> openVaultDetails vault

        OpenFlyingVaultDetails flyingVault ->
            model
                |> openFlyingVaultDetails flyingVault

        CloneVault vaultId ->
            model
                |> cloneVault vaultId
                |> andAlso (VaultDialog.Update.close vaultId)

        ClonedVault vaultId data ->
            model
                |> clonedVault vaultId data

        CloseVaultDetails vaultId ->
            model
                |> closeVaultDetails vaultId

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
                |> removeVaultFromSync vaultId

        RemovedVaultFromSync (Success vaultId) ->
            model
                |> VaultDialog.Update.cancel vaultId
                |> andAlso (notifyText ("Vault removed from sync: " ++ vaultId))

        RemovedVaultFromSync data ->
            model
                |> notifyText ("Vault removal failed: " ++ (toString data))

        DeletedVault data ->
            model
                |> deletedVault data

        VaultDialog vaultId msg ->
            model
                |> VaultDialog.Update.update msg vaultId

        OpenVaultFolder vault ->
            model
                ! [ Ports.openVaultFolder vault.folderPath ]

        OpenProgramSettings ->
            -- TODO
            model
                ! []

        OpenAccountSettings ->
            -- TODO
            model
                ! []

        Login ->
            model
                |> login

        Logout ->
            model
                |> logout

        Model.LoginDialog msg ->
            model
                |> LoginDialog.Update.update msg

        LoginResult email data ->
            model
                |> handleLoginResult email data

        LogoutResult _ ->
            { model | login = LoggedOut }
                ! []

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

        VaultUserAdded vaultId email data ->
            model
                |> vaultUserAdded vaultId email data

        VaultMetadataUpdated vaultId (Success _) ->
            model
                |> updateVaults

        VaultMetadataUpdated vaultId _ ->
            model
                |> notifyText ("Failed to update metadata for vault " ++ vaultId)


updateLoginState : Model -> ( Model, Cmd Msg )
updateLoginState model =
    model
        ! [ Daemon.getLoginState model ]


updateVaults : Model -> ( Model, Cmd Msg )
updateVaults model =
    { model | state = UpdatingVaults }
        ! [ Daemon.getVaults model ]


updateFlyingVaults : Model -> ( Model, Cmd Msg )
updateFlyingVaults model =
    { model | flyingVaults = Loading }
        ! [ Daemon.getFlyingVaults model ]


updateStats : Model -> ( Model, Cmd Msg )
updateStats model =
    model
        ! [ Daemon.getStats model ]


updatedLoginState : WebData LoginState -> Model -> ( Model, Cmd Msg )
updatedLoginState data model =
    case data of
        Success loginState ->
            { model | login = loginState }
                ! []

        Failure _ ->
            { model | login = LoggedOut }
                |> Model.retryOnFailure data UpdateLoginState

        _ ->
            { model | login = Unknown }
                ! []


updatedVaults : WebData (List Vault) -> Model -> ( Model, Cmd Msg )
updatedVaults vaults model =
    { model | vaults = vaults }
        ! []


updatedFlyingVaults : WebData (List FlyingVault) -> Model -> ( Model, Cmd Msg )
updatedFlyingVaults flyingVaults model =
    { model | flyingVaults = flyingVaults }
        |> Model.retryOnFailure flyingVaults UpdateFlyingVaults


openVaultDetails : Vault -> Model -> ( Model, Cmd Msg )
openVaultDetails vault model =
    { model | state = ShowingVaultDetails vault }
        |> VaultDialog.Update.open


closeVaultDetails : VaultId -> Model -> ( Model, Cmd Msg )
closeVaultDetails vaultId model =
    { model | state = ShowingAllVaults }
        |> VaultDialog.Update.cancel vaultId


openFlyingVaultDetails : FlyingVault -> Model -> ( Model, Cmd Msg )
openFlyingVaultDetails flyingVault model =
    { model | state = ShowingFlyingVaultDetails flyingVault }
        |> VaultDialog.Update.open


deletedVault : WebData VaultId -> Model -> ( Model, Cmd Msg )
deletedVault data model =
    case data of
        Success vaultId ->
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

        _ ->
            model
                |> notifyText ("Vault deletion failed: " ++ (toString data))


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


createVault : VaultDialog.Model.State -> Model -> ( Model, Cmd Msg )
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
cloneVault vaultId origModel =
    let
        model =
            { origModel | state = CloningVault vaultId }

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


clonedVault vaultId data model =
    case data of
        Success vault ->
            { model | state = ShowingAllVaults }
                ! [ Daemon.getVaults model ]

        Failure reason ->
            model
                |> notifyText ("Something went wrong while cloning the vault " ++ vaultId ++ " : " ++ (toString reason))

        result ->
            model
                |> notifyText ("Unexpected result when cloning vault " ++ vaultId ++ " : " ++ (toString result))


vaultUserAdded : VaultId -> Email -> WebData Email -> Model -> ( Model, Cmd Msg )
vaultUserAdded vaultId email data model =
    case data of
        Success _ ->
            model
                ! []

        _ ->
            model
                |> notifyText ("Failed to add user " ++ email ++ " to vault " ++ vaultId)


login : Model -> ( Model, Cmd Msg )
login model =
    let
        email =
            model.loginDialog.emailInput.value

        password =
            model.loginDialog.passwordInput.value
    in
        model
            ! [ Daemon.login email password model ]


logout : Model -> ( Model, Cmd Msg )
logout model =
    model
        ! [ Daemon.logout model ]


handleLoginResult email data model =
    case data of
        Success _ ->
            { model | login = LoggedIn { firstName = "", lastName = "", email = email } }
                ! []

        Failure reason ->
            { model | login = LoggedOut }
                ! []

        _ ->
            { model | login = Unknown }
                ! []


removeVaultFromSync : VaultId -> Model -> ( Model, Cmd Msg )
removeVaultFromSync vaultId model =
    model
        ! [ Daemon.removeVault vaultId model ]



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "MainScreen" ] <|
        case model.login of
            Unknown ->
                [ text "..." ]

            LoggedOut ->
                [ LoginDialog.view model ]

            LoggedIn _ ->
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
