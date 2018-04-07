module MainScreen exposing (..)

import Animation exposing (Animation(..), animation)
import Config exposing (Config)
import Daemon
import DaemonLog
import Data.Daemon exposing (GUIConfig)
import Data.User exposing (Email)
import Data.Vault exposing (FlyingVault, Vault, VaultId, VaultOptions(..))
import Date
import FeedbackWizard
import Html exposing (Html, div, span, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Language
import LoginDialog.Model
import LoginDialog.Update
import LoginDialog.View
import Model exposing (..)
import Ports
import RemoteData exposing (RemoteData(..), WebData)
import Set
import SettingsDialog.Model as SettingsDialog
import SettingsDialog.Update
import SettingsDialog.View
import SetupWizard
import Time
import Translation exposing (NotificationText(..), Text(..), t, translate)
import Ui.NotificationCenter
import Util exposing ((~>), Direction(..), andLog)
import VaultDialog.Model exposing (CloneStatus(..))
import VaultDialog.Update exposing (dialogState)
import VaultDialog.View
import VaultList
import View.IconButton as IconButton exposing (IconButton(..))
import WizardDialog


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
            , Ports.updateEmailCompletionList ()
            , Daemon.getConfig model
            ]
    in
    ( model
    , Cmd.batch initialActions
    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.login of
        LoggedIn _ ->
            Sub.batch
                [ VaultDialog.View.subscriptions model
                , Time.every (10 * Time.minute) (\_ -> UpdateVaultsWithForcedRefresh)
                , Time.every Time.second (Date.fromTime >> SetTime)
                , Time.every model.config.updateInterval (\_ -> UpdateVaults)
                , Time.every model.config.updateInterval (\_ -> UpdateStats)
                , Ports.getEmailCompletionList EmailCompletionList
                , Ports.selectedUserKeyExportFile SelectedUserKeyExportFile
                , DaemonLog.subscriptions model
                , Ports.updateAvailable Model.UpdateAvailable
                ]

        _ ->
            Sub.batch
                [ Ports.updateAvailable Model.UpdateAvailable
                ]



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetTime time ->
            ( { model | now = Just time }
            , Cmd.none
            )

        UpdateLoginState ->
            model
                |> updateLoginState

        UpdateVaults ->
            model
                |> updateVaults
                ~> updateStats

        UpdateVaultsWithForcedRefresh ->
            model
                |> updateVaultsWithForcedRefresh
                ~> updateStats

        UpdateDaemonConfig ->
            ( model
            , Daemon.getConfig model
            )

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
                ~> VaultDialog.Update.close vaultId
                ~> updateVaults

        ClonedVault vaultId data ->
            model
                |> clonedVault vaultId data
                ~> updateVaults
                ~> updateStats

        CloseVaultDetails vaultId ->
            model
                |> closeVaultDetails vaultId

        SaveVaultDetails vaultId ->
            { model | state = ShowingAllVaults }
                |> VaultDialog.Update.close vaultId
                ~> saveVault vaultId
                ~> updateVaults

        DeleteVaultDialog vaultId ->
            model
                |> VaultDialog.Update.cancel vaultId

        Model.CreateNewVault ->
            { model | state = CreatingNewVault }
                |> VaultDialog.Update.openNew

        CreatedVault dialogState (Success vault) ->
            model
                |> VaultDialog.Update.saveVaultChanges vault.id dialogState
                ~> (notifyText <| VaultCreated vault.id)
                ~> delayedUpdateVaults 5000

        CreatedVault _ (Failure reason) ->
            model
                |> notifyText (VaultCreateFailed <| toString reason)
                ~> delayedUpdateVaults 100

        CreatedVault _ webData ->
            ( model
            , Cmd.none
            )
                |> andLog "CreatedVault unexpected data: " webData

        ExportedVault vaultId (Success { success, filename }) ->
            model
                |> notifyText (VaultExported vaultId)

        ExportedVault vaultId data ->
            model
                |> notifyText (VaultExportFailed (toString data))

        RemoveVaultFromSync vaultId ->
            model
                |> removeVaultFromSync vaultId

        RemovedVaultFromSync (Success vaultId) ->
            model
                |> VaultDialog.Update.cancel vaultId
                ~> updateVaults
                ~> (notifyText <| VaultRemoved vaultId)

        RemovedVaultFromSync data ->
            model
                |> updateVaults
                ~> notifyText (VaultRemoveFailed <| toString data)

        DeletedVault data ->
            model
                |> deletedVault data
                ~> updateVaults

        VaultDialogMsg vaultId msg ->
            model
                |> VaultDialog.Update.update msg vaultId

        OpenVaultFolder vault ->
            ( model
            , Ports.openVaultFolder vault.folderPath
            )

        OpenSettingsDialog ->
            -- TODO
            ( model
                |> SettingsDialog.open
            , Cmd.none
            )

        CloseSettingsDialog ->
            ( model
                |> SettingsDialog.close
            , Cmd.none
            )

        Login ->
            model
                |> login

        Model.Logout ->
            model
                |> logout

        Model.LoginDialogMsg msg ->
            model
                |> LoginDialog.Update.update msg

        LoginResult email data ->
            model
                |> handleLoginResult email data

        LogoutResult _ ->
            ( { model | login = LoggedOut }
            , Cmd.none
            )

        FocusOn id ->
            ( model
            , Ports.focusOn id
            )

        NotificationCenterMsg msg ->
            let
                ( state, cmd ) =
                    Ui.NotificationCenter.update msg model.notificationCenter
            in
            ( { model | notificationCenter = state }
            , Cmd.map NotificationCenterMsg cmd
            )

        UpdatedStatsFromApi stats ->
            ( { model | stats = stats }
            , Cmd.none
            )

        VaultUserAdded vaultId email data ->
            model
                |> vaultUserAdded vaultId email data

        VaultMetadataUpdated vaultId (Success _) ->
            model
                |> updateVaults

        VaultMetadataUpdated vaultId _ ->
            model
                |> notifyText (VaultMetadataUpdateFailed vaultId)

        WizardDialogMsg msg ->
            model
                |> WizardDialog.update msg

        SettingsDialogMsg msg ->
            model
                |> SettingsDialog.Update.update msg

        SetupWizardFinished ->
            ( { model | isFirstLaunch = False }
            , Daemon.invalidateFirstLaunch model
            )
                ~> notify (text "Syncrypt initialized")

        EmailCompletionList emails ->
            ( { model | emailCompletionList = emails }
            , Cmd.none
            )
                |> andLog "got email completion list: " emails

        Model.UpdateAvailable version ->
            ( { model | updateAvailable = Just version }
            , Cmd.none
            )
                |> andLog "update available: " version

        InstallUpdate ->
            ( model
            , Ports.quitAndInstall ()
            )

        UpdatedDaemonConfig (Success { gui }) ->
            if gui.isFirstLaunch && not model.languageSelected then
                { model
                    | isFirstLaunch = True
                    , language = Language.fromLocale model.config.locale
                }
                    |> openSetupWizard
            else
                ( updateGUIConfig gui model
                , Cmd.none
                )

        UpdatedDaemonConfig msg ->
            model
                |> Util.retryOnFailure msg UpdateDaemonConfig

        OpenFeedbackWizard ->
            model
                |> openFeedbackWizard

        SentFeedback (Success _) ->
            { model | feedback = Nothing }
                |> notify (text "Thanks for your feedback!")

        SentFeedback error ->
            model
                |> notify (text "Sending feedback failed. Please try again.")

        --  ~> (notify (text "Failed to send feedback. Retrying."))
        FeedbackEntered text ->
            case String.trim text of
                "" ->
                    ( { model | feedback = Nothing }
                    , Cmd.none
                    )

                trimmedText ->
                    ( { model | feedback = Just trimmedText }
                    , Cmd.none
                    )

        Model.SendFeedback ->
            model
                |> WizardDialog.close
                |> sendFeedback

        SetLanguage lang ->
            ( Model.selectLanguage lang model
            , Daemon.updateGUIConfig model
                { isFirstLaunch = model.isFirstLaunch
                , language = lang
                }
            )

        SendPasswordResetLink ->
            ( sentPasswordReset model
            , -- TODO:
              -- Daemon.sendPasswordResetLink
              Cmd.none
            )

        SetupWizardEmail email ->
            ( setSetupWizardEmail email model
            , Cmd.none
            )

        SetupWizardPassword password ->
            ( setSetupWizardPassword password model
            , Cmd.none
            )

        OpenUserKeyExportDialog ->
            ( model
            , Ports.openUserKeyExportFileDialog "Export Key to file"
            )

        SelectedUserKeyExportFile filePath ->
            ( model
            , Daemon.exportUserKey filePath model
            )
                |> andLog "SelectedUserKeyExportFile" filePath

        ExportedUserKey _ ->
            ( model
            , Cmd.none
            )

        OpenDaemonLogDialog ->
            model
                |> openDaemonLogDialog

        CloseDaemonLogDialog ->
            model
                |> closeDaemonLogDialog

        DaemonLogStream (Ok logItem) ->
            ( model
                |> Model.addDaemonLogItem logItem
            , Cmd.none
            )

        DaemonLogStream (Err reason) ->
            ( model
            , Cmd.none
            )
                |> andLog "DaemonLogStream Error" reason


setSetupWizardEmail : String -> Model -> Model
setSetupWizardEmail email ({ setupWizard, loginDialog } as model) =
    { model
        | setupWizard = { setupWizard | email = Just email }
    }
        |> LoginDialog.Model.setEmailInput email


setSetupWizardPassword : String -> Model -> Model
setSetupWizardPassword password ({ setupWizard } as model) =
    { model
        | setupWizard = { setupWizard | password = Just password }
    }
        |> LoginDialog.Model.setPasswordInput password


sentPasswordReset : Model -> Model
sentPasswordReset ({ setupWizard } as model) =
    { model
        | setupWizard = { setupWizard | passwordResetSent = True }
    }


sendFeedback : Model -> ( Model, Cmd Msg )
sendFeedback model =
    case model.feedback of
        Nothing ->
            ( model
            , Cmd.none
            )

        Just feedback ->
            ( model
            , Daemon.sendFeedback feedback model
            )


updateGUIConfig : GUIConfig -> Model -> Model
updateGUIConfig { isFirstLaunch, language } model =
    { model
        | isFirstLaunch = isFirstLaunch
        , language = language
    }


openSetupWizard : Model -> ( Model, Cmd Msg )
openSetupWizard model =
    let
        setupWizard =
            { email = Nothing
            , password = Nothing
            , passwordResetSent = False
            }
    in
    { model | setupWizard = setupWizard }
        |> WizardDialog.open (SetupWizard.settings model)


openFeedbackWizard : Model -> ( Model, Cmd Msg )
openFeedbackWizard model =
    model
        |> WizardDialog.open (FeedbackWizard.settings model)


openDaemonLogDialog : Model -> ( Model, Cmd Msg )
openDaemonLogDialog model =
    { model | state = ShowingDaemonLog }
        |> WizardDialog.open (DaemonLog.dialogSettings model)


closeDaemonLogDialog : Model -> ( Model, Cmd Msg )
closeDaemonLogDialog model =
    model
        |> WizardDialog.hideAndClose


updateLoginState : Model -> ( Model, Cmd Msg )
updateLoginState model =
    ( model
    , Daemon.getLoginState model
    )


updateVaults : Model -> ( Model, Cmd Msg )
updateVaults model =
    ( { model | state = UpdatingVaults }
    , Daemon.getVaults model
    )


updateVaultsWithForcedRefresh : Model -> ( Model, Cmd Msg )
updateVaultsWithForcedRefresh model =
    ( { model | state = UpdatingVaults }
    , Daemon.getVaultsWithForcedRefresh model
    )


delayedUpdateVaults : Time.Time -> Model -> ( Model, Cmd Msg )
delayedUpdateVaults delayMs model =
    ( model
    , Util.delayMsg delayMs UpdateVaults
    )


updateFlyingVaults : Model -> ( Model, Cmd Msg )
updateFlyingVaults model =
    ( { model | flyingVaults = Loading }
    , Daemon.getFlyingVaults model
    )


updateStats : Model -> ( Model, Cmd Msg )
updateStats model =
    ( model
    , Daemon.getStats model
    )


updatedLoginState : WebData LoginState -> Model -> ( Model, Cmd Msg )
updatedLoginState data model =
    case data of
        Success loginState ->
            ( { model | login = loginState }
            , Cmd.none
            )

        Failure _ ->
            { model | login = LoggedOut }
                |> Util.retryOnFailure data UpdateLoginState

        _ ->
            ( { model | login = Unknown }
            , Cmd.none
            )


updatedVaults : WebData (List Vault) -> Model -> ( Model, Cmd Msg )
updatedVaults vaults model =
    let
        newModel =
            { model | vaults = vaults }
    in
    case vaults of
        RemoteData.Failure error ->
            ( newModel
            , Util.delayMsg 1000 UpdateVaults
            )
                |> andLog "Failed to get vaults, retrying" error

        _ ->
            ( newModel
            , Cmd.none
            )


updatedFlyingVaults : WebData (List FlyingVault) -> Model -> ( Model, Cmd Msg )
updatedFlyingVaults flyingVaults model =
    { model
        | flyingVaults = flyingVaults
    }
        |> Util.retryOnFailure flyingVaults UpdateFlyingVaults


openVaultDetails : Vault -> Model -> ( Model, Cmd Msg )
openVaultDetails vault model =
    { model | state = ShowingVaultDetails vault }
        |> VaultDialog.Update.open


closeVaultDetails : VaultId -> Model -> ( Model, Cmd Msg )
closeVaultDetails vaultId model =
    { model | state = ShowingAllVaults }
        |> VaultDialog.Update.close vaultId
        ~> initiateDeleteVaultDialog vaultId


initiateDeleteVaultDialog : VaultId -> Model -> ( Model, Cmd Msg )
initiateDeleteVaultDialog vaultId model =
    ( model
    , Util.delayMsg 150 (DeleteVaultDialog vaultId)
    )


openFlyingVaultDetails : FlyingVault -> Model -> ( Model, Cmd Msg )
openFlyingVaultDetails flyingVault model =
    { model | state = ShowingFlyingVaultDetails flyingVault }
        |> VaultDialog.Update.open


deletedVault : WebData VaultId -> Model -> ( Model, Cmd Msg )
deletedVault data model =
    let
        nextModelWithState vaultId =
            case model.state of
                ShowingVaultDetails v ->
                    if v.id == vaultId then
                        { model | state = ShowingAllVaults }
                    else
                        model

                _ ->
                    model
    in
    case data of
        Success vaultId ->
            nextModelWithState vaultId
                |> VaultDialog.Update.cancel vaultId
                ~> (notifyText <| VaultDeleted vaultId)

        _ ->
            model
                |> notifyText (VaultDeleteFailed <| toString data)


notify : Html Msg -> Model -> ( Model, Cmd Msg )
notify body model =
    let
        content =
            div [ class "MainScreen-NotificationCenter" ] [ body ]

        ( state, cmd ) =
            Ui.NotificationCenter.notify content model.notificationCenter
    in
    ( { model | notificationCenter = state }
    , Cmd.map NotificationCenterMsg cmd
    )


notifyText : Translation.NotificationText -> Model -> ( Model, Cmd Msg )
notifyText notificationText model =
    model
        |> notify
            (text <|
                t (NotificationText notificationText) model
            )


saveVault : Data.Vault.VaultId -> Model -> ( Model, Cmd Msg )
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
                ~> (notifyText <| VaultUpdated vaultId)


createVault : VaultDialog.Model.State -> Model -> ( Model, Cmd Msg )
createVault state model =
    case state.localFolderPath of
        Just folderPath ->
            ( model
            , model
                |> Daemon.updateVault
                    (Create
                        { folder = folderPath
                        , ignorePaths = Set.toList state.ignoredFolderItems
                        }
                    )
                |> Cmd.map (CreatedVault state)
            )

        Nothing ->
            model
                |> notifyText NoPathSelected


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
                |> notifyText (CouldNotCloneVaultWithoutFolder vaultId)

        Just folderPath ->
            ( model
            , model
                |> Daemon.updateVault
                    (Clone
                        { id = vaultId
                        , folder = folderPath
                        , ignorePaths = Set.toList state.ignoredFolderItems
                        }
                    )
                |> Cmd.map (ClonedVault vaultId)
            )


clonedVault : VaultId -> WebData Vault -> Model -> ( Model, Cmd Msg )
clonedVault vaultId data model =
    case data of
        Success vault ->
            ( { model | state = ShowingAllVaults }
            , Daemon.getVaults model
            )
                ~> VaultDialog.Update.cancel vaultId

        Failure reason ->
            model
                |> notifyText (VaultCloneFailed vaultId <| toString reason)

        result ->
            model
                |> notifyText (VaultCloneFailed vaultId <| toString result)


vaultUserAdded : VaultId -> Email -> WebData Email -> Model -> ( Model, Cmd Msg )
vaultUserAdded vaultId email data model =
    case data of
        Success _ ->
            ( model
            , Cmd.none
            )

        _ ->
            model
                |> notifyText (VaultAddUserFailed vaultId email)


login : Model -> ( Model, Cmd Msg )
login model =
    let
        email =
            model.loginDialog.emailInput.value

        password =
            model.loginDialog.passwordInput.value
    in
    ( model
    , Daemon.login email password model
    )


logout : Model -> ( Model, Cmd Msg )
logout model =
    ( model
    , Daemon.logout model
    )


handleLoginResult :
    Email
    -> WebData StatusResponse
    -> Model
    -> ( Model, Cmd Msg )
handleLoginResult email data model =
    case data of
        Success _ ->
            ( { model
                | login =
                    LoggedIn { firstName = "", lastName = "", email = email }
              }
                |> LoginDialog.Model.loginSucceeded
            , Cmd.none
            )

        Failure reason ->
            ( { model | login = LoggedOut }
                |> LoginDialog.Model.loginFailed reason
            , Cmd.none
            )

        _ ->
            ( { model | login = Unknown }
            , Cmd.none
            )


removeVaultFromSync : VaultId -> Model -> ( Model, Cmd Msg )
removeVaultFromSync vaultId model =
    ( model
    , Daemon.removeVault vaultId model
    )



-- VIEW


view : Model -> Html Msg
view model =
    let
        content =
            case model.login of
                Unknown ->
                    loadingView model

                LoggedOut ->
                    loggedOutView model

                LoggedIn _ ->
                    loggedInView model
    in
    div [ class "MainScreen" ]
        [ content ]


loadingView : Model -> Html msg
loadingView model =
    div [ class "Loading" ]
        [ text "Loading ..." ]


loggedOutView : Model -> Html Msg
loggedOutView model =
    if model.isFirstLaunch then
        div [ class (currentClass model) ] <|
            [ viewNotificationCenter model
            , WizardDialog.view model
            ]
    else
        LoginDialog.View.view model


loggedInView : Model -> Html Msg
loggedInView model =
    div [ class (currentClass model) ] <|
        [ header model
        , div
            [ class "MainScreen-Container"
            , animation 1.0 FadeInFast
            ]
            [ VaultList.view model ]
        , footer model
        , viewNotificationCenter model
        , WizardDialog.view model
        , SettingsDialog.View.view model
        ]
            ++ VaultDialog.View.viewAll model


currentClass : Model -> String
currentClass model =
    if model.sidebarOpen then
        "MainScreen-Container"
    else
        "MainScreen-Container MainScreen-Expanded"


viewNotificationCenter : Model -> Html Msg
viewNotificationCenter { notificationCenter } =
    Ui.NotificationCenter.view NotificationCenterMsg notificationCenter


header : Model -> Html Msg
header model =
    div [ class "MainScreen-Header" ]
        [ div [ class "MainScreen-HeaderLogo" ]
            []
        , div [ class "MainScreen-Buttons" ] <|
            headerButtons model
        ]


headerButtons : Model -> List (Html Msg)
headerButtons { language, login } =
    case login of
        LoggedIn _ ->
            [ IconButton.view [ onClick UpdateVaults ]
                language
                RefreshVaultsButton
            , IconButton.view [ onClick OpenDaemonLogDialog ]
                language
                DaemonLogButton
            , IconButton.view [ onClick OpenFeedbackWizard ]
                language
                FeedbackButton
            , IconButton.view [ onClick OpenSettingsDialog ]
                language
                SettingsButton
            , IconButton.view [ onClick Model.Logout ]
                language
                LogoutButton
            ]

        _ ->
            []


footer : Model -> Html Msg
footer { stats, vaults, language, updateAvailable } =
    let
        statsText =
            Translation.StatsText <|
                case stats of
                    Success s ->
                        Translation.Stats s

                    Loading ->
                        Translation.StatsLoading

                    NotAsked ->
                        Translation.StatsNotAvailable

                    Failure reason ->
                        Translation.StatsFailedToLoad (toString reason)

        syncedVaultsText =
            case vaults of
                Success vaults ->
                    Translation.SyncedVaults (List.length vaults)

                Loading ->
                    Translation.VaultsLoading

                NotAsked ->
                    Translation.VaultsNotAvailable

                Failure reason ->
                    Translation.VaultsFailedToLoad (toString reason)

        updateAvailableDiv =
            case updateAvailable of
                Just version ->
                    [ span [ class "MainScreen-UpdateAvailable", onClick Model.InstallUpdate ] [ text <| translate (Translation.UpdateAvailable version) language ] ]

                Nothing ->
                    []
    in
    div [ class "MainScreen-Footer" ] <|
        [ span [ class "MainScreen-Stats" ]
            [ text <|
                translate syncedVaultsText language
                    ++ translate statsText language
            ]
        ]
            ++ updateAvailableDiv
