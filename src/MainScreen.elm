module MainScreen exposing (..)

import Animation exposing (Animation(..), LoadingCircleSize(..), animation, loadingCircle)
import Config exposing (Config)
import Daemon
import DaemonLog
import Data.Daemon exposing (GUIConfig)
import Data.User exposing (Email)
import Data.Vault exposing (FlyingVault, Vault, VaultId)
import Date
import FeedbackWizard
import Html exposing (Html, div, span, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Http
import Language
import LoginDialog.Model
import LoginDialog.Update
import LoginDialog.View
import Model exposing (..)
import NewVaultWizard
import Ports
import ReleaseNotesWizard
import RemoteData exposing (RemoteData(..), WebData)
import Set
import SettingsDialog.Model as SettingsDialog
import SettingsDialog.Update
import SettingsDialog.View
import SetupWizard
import Time
import Tooltip
import Translation as T
    exposing
        ( NotificationText(..)
        , Text(..)
        , VaultCreateFailReason(..)
        , t
        , translate
        )
import Tutorial
import Ui.NotificationCenter
import Util exposing ((~>), Direction(..), Position(..), TooltipLength(..), andLog, materialIcon, tooltipItem)
import VaultDialog.Model exposing (CloneStatus(..))
import VaultDialog.Update exposing (dialogState)
import VaultDialog.View
import VaultList
import View.IconButton as IconButton exposing (IconButton(..))
import Window
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
            , Daemon.getFlyingVaults model
            , Daemon.getStats model
            , Ports.updateEmailCompletionList ()
            , Daemon.getConfig model
            , Ports.updateAutoStartEnabledState ()
            ]
    in
    ( model
    , Cmd.batch initialActions
    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        commonSubs =
            [ Time.every Time.second (Date.fromTime >> SetTime)
            , Window.resizes WindowResized
            , Ports.updateAvailable Model.UpdateAvailable
            ]
    in
    case model.login of
        LoggedIn _ ->
            Sub.batch <|
                commonSubs
                    ++ [ VaultDialog.View.subscriptions model
                       , Time.every (10 * Time.minute) (\_ -> UpdateVaultsWithForcedRefresh)
                       , Time.every model.config.updateInterval (\_ -> UpdateVaults)
                       , Time.every (10 * Time.minute) (\_ -> UpdateFlyingVaults)
                       , Time.every model.config.updateInterval (\_ -> UpdateStats)
                       , Ports.getEmailCompletionList EmailCompletionList
                       , Ports.selectedUserKeyExportFile SelectedUserKeyExportFile
                       , DaemonLog.subscriptions model
                       , Ports.selectedVaultKeyImportFile SelectedVaultKeyImportFile
                       , Ports.selectedVaultImportFolder SelectedVaultImportFolder
                       , Ports.autoStartChanged AutoStartChanged
                       ]

        _ ->
            Sub.batch commonSubs



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetTime time ->
            ( { model | now = Just time }
            , Cmd.none
            )

        CopyToClipboard data ->
            ( model
            , Ports.copyToClipboard data
            )

        AddTooltip tip ->
            ( model |> Tooltip.add tip
            , Tooltip.removeInSchedule tip RemoveTooltip
            )

        RemoveTooltip tip ->
            ( model |> Tooltip.remove tip
            , Cmd.none
            )

        ShowTooltip tip ->
            ( model |> Tooltip.activate (Tooltip.id tip)
            , Cmd.none
            )

        ShowTooltipWithID tipId ->
            ( model |> Tooltip.activate tipId
            , Cmd.none
            )

        HideTooltip tip ->
            ( model |> Tooltip.deactivate (Tooltip.id tip)
            , Cmd.none
            )

        HideTooltipWithId tipId ->
            ( model |> Tooltip.deactivate tipId
            , Cmd.none
            )

        UpdateLoginState ->
            model
                |> updateLoginState

        UpdateVaults ->
            model
                |> updateVaults { forceRefresh = False }

        UpdateVaultsWithForcedRefresh ->
            model
                |> updateVaults { forceRefresh = True }
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
                |> VaultDialog.Update.close vaultId
                |> cloneVault vaultId
                ~> updateVaults { forceRefresh = False }

        ClonedVault vaultId data ->
            model
                |> clonedVault vaultId data
                ~> updateVaults { forceRefresh = True }
                ~> updateStats

        CloseVaultDetails vaultId ->
            model
                |> closeVaultDetails vaultId

        SaveVaultDetails vaultId ->
            { model | state = ShowingAllVaults }
                |> VaultDialog.Update.close vaultId
                |> saveVault vaultId
                ~> updateVaults { forceRefresh = False }

        DeleteVaultDialog vaultId ->
            ( model
                |> VaultDialog.Update.cancel vaultId
            , Cmd.none
            )

        Model.CreateNewVault ->
            { model | state = CreatingNewVault }
                |> VaultDialog.Update.openNew

        CreatedVault dialogState (Success vault) ->
            model
                |> VaultDialog.Update.saveVaultChanges vault.id dialogState
                ~> (notifyText <| VaultCreated vault.id)
                ~> delayedUpdateVaults { delay = 5000, forceRefresh = True }

        CreatedVault _ webData ->
            model
                |> createVaultFailed webData

        CreatedVaultInEmptyFolder folderPath (Success vault) ->
            model
                |> (notifyText <| VaultCreated vault.id)
                ~> delayedUpdateVaults { delay = 5000, forceRefresh = True }

        CreatedVaultInEmptyFolder folderPath webData ->
            model
                |> createVaultFailed webData

        ImportedVault (Success vault) ->
            model
                |> notifyText (VaultImported vault.id)
                ~> delayedUpdateVaults { delay = 100, forceRefresh = True }

        ImportedVault webData ->
            model
                |> notifyText VaultImportFailed
                ~> delayedUpdateVaults { delay = 100, forceRefresh = True }
                |> andLog "ImportedVault error: " webData

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
                |> updateVaults { forceRefresh = False }
                ~> (notifyText <| VaultRemoved vaultId)

        RemovedVaultFromSync data ->
            model
                |> updateVaults { forceRefresh = False }
                ~> notifyText VaultRemoveFailed

        DeletedVault data ->
            model
                |> deletedVault data
                ~> updateVaults { forceRefresh = False }

        VaultDialogMsg vaultId msg ->
            model
                |> VaultDialog.Update.update msg vaultId

        OpenVaultFolder vault ->
            ( model
            , Ports.openVaultFolder vault.folderPath
            )

        OpenSettingsDialog ->
            ( SettingsDialog.open model
            , Ports.updateAutoStartEnabledState ()
            )

        CloseSettingsDialog ->
            ( SettingsDialog.close model
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
                |> updateVaults { forceRefresh = False }

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
                ~> notifyText SyncryptInitialized

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
                |> notifyText ThanksForYourFeedback

        SentFeedback error ->
            model
                |> notifyText SendingFeedbackFailed

        FeedbackEntered text ->
            ( model
                |> setFeedback text
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

        WindowResized windowSize ->
            { model | windowSize = windowSize }
                ! []

        OpenDaemonLogDialog ->
            model
                |> openDaemonLogDialog

        CloseDaemonLogDialog ->
            model
                |> closeWizardWithState ShowingAllVaults

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

        OpenNewVaultWizard ->
            model
                |> openNewVaultWizard

        NewVaultWizardFinished ->
            model
                |> newVaultWizardFinished

        OpenVaultKeyImportFileDialog ->
            ( model
            , Ports.openVaultKeyImportFileDialog ()
            )

        SelectedVaultKeyImportFile filePath ->
            ( model
                |> selectedVaultKeyImportFile filePath
            , Cmd.none
            )

        OpenVaultImportFolderDialog ->
            ( model
            , Ports.openVaultImportFolderDialog ()
            )

        SelectedVaultImportFolder folderPath ->
            ( model
                |> selectedVaultImportFolder folderPath
            , Cmd.none
            )

        AutoStartChanged isEnabled ->
            ( { model | autoStartEnabled = isEnabled }
            , Cmd.none
            )

        ToggleAutoStart ->
            if model.autoStartEnabled then
                ( model
                , Ports.disableAutoStart ()
                )
            else
                ( model
                , Ports.enableAutoStart ()
                )

        UpdateAutoStartEnabledState ->
            ( model
            , Ports.updateAutoStartEnabledState ()
            )

        OpenReleaseNotesWizard ->
            let
                settings =
                    ReleaseNotesWizard.settings model
            in
            model
                |> openWizardWithState ShowingReleaseNotes settings

        CloseReleaseNotesWizard ->
            model
                |> closeWizardWithState ShowingAllVaults

        MainTutorialMsg msg ->
            let
                ( state, cmd ) =
                    Tutorial.update msg model.mainTutorial
            in
            ( { model | mainTutorial = state }
            , cmd
            )


createVaultFailed : WebData Vault -> Model -> ( Model, Cmd Msg )
createVaultFailed webData model =
    case webData of
        Failure reason ->
            let
                msg =
                    case reason of
                        Http.BadStatus s ->
                            VaultCreateFailed FolderAlreadyInSync

                        _ ->
                            VaultCreateFailed FolderPathNotValid
            in
            model
                |> notifyText msg
                ~> delayedUpdateVaults { delay = 100, forceRefresh = True }

        _ ->
            ( model
            , Cmd.none
            )
                |> andLog "CreatedVault unexpected data: " webData


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


openWizardWithState state dialogSettings model =
    { model | state = state }
        |> WizardDialog.open dialogSettings


closeWizard : Model -> ( Model, Cmd Msg )
closeWizard model =
    model
        |> WizardDialog.hideAndClose


closeWizardWithState : State -> Model -> ( Model, Cmd Msg )
closeWizardWithState state model =
    { model | state = ShowingAllVaults }
        |> WizardDialog.hideAndClose


updateLoginState : Model -> ( Model, Cmd Msg )
updateLoginState model =
    ( model
    , Daemon.getLoginState model
    )


updateVaults : { forceRefresh : Bool } -> Model -> ( Model, Cmd Msg )
updateVaults { forceRefresh } model =
    let
        fetchVaults =
            if forceRefresh then
                Daemon.getVaultsWithForcedRefresh
            else
                Daemon.getVaults
    in
    ( { model | state = UpdatingVaults }
    , fetchVaults model
    )


delayedUpdateVaults : { delay : Time.Time, forceRefresh : Bool } -> Model -> ( Model, Cmd Msg )
delayedUpdateVaults { delay, forceRefresh } model =
    let
        msg =
            if forceRefresh then
                UpdateVaultsWithForcedRefresh
            else
                UpdateVaults
    in
    ( model
    , Util.delayMsg delay msg
    )


updateFlyingVaults : Model -> ( Model, Cmd Msg )
updateFlyingVaults model =
    ( model
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
            model
                |> delayedUpdateVaults { delay = 1000, forceRefresh = False }
                |> andLog "Failed to get vaults, retrying" error

        _ ->
            ( newModel
            , Cmd.none
            )


updatedFlyingVaults : WebData (List FlyingVault) -> Model -> ( Model, Cmd Msg )
updatedFlyingVaults flyingVaults model =
    let
        newModel =
            case ( model.flyingVaults, flyingVaults ) of
                ( Success _, Success _ ) ->
                    -- TODO: check if we have any new vaults and alert on them
                    { model | flyingVaults = flyingVaults }

                ( _, Success _ ) ->
                    { model | flyingVaults = flyingVaults }

                ( Success _, Loading ) ->
                    model

                ( Failure _, _ ) ->
                    { model | flyingVaults = flyingVaults }

                ( NotAsked, _ ) ->
                    { model | flyingVaults = flyingVaults }

                _ ->
                    model
    in
    newModel
        |> Util.retryOnFailure flyingVaults UpdateFlyingVaults


openVaultDetails : Vault -> Model -> ( Model, Cmd Msg )
openVaultDetails vault model =
    { model | state = ShowingVaultDetails vault }
        |> VaultDialog.Update.open


closeVaultDetails : VaultId -> Model -> ( Model, Cmd Msg )
closeVaultDetails vaultId model =
    { model | state = ShowingAllVaults }
        |> VaultDialog.Update.close vaultId
        |> initiateVaultDialogDisposal vaultId


initiateVaultDialogDisposal : VaultId -> Model -> ( Model, Cmd Msg )
initiateVaultDialogDisposal vaultId model =
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

        closeVaultDialog vaultId =
            nextModelWithState vaultId
                |> VaultDialog.Update.cancel vaultId

        deleteFailed =
            case model.state of
                ShowingVaultDetails vault ->
                    closeVaultDialog vault.id

                _ ->
                    model
    in
    case data of
        Success vaultId ->
            closeVaultDialog vaultId
                |> notifyText (VaultDeleted vaultId)

        _ ->
            deleteFailed
                |> notifyText VaultDeleteFailed


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


notifyText : T.NotificationText -> Model -> ( Model, Cmd Msg )
notifyText notificationText model =
    model
        |> notify
            (text <|
                t (NotificationTxt notificationText) model
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
                    (Data.Vault.Create
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
                    (Data.Vault.Clone
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
                |> VaultDialog.Update.cancel vaultId
            , Daemon.getVaults model
            )

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
            ( model
                |> Model.login email
                |> LoginDialog.Model.loginSucceeded
            , Cmd.none
            )

        Failure reason ->
            ( model
                |> Model.logout
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


openNewVaultWizard : Model -> ( Model, Cmd Msg )
openNewVaultWizard model =
    { model | state = ImportingVaultKey }
        |> WizardDialog.open (NewVaultWizard.settings model)


newVaultWizardFinished : Model -> ( Model, Cmd Msg )
newVaultWizardFinished model =
    let
        cmd =
            case model.newVaultWizard of
                NoVaultImportStarted ->
                    Cmd.none
                        |> andLog "No import started after closing NewVaultWizard" model.newVaultWizard

                Model.ImportVault importState ->
                    case importState of
                        SelectedVaultKeyAndFolder keyPath folderPath ->
                            model
                                |> Daemon.importVault folderPath keyPath
                                |> Cmd.map ImportedVault

                        wizardState ->
                            Cmd.none
                                |> andLog "Invalid NewVaultWizard Import state" wizardState

                CreateNewVaultInPath folderPath ->
                    -- TODO: allow ignoring existing files if selected folder is not empty ?
                    model
                        |> Daemon.createNewVaultInFolder folderPath []
                        |> Cmd.map (CreatedVaultInEmptyFolder folderPath)
    in
    ( model
        |> resetVaultKeyImportState
    , cmd
    )



-- VIEW


view : Model -> Html Msg
view model =
    let
        content =
            case model.login of
                Unknown ->
                    loadingCircle LargeCircle model

                LoggedOut ->
                    loggedOutView model

                LoggedIn _ ->
                    loggedInView model
    in
    div [ class "MainScreen" ]
        [ content ]


loggedOutView : Model -> Html Msg
loggedOutView model =
    if model.isFirstLaunch then
        div [ class (currentClass model) ] <|
            [ viewNotificationCenter model
            , WizardDialog.view model
            ]
    else
        span []
            [ loadingCircle LargeCircle model
            , LoginDialog.View.view model
            ]


loggedInView : Model -> Html Msg
loggedInView model =
    div [ class (currentClass model) ] <|
        [ case model.stats of
            Success _ ->
                text ""

            _ ->
                span [ class "Transparent" ]
                    [ loadingCircle LargeCircle model ]
        , header model
        , div
            [ class "MainScreen-Container"
            , animation 1.0 FadeInFast
            ]
            [ Tutorial.view model.language model.mainTutorial
            , VaultList.view model
            ]
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
            [ IconButton.view [ onClick UpdateVaultsWithForcedRefresh ]
                language
                RefreshVaultsButton
            , IconButton.view [ onClick OpenNewVaultWizard ]
                language
                ImportVaultButton
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
footer ({ stats, vaults, language, updateAvailable } as model) =
    let
        statsText =
            T.StatsTxt <|
                case stats of
                    Success s ->
                        T.StatsLoaded s

                    Loading ->
                        T.StatsLoading

                    NotAsked ->
                        T.StatsNotAvailable

                    Failure reason ->
                        T.StatsFailedToLoad (toString reason)

        statsIcons =
            case stats of
                Success s ->
                    span []
                        [ text <| toString (s.busySlots + s.idleSlots)
                        , span [ class "OpenConnectionsIcon" ]
                            []
                        , text <| toString s.idleSlots
                        , span [ class "IdleConnectionsIcon" ]
                            []
                        , text <| toString s.stats
                        , materialIcon "autorenew" [ class "FileQueriesIcon" ]
                        , text <| toString s.downloads
                        , span [ class "DownloadsIcon" ]
                            []
                        , text <| toString s.uploads
                        , span [ class "UploadsIcon" ]
                            []
                        ]

                Failure reason ->
                    span []
                        [ materialIcon "error_outline" []
                        , text <| toString reason
                        ]

                _ ->
                    materialIcon "sync" []

        syncedVaultsText =
            case vaults of
                Success vaults ->
                    T.SyncedVaults (List.length vaults)

                Loading ->
                    T.VaultsLoading

                NotAsked ->
                    T.VaultsNotAvailable

                Failure reason ->
                    T.VaultsFailedToLoad (toString reason)

        syncedVaultsIcon =
            case vaults of
                Success vaults ->
                    span []
                        [ text <| toString <| List.length vaults
                        , materialIcon "folder_open" [ class "SyncedVaultsIcon" ]
                        ]

                Loading ->
                    materialIcon "autorenew" []

                NotAsked ->
                    materialIcon "autorenew" []

                Failure reason ->
                    materialIcon "sync_problem" [ class "VaultsFailedToLoadIcon" ]

        updateAvailableDiv =
            case updateAvailable of
                Just version ->
                    [ span
                        [ class "MainScreen-UpdateAvailable"
                        , onClick Model.InstallUpdate
                        ]
                        [ text <| translate language (T.UpdateAvailable version) ]
                    ]

                Nothing ->
                    []
    in
    div [ class "MainScreen-Footer" ] <|
        [ span [ class "MainScreen-Stats" ]
            [ Tooltip.viewIfActive statusBarTooltip
                model
                [ tooltipItem
                    { position = Top
                    , length = Auto
                    , text = translate language syncedVaultsText
                    , visible = False
                    }
                    [ syncedVaultsIcon ]
                ]
            , tooltipItem
                { position = Top
                , length = Fit
                , text = translate language statsText
                , visible = False
                }
                [ statsIcons ]
            ]
        ]
            ++ updateAvailableDiv
