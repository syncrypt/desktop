module Translation
    exposing
        ( ConfirmationDialogText(..)
        , FolderButtonType(..)
        , LoginDialogText(..)
        , MainTutorialText(..)
        , NewVaultWizardText(..)
        , NotificationText(..)
        , ReleaseNotesWizardText(..)
        , SettingsDialogText(..)
        , SetupWizardText(..)
        , StatsText(..)
        , Text(..)
        , VaultCreateFailReason(..)
        , VaultDialogText(..)
        , VaultListText(..)
        , t
        , timeAgo
        , translate
        )

import Data.Daemon exposing (KeyState, Stats)
import Data.User exposing (Email)
import Data.Vault exposing (VaultId)
import Date exposing (Date)
import Date.Distance
import Date.Distance.Types
import Http
import Language exposing (HasLanguage, Language(..))
import Util exposing (andLog)


type alias Reason =
    String


type alias Now =
    Date


type Text
    = LoginDialogTxt LoginDialogText
    | NotificationTxt NotificationText
    | StatsTxt StatsText
    | SyncedVaults Int
    | VaultsLoading
    | VaultsNotAvailable
    | VaultsFailedToLoad Reason
    | CreateNewVault
    | VaultNotSynced VaultId
    | Updated Date Now
    | LastUpdateToVault
    | LastUpdateToVaultLabel
    | NoFilesUploadedYet
    | UsersWithAccessTooltip
    | TotalVaultSizeTooltip
    | TotalVaultFilesTooltip
    | VaultDialogTxt VaultDialogText
    | VaultListTxt VaultListText
    | Previous
    | Next
    | Cancel
    | OK
    | CopiedToClipboard
    | Finish
    | Close
    | Confirm
    | YourFeedback
    | SendUsYourFeedbackSuggestionsOrBugReports
    | WeWillReplyToYouViaEmail
    | TypeYourFeedbackHere
    | SendFeedback
    | ConfirmationDialogTxt ConfirmationDialogText
    | Logout
    | SoftwareAndAccountSettings
    | SendUsFeedbackAndBugReports
    | ViewDaemonLog
    | ImportVault
    | ProgramSettings
    | SettingsDialogTxt SettingsDialogText
    | UpdateAvailable String
    | SetupWizardTxt SetupWizardText
    | NewVaultWizardTxt NewVaultWizardText
    | ReleaseNotesWizardTxt ReleaseNotesWizardText
    | FinishTutorial
    | SkipTutorial
    | MainTutorialTxt MainTutorialText


type LoginDialogText
    = LoginEmail
    | LoginPassword
    | DaemonConnectionTimedOut
    | NetworkError
    | BadStatusOrLoginFailed (Http.Response String)
    | UnknownError Http.Error


type NotificationText
    = VaultCreated VaultId
    | VaultImported VaultId
    | VaultRemoved VaultId
    | VaultDeleted VaultId
    | VaultUpdated VaultId
    | VaultCreateFailed VaultCreateFailReason
    | VaultImportFailed
    | VaultRemoveFailed
    | VaultDeleteFailed
    | VaultIsResyncing
    | VaultMetadataUpdateFailed VaultId
    | VaultCloneFailed VaultId Reason
    | VaultAddUserFailed VaultId Email
    | VaultExported VaultId
    | VaultExportFailed Reason
    | CouldNotCloneVaultWithoutFolder VaultId
    | NoPathSelected
    | SyncryptInitialized
    | SendingFeedbackFailed
    | ThanksForYourFeedback


type VaultCreateFailReason
    = FolderAlreadyInSync
    | FolderPathNotValid


type StatsText
    = StatsLoaded Data.Daemon.Stats
    | StatsLoading
    | StatsNotAvailable
    | StatsFailedToLoad Reason


type VaultDialogText
    = NameAndFilesTab
    | UsersTab
    | UsersTabInfoText Bool -- owns vault?
    | CryptoTab
    | LogTab
    | AdminTab
    | AdminTabInfoText
    | VaultNameLabel
    | VaultNameTooltip
    | FolderLabel
    | FolderButtonLabel FolderButtonType
    | FolderButtonTooltip FolderButtonType
    | FilesLabel
    | FileSelectionTooltip
    | UserInputLabel
    | UserInputTooltip
    | VaultUsersLabel
    | CryptoTabInfoText
    | VaultIdLabel
    | VaultIdTooltip
    | FileRevisionsLabel
    | TotalNumberOfFileRevisionsTooltip
    | LastModifiedLabel
    | LastModifiedTooltip
    | NoChangesSoFar
    | KeyAlgorithmLabel
    | KeyAlgorithmTooltip
    | KeyFingerprintLabel
    | KeyFingerprintTooltip
    | TransferAlgorithmLabel
    | TransferAlgorithmTooltip
    | HashAlgorithmLabel
    | HashAlgorithmTooltip
    | AESKeyLengthLabel
    | AESKeyLengthTooltip
    | RSAKeyLengthLabel
    | RSAKeyLengthTooltip
    | VaultRemoveButtonInfo
    | VaultResyncButtonInfo
    | VaultDeleteButtonInfo
    | VaultExportButtonInfo
    | AskDeleteVault
    | AskDeleteVaultExtended
    | ExportToFile
    | CancelChanges
    | DeleteFromServer
    | StopSyncing
    | Resync
    | CreateVault
    | SaveVault
    | SyncVaultToFolder
    | YouDontHaveAccessToVaultUsers
    | Filters
    | Time
    | User
    | UserTooltip
    | Operation
    | OperationTooltip
    | FilePathOrMessage
    | DebugFilter
    | InfoFilter
    | WarningFilter
    | ErrorFilter
    | HistoryFilter
    | LogFilter
    | LogLevels
    | InviteWithSelectedKeys
    | Folder
    | Name
    | Invited Date Now
    | InvitedAt Date
    | Created Date Now
    | VaultOwner
    | AllEventsHaveBeenFiltered
    | HistoryItemDescription Data.Vault.HistoryItem
    | ExportVaultKeyBundle


type VaultListText
    = HeaderDescription
    | LoadVaultsFailed Reason
    | LoadingVaults
    | LoadRemoteVaults
    | Vaults
    | AvailableVaults
    | FetchingRemoteVaultInfo
    | YouDontHaveAnyRemoteVaultsYet
    | YouHaveClonedAllAvailableVaults
    | ClickOnVaultToClone
    | FetchingRemoteVaultsFailed Reason


type ConfirmationDialogText
    = RemoveVaultFromSyncQuestion
    | RemoveVaultFromSyncExplanation


type SettingsDialogText
    = ChooseYourLanguage
    | AccountOptions
    | ChangePassword
    | ConfirmChangePassword
    | ResetPassword
    | OldPasswordLabel
    | OldPasswordTooltip
    | NewPasswordLabel
    | NewPasswordTooltip
    | NewPasswordConfirmationLabel
    | NewPasswordConfirmationTooltip
    | UpdateIsAvailable
    | KeyExportOrImport
    | AutoStart
    | AutoStartEnabled
    | AutoStartDisabled
    | AboutSyncryptDesktop
    | Version
    | DaemonAuthToken
    | YouNeedToEnterYourCurrentPassword
    | YouNeedToEnterANewPassword
    | PasswordConfirmationDoesNotMatch


type SetupWizardText
    = WelcomeToSyncrypt
    | WelcomeHeader1
    | WelcomeHeader2
    | AccountSetup
    | DoYouAlreadyHaveASyncryptAccount
    | YouCanLoginWithAnExistingAccountOrCreateANewOne
    | LoginWithAccount
    | SignUpWithNewAccount
    | AccountLogin
    | LoginWithYourAccount
    | IfYouForgotYourPassword
    | WeWillSendYouAPasswordResetLink
    | YourEmail
    | Password
    | ForgotPassword
    | PasswordResetLinkHasBeenSent
    | Login
    | AccountSignup
    | LegalNotice
    | PleaseReadAndConfirm
    | TOS1
    | TOS2
    | TOS3
    | TOS4
    | TOS5
    | TOS6
    | TOS7
    | TOS8
    | TOS9
    | TOS10
    | TOS11
    | IAgree
    | CreateYourNewAccount
    | Signup
    | KeyCreation
    | KeyImport
    | KeyNotYetInitialized
    | InitializingKey
    | KeySuccessfullyInitialized
    | Updating


type NewVaultWizardText
    = VaultKeyImport
    | ImportYourVaultKeyHere
    | SelectVaultKeyFileForImport
    | SelectYourVaultImportDestinationFolder


type ReleaseNotesWizardText
    = ReleaseNotesTitle


type MainTutorialText
    = MainTutorialS1T
    | MainTutorialS1P1
    | MainTutorialS2T
    | MainTutorialS2TT1
    | MainTutorialS2P1
    | MainTutorialS2P2
    | MainTutorialS2P3
    | MainTutorialS3T
    | MainTutorialS3TT1
    | MainTutorialS3P1
    | MainTutorialS3P2
    | MainTutorialS4T
    | MainTutorialS4TT1
    | MainTutorialS4P1
    | MainTutorialS4P2
    | MainTutorialS5T
    | MainTutorialS5TT1
    | MainTutorialS5P1
    | MainTutorialS5P2
    | MainTutorialS6T
    | MainTutorialS6TT1
    | MainTutorialS6P1
    | MainTutorialS6P2
    | MainTutorialS7T
    | MainTutorialS7TT1
    | MainTutorialS7P1
    | MainTutorialS7P2
    | MainTutorialS8T
    | MainTutorialS8TT1
    | MainTutorialS8P1


type FolderButtonType
    = SelectFolder
    | CloneIntoFolder
    | FolderSelectedForSync String
    | SyncedFolder String


{-| Translates a `Text` into a `String` based on `language`.

    t (VaultCreated "123") {language = English} -- -> "Vault created: 123"
    t (VaultCreated "123") {language = German}  -- -> "Vault wurde erstellt: 123"

-}
t : Text -> HasLanguage a -> String
t text { language } =
    translate language text


{-| Translates a `Text` into a `String` based on `lang`.

    translate (VaultCreated "123") English -- -> "Vault created: 123"
    translate (VaultCreated "123") German  -- -> "Vault wurde erstellt: 123"

-}
translate : Language -> Text -> String
translate lang text =
    case lang of
        English ->
            translateEnglish text

        German ->
            translateGerman text


translateEnglish : Text -> String
translateEnglish text =
    case text of
        LoginDialogTxt t ->
            translateEnglishLoginDialogText t

        NotificationTxt t ->
            translateEnglishNotificationText t

        StatsTxt st ->
            translateEnglishStatsText st

        SyncedVaults vaultCount ->
            if vaultCount == 1 then
                "1 synchronized vault"
            else
                toString vaultCount ++ " synchronized vaults"

        VaultsLoading ->
            "..."

        VaultsNotAvailable ->
            "N/A"

        VaultsFailedToLoad reason ->
            "Error: " ++ reason

        CreateNewVault ->
            "Create New Vault"

        VaultNotSynced vaultId ->
            "Vault (not synchronized) " ++ vaultId

        Updated date now ->
            "Last update " ++ Date.Distance.inWords date now ++ " ago"

        LastUpdateToVault ->
            "Last update"

        LastUpdateToVaultLabel ->
            "Last update to vault"

        NoFilesUploadedYet ->
            "No files uploaded yet"

        UsersWithAccessTooltip ->
            "Users with access"

        TotalVaultSizeTooltip ->
            "Total vault size (with all file revisions on server)"

        TotalVaultFilesTooltip ->
            "Total number of files"

        VaultDialogTxt vt ->
            translateEnglishVaultDialogText vt

        VaultListTxt vlt ->
            translateEnglishVaultListText vlt

        Previous ->
            "Previous"

        Next ->
            "Next"

        Cancel ->
            "Cancel"

        OK ->
            "OK"

        CopiedToClipboard ->
            "Copied"

        Finish ->
            "Finish"

        Close ->
            "Close"

        Confirm ->
            "Confirm"

        YourFeedback ->
            "Your Feedback"

        SendUsYourFeedbackSuggestionsOrBugReports ->
            "Send us your feedback, suggestions or bug report"

        WeWillReplyToYouViaEmail ->
            "We will reply to you via email"

        TypeYourFeedbackHere ->
            "Type your feedback here"

        SendFeedback ->
            "Send feedback"

        ConfirmationDialogTxt cdt ->
            translateEnglishConfirmationDialogText cdt

        Logout ->
            "Logout"

        SoftwareAndAccountSettings ->
            "Software & Account Settings"

        SendUsFeedbackAndBugReports ->
            "Send us feedback & bug reports"

        ViewDaemonLog ->
            "View the Syncrypt background process log"

        ImportVault ->
            "Import a previously exported Vault & key"

        ProgramSettings ->
            "Program Settings"

        SettingsDialogTxt text ->
            translateEnglishSettingsDialogText text

        UpdateAvailable version ->
            "Update available, click here to install version " ++ version

        SetupWizardTxt text ->
            translateEnglishSetupWizardText text

        NewVaultWizardTxt text ->
            translateEnglishNewVaultWizardText text

        ReleaseNotesWizardTxt text ->
            translateEnglishReleaseNotesWizardText text

        FinishTutorial ->
            "Finish Tutorial"

        SkipTutorial ->
            "Skip Tutorial"

        MainTutorialTxt text ->
            translateEnglishMainTutorialText text


translateEnglishLoginDialogText : LoginDialogText -> String
translateEnglishLoginDialogText t =
    case t of
        LoginEmail ->
            "Email"

        LoginPassword ->
            "Password"

        DaemonConnectionTimedOut ->
            "Daemon connection timed out."

        NetworkError ->
            "Network error."

        BadStatusOrLoginFailed resp ->
            "Login failed: " ++ toString resp

        UnknownError httpError ->
            "Unknown error while talking to Syncrypt background process."
                |> andLog "Unexpected login error: " httpError


translateEnglishNotificationText : NotificationText -> String
translateEnglishNotificationText t =
    case t of
        VaultCreated vaultId ->
            "Vault created"

        VaultImported vaultId ->
            "Vault imported"

        VaultDeleted vaultId ->
            "Vault deleted from server"

        VaultUpdated vaultId ->
            "Vault updated"

        VaultRemoved vaultId ->
            "Vault removed from sync"

        VaultCreateFailed reason ->
            case reason of
                FolderAlreadyInSync ->
                    "Failed to create vault. Folder is already in sync in another vault."

                FolderPathNotValid ->
                    "Failed to create vault. Folder path is not a valid path."

        VaultIsResyncing ->
            "Vault is re-syncing."

        VaultRemoveFailed ->
            "Failed to remove vault. Please try again."

        VaultDeleteFailed ->
            "Vault deletion failed. Please try again."

        VaultImportFailed ->
            "Vault import failed. Please try again."

        VaultMetadataUpdateFailed vaultId ->
            "Failed to update metadata for vault"

        VaultCloneFailed vaultId reason ->
            "Something went wrong while cloning the vault : "
                ++ reason

        VaultAddUserFailed vaultId email ->
            "Failed to add user " ++ email ++ " to vault "

        VaultExported vaultId ->
            "Vault exported"

        VaultExportFailed _ ->
            "Vault export failed"

        CouldNotCloneVaultWithoutFolder vaultId ->
            "Could not clone vault - no folder specified"

        NoPathSelected ->
            "No path selected - vault not created"

        SyncryptInitialized ->
            "Syncrypt initialized"

        SendingFeedbackFailed ->
            "Sending feedback failed. Please try again."

        ThanksForYourFeedback ->
            "Thanks for your feedback!"


translateEnglishStatsText : StatsText -> String
translateEnglishStatsText st =
    case st of
        StatsLoaded s ->
            toString (s.busySlots + s.idleSlots)
                ++ " open connections ("
                ++ toString s.idleSlots
                ++ " idle) / "
                ++ toString s.downloads
                ++ " downloads / "
                ++ toString s.uploads
                ++ " uploads"

        StatsLoading ->
            "Stats loading ..."

        StatsNotAvailable ->
            "Stats N/A"

        StatsFailedToLoad reason ->
            "Stats failed to load: " ++ reason


translateEnglishVaultDialogText : VaultDialogText -> String
translateEnglishVaultDialogText vt =
    case vt of
        NameAndFilesTab ->
            "Files"

        UsersTab ->
            "Users"

        UsersTabInfoText ownsVault ->
            if ownsVault then
                "Add users to this vault to securely share access to files and collaborate on folders and files with as many people as you like."
            else
                "These users have access to this vault (including you). Anyone with access can add, edit and read files in this vault."

        CryptoTab ->
            "Metadata"

        LogTab ->
            "Log"

        AdminTab ->
            "Admin"

        AdminTabInfoText ->
            "Administrative options for this vault"

        VaultNameLabel ->
            "Name"

        VaultNameTooltip ->
            "The name of the vault. Chosen by the owner."

        FolderLabel ->
            "Folder"

        FolderButtonLabel t ->
            case t of
                SelectFolder ->
                    "Select Folder"

                CloneIntoFolder ->
                    "Select clone destination path"

                FolderSelectedForSync path ->
                    path

                SyncedFolder path ->
                    path

        FolderButtonTooltip t ->
            case t of
                SelectFolder ->
                    "Select a new folder for this vault."

                CloneIntoFolder ->
                    "By clicking here, you select a folder to use for this vault to download its files to."

                FolderSelectedForSync path ->
                    "This new vault will synchronize files in this folder: " ++ path

                SyncedFolder path ->
                    "This vault is synchronizing files from and to this folder"

        FilesLabel ->
            "Files"

        FileSelectionTooltip ->
            "This shows all local files in your vault. Toggle individual files or whole subdirectories from automated synchronization if you don't want all files to be uploaded & synchronized automatically."

        UserInputLabel ->
            "Invite"

        UserInputTooltip ->
            "Search for a user's email address to add them to this vault."

        VaultUsersLabel ->
            "Current Members"

        CryptoTabInfoText ->
            "Here you can see detailed information on this vault's cryptographic settings, used algorithms and keys."

        VaultIdLabel ->
            "Vault ID"

        VaultIdTooltip ->
            "Syncrypt Vault ID"

        FileRevisionsLabel ->
            "File Revisions"

        TotalNumberOfFileRevisionsTooltip ->
            "Total number of file revisions in this vault."

        LastModifiedLabel ->
            "Last modified"

        LastModifiedTooltip ->
            "Date & time of last update to this vault."

        NoChangesSoFar ->
            "No changes so far."

        KeyAlgorithmLabel ->
            "Key Algorithm"

        KeyAlgorithmTooltip ->
            "Asymmetric key algorithm used for vault key"

        KeyFingerprintLabel ->
            "Vault Key Fingerprint"

        KeyFingerprintTooltip ->
            "Vault public key fingerprint"

        TransferAlgorithmLabel ->
            "Transfer Algorithm"

        TransferAlgorithmTooltip ->
            "Algorithm used for encrypting data transfer"

        HashAlgorithmLabel ->
            "Hash Algorithm"

        HashAlgorithmTooltip ->
            "Algorithm used for hashing file contents & names"

        AESKeyLengthLabel ->
            "AES Key Length"

        AESKeyLengthTooltip ->
            "Length of symmetric file encryption keys in this vault"

        RSAKeyLengthLabel ->
            "RSA Key Length"

        RSAKeyLengthTooltip ->
            "Length of vault private key"

        VaultRemoveButtonInfo ->
            "Stops synchronizing this vault on this device. Will stop all local changes from being uploaded and any remote changes being downloaded to this machine."

        VaultResyncButtonInfo ->
            "Re-sync this vault and validate the sign chain from the beginning."

        VaultDeleteButtonInfo ->
            "Delete this vault with its files from the Syncrypt cloud."

        VaultExportButtonInfo ->
            "You can export your vault here to backup the vault's configuration, private metadata and encryption key. This allows you to re-download the vault in case of a disk failure or theft of the computer you're currently uploading files to this vault from."

        AskDeleteVault ->
            "Delete vault?"

        AskDeleteVaultExtended ->
            "Do you really want to delete this vault from the server? Deleting the vault on the server will also delete the vault keys and metadata, but it won't delete any of your local files inside this vault."

        ExportToFile ->
            "Export to file"

        CancelChanges ->
            "Cancel changes"

        DeleteFromServer ->
            "Delete from server"

        StopSyncing ->
            "Stop syncing"

        Resync ->
            "Re-sync"

        CreateVault ->
            "Create vault"

        SaveVault ->
            "Save Vault & confirm changes"

        SyncVaultToFolder ->
            "Sync vault to folder"

        YouDontHaveAccessToVaultUsers ->
            "You don't have access to this vault's user list."

        Filters ->
            "Filters"

        Time ->
            "Time"

        User ->
            "User"

        UserTooltip ->
            "Hover over a user to see the fingerprint that was used for each revision"

        Operation ->
            "Operation"

        OperationTooltip ->
            "Hover over the operation to see the revision ID"

        FilePathOrMessage ->
            "Path / Message"

        DebugFilter ->
            "Debug"

        InfoFilter ->
            "Info"

        WarningFilter ->
            "Warning"

        ErrorFilter ->
            "Error"

        HistoryFilter ->
            "History"

        LogFilter ->
            "Log"

        LogLevels ->
            "Log Levels"

        InviteWithSelectedKeys ->
            "Invite with selected keys"

        Folder ->
            "Folder"

        Name ->
            "Name"

        Invited date now ->
            "Invited " ++ Date.Distance.inWords date now ++ " ago"

        InvitedAt date ->
            "Invited " ++ toString date

        Created date now ->
            "Created " ++ Date.Distance.inWords date now ++ " ago"

        VaultOwner ->
            "Vault Owner"

        AllEventsHaveBeenFiltered ->
            "All events have been filtered."

        HistoryItemDescription { email, operation, fingerprint, path } ->
            let
                fpath =
                    Maybe.withDefault "N/A" path
            in
            case operation of
                Data.Vault.CreateVault ->
                    "Vault created"

                Data.Vault.SetMetadata ->
                    "Vault metadata updated"

                Data.Vault.AddUser ->
                    "User " ++ email ++ " added to vault"

                Data.Vault.RemoveUser ->
                    "User " ++ email ++ " removed from vault"

                Data.Vault.AddUserKey ->
                    "User key for user " ++ email ++ " added: " ++ fingerprint

                Data.Vault.RemoveUserKey ->
                    "User key for user " ++ email ++ " removed: " ++ fingerprint

                Data.Vault.AddFile ->
                    "Uploaded: " ++ fpath

                Data.Vault.DeleteFileRevision ->
                    "Deleted (with all revisions): " ++ fpath

                Data.Vault.RemoveFile ->
                    "Deleted (with all revisions): " ++ fpath

                Data.Vault.RenameFile ->
                    "Renamed: " ++ fpath

                Data.Vault.RestoreFile ->
                    "Restored revision for: " ++ fpath

        ExportVaultKeyBundle ->
            "Export vault key & configuration bundle"


translateEnglishVaultListText : VaultListText -> String
translateEnglishVaultListText vlt =
    case vlt of
        HeaderDescription ->
            "These vaults are cloned and synchronized on this computer"

        LoadVaultsFailed reason ->
            "Failed to load vaults: " ++ reason

        LoadingVaults ->
            "Loading vaults"

        LoadRemoteVaults ->
            "Load remote vaults"

        Vaults ->
            "Vaults"

        AvailableVaults ->
            "Available Cloud Vaults"

        FetchingRemoteVaultInfo ->
            "Fetching remote vault info "

        YouDontHaveAnyRemoteVaultsYet ->
            "You don't have any remote vaults in the cloud yet. Create a new vault by clicking the plus button above."

        YouHaveClonedAllAvailableVaults ->
            "You have already cloned all available vaults to this computer."

        ClickOnVaultToClone ->
            "Click on a vault to clone it to your computer"

        FetchingRemoteVaultsFailed reason ->
            "Error fetching remote vaults: " ++ reason


translateEnglishConfirmationDialogText : ConfirmationDialogText -> String
translateEnglishConfirmationDialogText cdt =
    case cdt of
        RemoveVaultFromSyncQuestion ->
            "Remove vault from sync?"

        RemoveVaultFromSyncExplanation ->
            "This vault will stop being synchronized to this device. Any local file changes won't be uploaded and new files added to the vault won't be downloaded to this machine."


translateEnglishSettingsDialogText : SettingsDialogText -> String
translateEnglishSettingsDialogText text =
    case text of
        ChooseYourLanguage ->
            "Choose your language"

        AccountOptions ->
            "Account Options"

        ChangePassword ->
            "Change Password"

        ConfirmChangePassword ->
            "Confirm password change"

        ResetPassword ->
            "Reset Password"

        OldPasswordLabel ->
            "Current Password"

        OldPasswordTooltip ->
            "Your current password that you want to expire and update to a new one"

        NewPasswordLabel ->
            "New Password"

        NewPasswordTooltip ->
            "This will be your new password once you hit save"

        NewPasswordConfirmationLabel ->
            "Confirm New Password"

        NewPasswordConfirmationTooltip ->
            "This is to make sure you're not mispelling your new password"

        UpdateIsAvailable ->
            "Update available"

        KeyExportOrImport ->
            "Key export / import"

        AutoStart ->
            "Autostart at system boot"

        AutoStartEnabled ->
            "Autostart enabled"

        AutoStartDisabled ->
            "Autostart disabled"

        AboutSyncryptDesktop ->
            "About Syncrypt Desktop"

        Version ->
            "Version"

        DaemonAuthToken ->
            "Daemon auth token"

        YouNeedToEnterYourCurrentPassword ->
            "You need to enter your current password"

        YouNeedToEnterANewPassword ->
            "You need to enter a new password"

        PasswordConfirmationDoesNotMatch ->
            "Password confirmation doesn't match"


translateEnglishSetupWizardText : SetupWizardText -> String
translateEnglishSetupWizardText text =
    case text of
        WelcomeToSyncrypt ->
            "Welcome to Syncrypt"

        WelcomeHeader1 ->
            "We'll guide you through a step-by-step setup process to initiate your Syncrypt account."

        WelcomeHeader2 ->
            "Please pick a language:"

        AccountSetup ->
            "Account Setup"

        DoYouAlreadyHaveASyncryptAccount ->
            "Do you already have a Syncrypt Account?"

        YouCanLoginWithAnExistingAccountOrCreateANewOne ->
            "You can login with an existing account or create a new one and get started right away."

        LoginWithAccount ->
            "Yes, login with account"

        SignUpWithNewAccount ->
            "No, sign up with new account"

        AccountLogin ->
            "Account Login"

        LoginWithYourAccount ->
            "Login with your existing Syncrypt Account"

        IfYouForgotYourPassword ->
            "If you forgot your password, enter your email and press the button below."

        WeWillSendYouAPasswordResetLink ->
            "We will send you a password reset link to the email you entered."

        YourEmail ->
            "Your Email"

        Password ->
            "Password"

        ForgotPassword ->
            "Forgot Password"

        PasswordResetLinkHasBeenSent ->
            "Password reset link has been sent."

        Login ->
            "Login"

        AccountSignup ->
            "Account Signup"

        LegalNotice ->
            "Legal Notice"

        PleaseReadAndConfirm ->
            "Please read and confirm the following agreement and terms of service:"

        TOS1 ->
            "I hereby permit SYNCRYPT UG (haftungsbeschränkt), henceforth: Syncrypt, to collect, save, process and use my personal data."

        TOS2 ->
            "The collected data in particular is: Last name, first name & email adress to create and sustain a customer account. It is collected with the registration and saved for the entire period of service."

        TOS3 ->
            "The IP-adress is stored for a period of maximum two weeks to be able to identify and prevent attacks on our servers."

        TOS4 ->
            "Syncrypt stores and uses the personal data only to provide the service."

        TOS5 ->
            "Under no circumstance will Syncrypt sell personal Data to advertisers or other third parties."

        TOS6 ->
            "It is possible that the government and/or justice system will contact Syncrypt and ask to provide personal data. Every request will be examined by Syncrypt to full extent before releasing any data. If the legal requirements are granted, Syncrypt can be forced to give away this data and any stored encrypted files. All stored files are still encrypted with your private key. Syncrypt has no way to circumvent or lift this encryption."

        TOS7 ->
            "I give my permission voluntarily. I can withdraw it anytime without having to state a reason. To do so, I can simply write an email to alpha@syncrypt.space."

        TOS8 ->
            "I understand that this service can not be provided without this permission. If I disagree with this document, I can not use the service."

        TOS9 ->
            "Syncrypt can change parts of this permission in the future. In that case I will be informed and have to give a new permission."

        TOS10 ->
            "The current Privacy Policy and this document can be found at syncrypt.space/legal."

        TOS11 ->
            "This permission is in accordance with §§ 4a I, 28 BDSG."

        IAgree ->
            "I agree"

        CreateYourNewAccount ->
            "Create your new account"

        Signup ->
            "Signup"

        KeyCreation ->
            "Key Creation"

        KeyImport ->
            "Key Import"

        KeyNotYetInitialized ->
            "Key not yet initialized."

        InitializingKey ->
            "Initializing key."

        KeySuccessfullyInitialized ->
            "Key successfully initialized."

        Updating ->
            "Updating..."


translateEnglishNewVaultWizardText : NewVaultWizardText -> String
translateEnglishNewVaultWizardText text =
    case text of
        VaultKeyImport ->
            "Vault Key import"

        ImportYourVaultKeyHere ->
            "You can Import your vault key here."

        SelectVaultKeyFileForImport ->
            "Select Vault Key file for import"

        SelectYourVaultImportDestinationFolder ->
            "Select the folder you want to use for the imported Vault"


translateEnglishReleaseNotesWizardText : ReleaseNotesWizardText -> String
translateEnglishReleaseNotesWizardText text =
    case text of
        ReleaseNotesTitle ->
            "Release Notes"


translateEnglishMainTutorialText : MainTutorialText -> String
translateEnglishMainTutorialText text =
    case text of
        MainTutorialS1T ->
            "Tutorial"

        MainTutorialS1P1 ->
            "Click next to start the tutorial and learn more about how Syncrypt works."

        MainTutorialS2T ->
            "Vault List"

        MainTutorialS2TT1 ->
            "Your synchronized vaults"

        MainTutorialS2P1 ->
            "The vault list to the left shows all your currently cloned & synchronized vaults."

        MainTutorialS2P2 ->
            "Each vault is represented as a card with some meta information, like the number of people with access to the vault, the size of all files uploaded as well as the total number of files in the vault."

        MainTutorialS2P3 ->
            "You can also see the last time the vault has been updated."

        MainTutorialS3T ->
            "Remote Vault List"

        MainTutorialS3TT1 ->
            "Your available cloud vaults"

        MainTutorialS3P1 ->
            "Below you can see all remote vaults you have access to, but don't currently have cloned to this device."

        MainTutorialS3P2 ->
            "Click on a remote vault to pick a location to clone it to and download & synchronize files to and from it."

        MainTutorialS4T ->
            "Status Bar"

        MainTutorialS4TT1 ->
            "The Status Bar shows realtime stats on background tasks"

        MainTutorialS4P1 ->
            "At the bottom of the application you can see the status bar. It displays information on background tasks like uploads, downloads, open & idle connection and more."

        MainTutorialS4P2 ->
            "You can hover your mouse over it to get a better idea of what each icon means."

        MainTutorialS5T ->
            "Daemon Log Viewer"

        MainTutorialS5TT1 ->
            "Daemon Log"

        MainTutorialS5P1 ->
            "You can view what the Syncrypt background process is doing in the daemon log viewer."

        MainTutorialS5P2 ->
            "View up-to-date information on what Syncrypt is doing, what vaults are being synchronized and other useful debugging information."

        MainTutorialS6T ->
            "Feedback & Bug reports"

        MainTutorialS6TT1 ->
            "Send Feedback"

        MainTutorialS6P1 ->
            "You can send us suggestions for improvements, error & bug reports as well as any other feedback."

        MainTutorialS6P2 ->
            "If there are any questions on our part, we will get back to you via Email."

        MainTutorialS7T ->
            "Program & Account Settings"

        MainTutorialS7TT1 ->
            "Settings Dialog"

        MainTutorialS7P1 ->
            "You can configure various account and program settings in the settings dialog."

        MainTutorialS7P2 ->
            "Feel free to play around and explore the available settings. Most of them should be pretty self-explanatory but if you run into any issues, please let us know."

        MainTutorialS8T ->
            "Logout"

        MainTutorialS8TT1 ->
            "Logout here"

        MainTutorialS8P1 ->
            "If you wish to logout of Syncrypt or login with another account, click the logout button in the top right corner."


translateGerman : Text -> String
translateGerman text =
    case text of
        LoginDialogTxt t ->
            translateGermanLoginDialogText t

        NotificationTxt t ->
            translateGermanNotificationText t

        StatsTxt st ->
            translateGermanStatsText st

        SyncedVaults vaultCount ->
            if vaultCount == 1 then
                "1 synchronisierter Vault"
            else
                toString vaultCount ++ " synchronisierte Vaults"

        VaultsLoading ->
            "..."

        VaultsNotAvailable ->
            "N/A"

        VaultsFailedToLoad reason ->
            "Fehler beim Laden der Vaults: " ++ reason

        CreateNewVault ->
            "Erzeuge einen neuen Vault"

        VaultNotSynced vaultId ->
            "Vault (nicht synchronisiert) " ++ vaultId

        Updated date now ->
            "Letzte Änderung vor " ++ germanDistance date now

        LastUpdateToVault ->
            "Letzte Änderung am"

        LastUpdateToVaultLabel ->
            "Letzte Änderung am Vault"

        NoFilesUploadedYet ->
            "Noch keine Dateien vorhanden"

        UsersWithAccessTooltip ->
            "Benutzer mit Zugriff"

        TotalVaultSizeTooltip ->
            "Gesamtgröße des Vaults (inkl. aller Dateiversionen)"

        TotalVaultFilesTooltip ->
            "Anzahl aller Dateien"

        VaultDialogTxt vt ->
            translateGermanVaultDialogText vt

        VaultListTxt vlt ->
            translateGermanVaultListText vlt

        Previous ->
            "Zurück"

        Next ->
            "Weiter"

        Cancel ->
            "Abbrechen"

        OK ->
            "OK"

        CopiedToClipboard ->
            "Kopiert"

        Finish ->
            "Beenden"

        Close ->
            "Schließen"

        Confirm ->
            "Bestätigen"

        YourFeedback ->
            "Dein Feedback an uns"

        SendUsYourFeedbackSuggestionsOrBugReports ->
            "Sende uns dein Feedback und Verbesserungsvorschläge"

        WeWillReplyToYouViaEmail ->
            "Wir werden Dir via Email antworten"

        TypeYourFeedbackHere ->
            "Dein Feedback"

        SendFeedback ->
            "Feedback absenden"

        ConfirmationDialogTxt cdt ->
            translateGermanConfirmationDialogText cdt

        Logout ->
            "Abmelden"

        SoftwareAndAccountSettings ->
            "Software- & Accounteinstellungen"

        SendUsFeedbackAndBugReports ->
            "Sende uns Feedback, Vorschläge und Bugreports"

        ViewDaemonLog ->
            "Syncrypt Logbuch öffnen"

        ImportVault ->
            "Importiere einen zuvor exportierten Vault & Schlüssel"

        ProgramSettings ->
            "Einstellungen"

        SettingsDialogTxt text ->
            translateGermanSettingsDialogText text

        UpdateAvailable version ->
            "Update verfügbar, klicken zum Installieren der Version " ++ version

        SetupWizardTxt text ->
            translateGermanSetupWizardText text

        NewVaultWizardTxt text ->
            translateGermanNewVaultWizardText text

        ReleaseNotesWizardTxt text ->
            translateGermanReleaseNotesWizardText text

        FinishTutorial ->
            "Tutorial beenden"

        SkipTutorial ->
            "Tutorial überspringen"

        MainTutorialTxt text ->
            translateGermanMainTutorialText text


translateGermanLoginDialogText : LoginDialogText -> String
translateGermanLoginDialogText t =
    case t of
        LoginEmail ->
            "E-Mail"

        LoginPassword ->
            "Passwort"

        DaemonConnectionTimedOut ->
            "Daemon Verbindung abgebrochen."

        NetworkError ->
            "Netzwerkfehler."

        BadStatusOrLoginFailed resp ->
            "Login fehlgeschlagen: " ++ toString resp

        UnknownError httpError ->
            "Unbekannter Fehler bei Kommunikation mit Syncrypt Hintergrundprozess"
                |> andLog "Unexpected login error: " httpError


translateGermanNotificationText : NotificationText -> String
translateGermanNotificationText t =
    case t of
        VaultCreated vaultId ->
            "Vault wurde erstellt"

        VaultImported vaultId ->
            "Vault wurde importiert"

        VaultDeleted vaultId ->
            "Vault wurde in der Cloud gelöscht"

        VaultUpdated vaultId ->
            "Vault wurde aktualisiert"

        VaultRemoved vaultId ->
            "Vault sync wurde deaktiviert"

        VaultCreateFailed reason ->
            case reason of
                FolderAlreadyInSync ->
                    "Vault konnte nicht erstellt werden. Ordner wird bereits in einem anderen Vault aktiv verwendet."

                FolderPathNotValid ->
                    "Vault konnte nicht erstellt werden. Der angegebene Ordner existiert nicht."

        VaultRemoveFailed ->
            "Es gab einen Fehler beim Deaktivieren des Vaults. Bitte versuche es noch einmal."

        VaultDeleteFailed ->
            "Es gab einen Fehler beim Löschen des Vaults. Bitte versuche es noch einmal."

        VaultImportFailed ->
            "Es gab einen Fehler beim Import des Vaults. Bitte versuche es noch einmal."

        VaultMetadataUpdateFailed vaultId ->
            "Es gab einen Fehler beim Aktualisieren der Metadaten des Vaults"

        VaultCloneFailed vaultId reason ->
            "Es gab einen Fehler beim Download des Vaults : "
                ++ reason

        VaultAddUserFailed vaultId email ->
            "Es gab einen Fehler beim Hinzufügen des Users "
                ++ email
                ++ " zum Vault"

        VaultExported vaultId ->
            "Vault wurde exportiert"

        VaultExportFailed _ ->
            "Vault Export ist fehlgeschlagen"

        CouldNotCloneVaultWithoutFolder vaultId ->
            "Konnte den Vault nicht klonen - Kein Ordner für die Dateien wurde festgelegt"

        NoPathSelected ->
            "Der Vault konnte nicht erstellt werden, da kein Pfad ausgewählt wurde"

        SyncryptInitialized ->
            "Syncrypt erfolgreich initialisiert"

        VaultIsResyncing ->
            "Der Vault wird neu gesynct und validiert."

        SendingFeedbackFailed ->
            "Feedback konnte nicht gesendet werden. Bitte versuche es nochmal."

        ThanksForYourFeedback ->
            "Danke für dein Feedback!"


translateGermanStatsText : StatsText -> String
translateGermanStatsText st =
    case st of
        StatsLoaded s ->
            toString (s.busySlots + s.idleSlots)
                ++ " offene Verbindungen ("
                ++ toString s.idleSlots
                ++ " ruhend) / "
                ++ toString s.downloads
                ++ " Downloads / "
                ++ toString s.uploads
                ++ " Uploads"

        StatsLoading ->
            "Stats werden abgerufen ..."

        StatsNotAvailable ->
            "Stats nicht verfügbar"

        StatsFailedToLoad reason ->
            "Fehler beim Laden der Stats: " ++ reason


translateGermanVaultDialogText : VaultDialogText -> String
translateGermanVaultDialogText vt =
    case vt of
        NameAndFilesTab ->
            "Dateien"

        UsersTab ->
            "Benutzer"

        UsersTabInfoText ownsVault ->
            if ownsVault then
                "Füge andere Nutzer zu diesem Vault hinzu um Dateien und Ordner sicher und einfach zu teilen. Du kannst so viele Leute einladen, wie du willst."
            else
                "Diese Nutzer haben derzeit Zugriff auf diesen Vault (dich eingeschlossen). Jeder Nutzer mit Zugriff kann Dateien in diesem Vault hinzufügen, bearbeiten, löschen und lesen."

        CryptoTab ->
            "Metadaten"

        LogTab ->
            "Log"

        AdminTab ->
            "Admin"

        AdminTabInfoText ->
            "Administrative Aktionen für diesen Vault"

        VaultNameLabel ->
            "Name"

        VaultNameTooltip ->
            "Name des Vaults. Wird vom Eigentümer festgelegt."

        FolderLabel ->
            "Ordner"

        FolderButtonLabel t ->
            case t of
                SelectFolder ->
                    "Ordner auswählen"

                CloneIntoFolder ->
                    "Ordner für Sync auswählen"

                FolderSelectedForSync path ->
                    path

                SyncedFolder path ->
                    path

        FolderButtonTooltip t ->
            case t of
                SelectFolder ->
                    "Wähle einen neuen Ordner für diesen Vault aus."

                CloneIntoFolder ->
                    "Klicke hier um einen Ordner für diesen Vault auszuwählen."

                FolderSelectedForSync _ ->
                    "Dieser neue Vault wird Dateien in diesem Ordner synchronisieren."

                SyncedFolder _ ->
                    "Dieser Vault synchronisiert Dateien in diesem Ordner."

        FilesLabel ->
            "Dateien"

        FileSelectionTooltip ->
            "Hier siehst du alle Dateien in diesem Vault. Du kannst einzelne Dateien und Unterordner von der automatischen Synchronisation ausschließen, falls du bestimmte Dateien nicht hoch- bzw. runterladen willst."

        UserInputLabel ->
            "Einladen"

        UserInputTooltip ->
            "Gib die Email einer Person ein, die du in diesen Vault einladen möchtest."

        VaultUsersLabel ->
            "Aktuelle Mitglieder"

        CryptoTabInfoText ->
            "Hier kannst du alle kryptographischen Details dieses Vaults einsehen."

        VaultIdLabel ->
            "Vault ID"

        VaultIdTooltip ->
            "Syncrypt Vault ID"

        FileRevisionsLabel ->
            "Datei Versionen"

        TotalNumberOfFileRevisionsTooltip ->
            "Anzahl aller Dateiversionen in diesem Vault"

        LastModifiedLabel ->
            "Zuletzt aktualisiert"

        LastModifiedTooltip ->
            "Zeit der letzten Änderung im Vault"

        NoChangesSoFar ->
            "Bisher keine Veränderungen."

        KeyAlgorithmLabel ->
            "Schlüssel Algorithmus"

        KeyAlgorithmTooltip ->
            "Asymmetrischer Verschlüsselungsalgorithmus des Vault Schlüssels"

        KeyFingerprintLabel ->
            "Schlüssel Fingerabdruck"

        KeyFingerprintTooltip ->
            "Fingerabdruck des Vault Schlüssels"

        TransferAlgorithmLabel ->
            "Transfer Algorithmus"

        TransferAlgorithmTooltip ->
            "Algorithmus für verschlüsselte Datenübertragung"

        HashAlgorithmLabel ->
            "Hash Algorithmus"

        HashAlgorithmTooltip ->
            "Datei Hash Algorithmus"

        AESKeyLengthLabel ->
            "AES Schlüssellänge"

        AESKeyLengthTooltip ->
            "Schlüssellänge für die symmetrische Dateiverschlüsselung"

        RSAKeyLengthLabel ->
            "RSA Schlüssellänge"

        RSAKeyLengthTooltip ->
            "Schlüssellänge des privaten Vault RSA Schlüssels"

        VaultRemoveButtonInfo ->
            "Stoppt die Sychronisation des Vaults auf diesem Gerät. Dadurch werden keine neuen hochgeladenen Dateien runter- bzw. lokale Veränderungen mehr hochgeladen."

        VaultResyncButtonInfo ->
            "Setzt den lokalen Zustand des Vaults zurück. Danach wird die Signierungskette nochmal neu gesynct und validiert."

        VaultDeleteButtonInfo ->
            "Lösche diesen Vault mit allen Dateien in der Syncrypt Cloud (es werden keine Dateien lokal gelöscht)."

        VaultExportButtonInfo ->
            "Du kannst deinen Vault exportieren um alle relevanten Schlüssel und Einstellungen zu sichern. Es erlaubt das Herunterladen & Wiederherstellen (Entschlüsselung) der Dateien im Falle eines Hardware- bzw. Festplattendefekts oder Diebstahls dieses Gerätes."

        AskDeleteVault ->
            "Vault löschen?"

        AskDeleteVaultExtended ->
            "Soll der Vault wirklich vom Server gelöscht werden? Beim Löschen auf dem Server werden auch die Vaultschlüssel und Metadaten gelöscht, deine lokalen Dateien im Vaultordner werden aber nicht gelöscht oder anderweitig verändert."

        ExportToFile ->
            "In Datei exportieren"

        CancelChanges ->
            "Änderungen verwerfen"

        DeleteFromServer ->
            "Vault auf dem Server löschen"

        StopSyncing ->
            "Synchronisation stoppen"

        Resync ->
            "Neu synchronisieren"

        CreateVault ->
            "Vault erstellen"

        SaveVault ->
            "Änderungen bestätigen & speichern"

        SyncVaultToFolder ->
            "Vault in Ordner synchronisieren"

        YouDontHaveAccessToVaultUsers ->
            "Du hast keinen Zugriff auf die Benutzerliste dieses Vaults"

        Filters ->
            "Filter"

        Time ->
            "Zeit"

        User ->
            "Benutzer"

        UserTooltip ->
            "↓ Verwendeter Fingerabdruck des Benutzers"

        Operation ->
            "Operation"

        OperationTooltip ->
            "↓ Versions ID (Revision ID)"

        FilePathOrMessage ->
            "Datei / Nachricht"

        DebugFilter ->
            "Debug"

        InfoFilter ->
            "Info"

        WarningFilter ->
            "Warnung"

        ErrorFilter ->
            "Fehler"

        HistoryFilter ->
            "Historie"

        LogFilter ->
            "Log"

        LogLevels ->
            "Log Level"

        InviteWithSelectedKeys ->
            "Nutzer mit ausgewählten Schlüsseln einladen"

        Folder ->
            "Ordner"

        Name ->
            "Name"

        Invited date now ->
            "Eingeladen vor " ++ germanDistance date now

        InvitedAt date ->
            "Eingeladen " ++ toString date

        Created date now ->
            "Erstellt vor " ++ germanDistance date now

        VaultOwner ->
            "Vaultbesitzer"

        AllEventsHaveBeenFiltered ->
            "Alle Ereignisse wurden gefiltert."

        HistoryItemDescription { email, operation, fingerprint, path } ->
            let
                fpath =
                    Maybe.withDefault "N/A" path
            in
            case operation of
                Data.Vault.CreateVault ->
                    "Vault erstellt"

                Data.Vault.SetMetadata ->
                    "Vault Metadaten aktualisiert"

                Data.Vault.AddUser ->
                    "Benutzer " ++ email ++ " zum Vault hinzugefügt"

                Data.Vault.RemoveUser ->
                    "Benutzer " ++ email ++ " vom Vault entfernt"

                Data.Vault.AddUserKey ->
                    "Benutzerschlüssel für " ++ email ++ " hinzugefügt: " ++ fingerprint

                Data.Vault.RemoveUserKey ->
                    "Benutzerschlüssel für  " ++ email ++ " hinzugefügt: " ++ fingerprint

                Data.Vault.AddFile ->
                    "Upload: " ++ fpath

                Data.Vault.DeleteFileRevision ->
                    "Datei Version gelöscht: " ++ fpath

                Data.Vault.RemoveFile ->
                    "Sämtliche Versionen gelöscht: " ++ fpath

                Data.Vault.RenameFile ->
                    "Datei umbenannt:" ++ fpath

                Data.Vault.RestoreFile ->
                    "Ältere Dateiversion wiederhergestellt: " ++ fpath

        ExportVaultKeyBundle ->
            "Exportiere Vault Schlüssel & -Einstellungen"


translateGermanVaultListText : VaultListText -> String
translateGermanVaultListText vlt =
    case vlt of
        HeaderDescription ->
            "Diese Vaults sind auf diesem Computer gespiegelt und werden synchronisiert"

        LoadVaultsFailed reason ->
            "Fehler beim Laden der Vaults: " ++ reason

        LoadingVaults ->
            "Vaults werden geladen"

        LoadRemoteVaults ->
            "Aktualisiere Cloud Vaults"

        Vaults ->
            "Vaults"

        AvailableVaults ->
            "Verfügbare Cloud Vaults"

        FetchingRemoteVaultInfo ->
            "Lade Cloud Vault Metadaten"

        YouDontHaveAnyRemoteVaultsYet ->
            "Es sind noch keine Vaults in der Cloud vorhanden. Erstelle einen neuen Vault durch klicken des Plus-Buttons."

        YouHaveClonedAllAvailableVaults ->
            "Es wurden bereits alle verfügbaren Vaults auf diesen Computer heruntergeladen."

        ClickOnVaultToClone ->
            "Klick auf einen Vault um ihn auf diesen Computer herunterzuladen"

        FetchingRemoteVaultsFailed reason ->
            "Fehler beim Laden der Cloud Vaults: " ++ reason


translateGermanConfirmationDialogText : ConfirmationDialogText -> String
translateGermanConfirmationDialogText cdt =
    case cdt of
        RemoveVaultFromSyncQuestion ->
            "Vault deaktivieren und automatische Synchronisation deaktivieren?"

        RemoveVaultFromSyncExplanation ->
            "Dieser Vault wird nicht mehr mit der Cloud synchronisiert. Lokale Änderungen werden nicht mehr hochgeladen und Dateien, die von anderen Benutzern hochgeladen wurden, werden nicht mehr auf diesem Gerät heruntergeladen."


translateGermanSettingsDialogText : SettingsDialogText -> String
translateGermanSettingsDialogText text =
    case text of
        ChooseYourLanguage ->
            "Wähle deine Sprache"

        AccountOptions ->
            "Kontoeinstellungen"

        ChangePassword ->
            "Passwort ändern"

        ConfirmChangePassword ->
            "Passwortänderung bestätigen"

        ResetPassword ->
            "Passwort zurücksetzen"

        OldPasswordLabel ->
            "Aktuelles Passwort"

        OldPasswordTooltip ->
            "Dein aktuelles Passwort, welches du durch ein neues ersetzen willst"

        NewPasswordLabel ->
            "Neues Passwort"

        NewPasswordTooltip ->
            "Dies wird Dein neues Passwort sein, sobald du die Änderung bestätigst"

        NewPasswordConfirmationLabel ->
            "Bestätige Dein neues Passwort"

        NewPasswordConfirmationTooltip ->
            "Hiermit stellen wir sicher, dass Du dich nicht vertippst"

        UpdateIsAvailable ->
            "Update verfügbar"

        KeyExportOrImport ->
            "Schlüssel exportieren / importieren"

        AutoStart ->
            "Autostart beim Booten"

        AutoStartEnabled ->
            "Autostart aktiviert"

        AutoStartDisabled ->
            "Autostart deaktiviert"

        AboutSyncryptDesktop ->
            "Über Syncrypt Desktop"

        Version ->
            "Version"

        DaemonAuthToken ->
            "Daemon auth token"

        YouNeedToEnterYourCurrentPassword ->
            "Du musst dein aktuelles Passwort eingeben"

        YouNeedToEnterANewPassword ->
            "Du musst ein neues Passwort eingeben"

        PasswordConfirmationDoesNotMatch ->
            "Passwortbestätigung gleicht nicht dem Passwort"


translateGermanSetupWizardText : SetupWizardText -> String
translateGermanSetupWizardText text =
    case text of
        WelcomeToSyncrypt ->
            "Willkommen bei Syncrypt"

        WelcomeHeader1 ->
            "Wir leiten dich Schritt für Schritt durch alle nötigen Schritte, um ein neues Syncrypt Profil einzurichten."

        WelcomeHeader2 ->
            "Bitte wähle eine Sprache:"

        AccountSetup ->
            "Profil Erstellung"

        DoYouAlreadyHaveASyncryptAccount ->
            "Hast du bereits ein Syncrypt Profil?"

        YouCanLoginWithAnExistingAccountOrCreateANewOne ->
            "Du kannst dich mit einem bestehenden Profil einloggen oder ein neues erstellen."

        LoginWithAccount ->
            "Ja, mit bestehendem Profil einloggen"

        SignUpWithNewAccount ->
            "Nein, ein neues Profil erstellen"

        AccountLogin ->
            "Profil Login"

        LoginWithYourAccount ->
            "Logge dich mit deinem bestehenden Syncrypt Profil ein"

        IfYouForgotYourPassword ->
            "Falls du dein Passwort vergessen hast, trage deine Email hier ein."

        WeWillSendYouAPasswordResetLink ->
            "Wir werden dir einen Link zum zurücksetzen des Passworts schicken."

        YourEmail ->
            "Deine Email"

        Password ->
            "Passwort"

        ForgotPassword ->
            "Passwort vergessen"

        PasswordResetLinkHasBeenSent ->
            "Email mit Link zum Zurücksetzen des Passworts wurde verschickt"

        Login ->
            "Einloggen"

        AccountSignup ->
            "Neues Profil Erstellen"

        LegalNotice ->
            "Rechtlicher Hinweis"

        PleaseReadAndConfirm ->
            "Bitte lies und bestätige die folgende Vereinbarung und Einwilligungserklärung zum Datenschutz"

        TOS1 ->
            "Ich willige hiermit ein, dass die SYNCRYPT UG (haftungsbeschränkt), im folgenden: Syncrypt, meine personenbezogene Daten erhebt, speichert verarbeitet und nutzt."

        TOS2 ->
            "Im Einzelenen sind diese: Name, Vorname und Email-Adresse für die gesamte Dauer der Wahrnehmung des Dienstes. Diese Daten werden erhoben um ein Kundenkonto anzulegen und den Dienst zu ermöglichen."

        TOS3 ->
            "Weiterhin wird die IP-Adresse für einen Zeitraum von bis zu zwei Wochen gespeichert. Zweck der Speicherung ist die Erkennung und Vermeidung von potenziellen Attacken."

        TOS4 ->
            "Die personenbezogenen Daten werden von Syncrypt nur für die Ausübung des Dienstes gespeichert und genutzt."

        TOS5 ->
            "Zu keiner Zeit und unter keinen Umständen werden die personenbezogenen Daten an Dritte zu Zwecken der Werbung weitergegeben."

        TOS6 ->
            "In bestimmten Fällen kann Syncrypt vom Staat und dessen unterstellten Stellen dazu aufgefordert werden Daten herauszugeben. Diese Anfragen werden von Syncrypt geprüft. Bei Vorliegen der gesetzlichen Voraussetzungen ist Syncrypt verpflichtet diese Daten und hochgeladene, verschlüsselte Dateien herauszugeben. Die hochgeladenenen Dateien sind weiterhin mit dem privaten Schlüssel des Benutzers verschlüsselt. Syncrypt hat keine Möglichkeit die Verschlüsselung der Daten des Nutzers aufzuheben oder zu umgehen."

        TOS7 ->
            "Diese Einwilligung gebe ich bewusst und freiwillig ab. Ich kann sie jederzeit widerrufen. Die Widerrufserklärung ist zu richten an: alpha@syncrypt.space"

        TOS8 ->
            "Ich verstehe, dass Syncrypt ohne diese personenbezogenen Daten den Dienst nicht anbieten kann. Sollte ich mit der Einwilligung nicht einverstanden sein oder diese zu einem späteren Zeitpunkt widerrufen, kann ich den Dienst nicht in Anspruch nehmen."

        TOS9 ->
            "Diese Einwilligung steht im Einklang mit den §§ 4a I, 28 BDSG"

        TOS10 ->
            "Syncrypt behält sich vor, die Einwilligungserklärung zum Datenschutz zu ändern und zu erweitern. In diesem Fall sendet Syncrypt die neue Version per Email an den Nutzer und muss erneut eingewilligt werden."

        TOS11 ->
            "Die aktuelle Version der Einwilligung Datenschutz liegt auf syncrypt.space/legal"

        IAgree ->
            "Ich stimme zu"

        CreateYourNewAccount ->
            "Erstelle dein neues Benutzerprofil"

        Signup ->
            "Anmelden"

        KeyCreation ->
            "Schlüssel Generierung"

        KeyImport ->
            "Schlüssel Import"

        KeyNotYetInitialized ->
            "Schlüssel wurde noch nicht generiert"

        InitializingKey ->
            "Generiere Schlüssel"

        KeySuccessfullyInitialized ->
            "Schlüssel wurde erfolgreich generiert."

        Updating ->
            "Aktualisiere..."


translateGermanNewVaultWizardText : NewVaultWizardText -> String
translateGermanNewVaultWizardText text =
    case text of
        VaultKeyImport ->
            "Vault Schlüssel Import"

        ImportYourVaultKeyHere ->
            "Hier kannst du einen Vault Key importieren."

        SelectVaultKeyFileForImport ->
            "Vault Schlüssel Datei für Import auswählen"

        SelectYourVaultImportDestinationFolder ->
            "Wähle einen Ordner für den importierten Vault"


translateGermanReleaseNotesWizardText : ReleaseNotesWizardText -> String
translateGermanReleaseNotesWizardText text =
    case text of
        ReleaseNotesTitle ->
            "Versionshinweise"


translateGermanMainTutorialText : MainTutorialText -> String
translateGermanMainTutorialText text =
    case text of
        MainTutorialS1T ->
            "Tutorial"

        MainTutorialS1P1 ->
            "Klicke auf weiter um das Tutorial zu starten und mehr über Syncrypt zu lernen."

        MainTutorialS2T ->
            "Vault Liste"

        MainTutorialS2TT1 ->
            "Deine synchroniserten Vaults"

        MainTutorialS2P1 ->
            "Die Vault-Liste auf der linken Seite zeigt dir all deine verfügbaren Vaults an, die du derzeit auf diesem Gerät synchronisiert hast."

        MainTutorialS2P2 ->
            "Jeder Vault ist als Karte repräsentiert. Darauf kannst du weitere Informationen sehen, wie die Anzahl an Leuten, die Zugriff auf einen Vault haben oder die Anzahl & Größe aller Dateien, die bereits hochgeladen wurden."

        MainTutorialS2P3 ->
            "Du kannst außerdem sehen, wann ein Vault zuletzt aktualisiert wurde"

        MainTutorialS3T ->
            "Verfügbare Cloud Vaults (nicht synchronisiert)"

        MainTutorialS3TT1 ->
            "Deine vefügbaren Cloud Vaults"

        MainTutorialS3P1 ->
            "Weiter unten kannst du alle verfügbaren, aber derzeit nicht auf diesem Gerät synchronisierten Vaults sehen."

        MainTutorialS3P2 ->
            "Klicke auf einen verfügbaren Vault und wähle einen Ort, wo du den Vault hin synchronisieren willst. Je nach Größe der Dateien in diesem Vault, kann die Synchronisation eine Weile dauern."

        MainTutorialS4T ->
            "Status Leiste"

        MainTutorialS4TT1 ->
            "Diese Status Leiste gibt Auskunft über Hintergrundprozesse"

        MainTutorialS4P1 ->
            "Am unteren Rand befindet sich die Status Leiste. Sie Auskunft über Hintergrundprozesse wie Dateiup- und -downloads, offene & inaktive Verbindungen zur Syncrypt Cloud und mehr."

        MainTutorialS4P2 ->
            "Du kannst mit deiner Mouse über die Status Leiste fahren um mehr Informationen zu den Symbolen zu sehen."

        MainTutorialS5T ->
            "Syncrypt Logbuch"

        MainTutorialS5TT1 ->
            "Syncrypt Logbuch (Daemon Log)"

        MainTutorialS5P1 ->
            "Du kannst dir im Logbuch Informationen zu allen aktuellen Hintergrundprozesse von Syncrypt anzeigen lassen."

        MainTutorialS5P2 ->
            "Sieh dir an z.B. an, welche Vaults gerade synchronisiert werden sowie weitere Informationen."

        MainTutorialS6T ->
            "Feedback & Bugreports"

        MainTutorialS6TT1 ->
            "Sende uns Feedback"

        MainTutorialS6P1 ->
            "Du kannst uns Verbesserungsvorschläge, Fehler, Anregungen und sonstiges Feedback schicken."

        MainTutorialS6P2 ->
            "Falls es Rückfragen gibt, werden wir uns bei dir per Email melden."

        MainTutorialS7T ->
            "Programm- & Accounteinstellungen"

        MainTutorialS7TT1 ->
            "Einstellungen"

        MainTutorialS7P1 ->
            "Du kannst verschiedene Account- und Programmeinstellungen vornehmen, um Syncrypt an deine Bedürfnisse anzupassen"

        MainTutorialS7P2 ->
            "Die vorhandenen Einstellungen sollten soweit selbsterklärend sein. Bei Unklarheiten oder etwaigen Problemen, stehen wir gerne zur Verfügung. Sende uns dazu einfach entsprechendes Feedback (siehe letzter Schritt)."

        MainTutorialS8T ->
            "Abmeldung / Benutzerwechsel"

        MainTutorialS8TT1 ->
            "Abmelden"

        MainTutorialS8P1 ->
            "Falls du dich aus Syncrypt ausloggen bzw. den Benutzer wechseln möchtest, klicke den Logout-Button im oberen rechten Rand."


germanDistance : Date -> Date -> String
germanDistance =
    Date.Distance.inWordsWithConfig Date.Distance.germanConfig


type alias HasNowAndLanguage a =
    { a | now : Maybe Date, language : Language }


timeAgo : Date -> HasNowAndLanguage a -> String
timeAgo date { now, language } =
    let
        config =
            dateDistanceConfig language
    in
    case now of
        Just now ->
            let
                distanceString =
                    Date.Distance.inWordsWithConfig config date now
            in
            case language of
                German ->
                    "vor " ++ distanceString

                English ->
                    distanceString ++ " ago"

        Nothing ->
            toString date


dateDistanceConfig : Language -> Date.Distance.Types.Config
dateDistanceConfig language =
    case language of
        English ->
            Date.Distance.defaultConfig

        German ->
            Date.Distance.germanConfig
