module Translation
    exposing
        ( ConfirmationDialogText(..)
        , FolderButtonType(..)
        , NotificationText(..)
        , SettingsDialogText(..)
        , SetupWizardText(..)
        , StatsText(..)
        , Text(..)
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
import Dict exposing (Dict)
import Language exposing (HasLanguage, Language(..))


type alias Reason =
    String


type alias Now =
    Date


type Text
    = NotificationText NotificationText
    | StatsText StatsText
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
    | TotalVaultRevisionsTooltip
    | VaultDialogText VaultDialogText
    | VaultListText VaultListText
    | Previous
    | Next
    | Cancel
    | OK
    | Finish
    | Close
    | Confirm
    | YourFeedback
    | SendUsYourFeedbackSuggestionsOrBugReports
    | WeWillReplyToYouViaEmail
    | TypeYourFeedbackHere
    | SendFeedback
    | ConfirmationDialogText ConfirmationDialogText
    | Logout
    | SoftwareAndAccountSettings
    | SendUsFeedbackAndBugReports
    | ViewDaemonLog
    | ProgramSettings
    | RefreshVaults
    | SettingsDialogText SettingsDialogText
    | UpdateAvailable String
    | SetupWizardText SetupWizardText


type NotificationText
    = VaultCreated VaultId
    | VaultRemoved VaultId
    | VaultDeleted VaultId
    | VaultUpdated VaultId
    | VaultCreateFailed Reason
    | VaultRemoveFailed Reason
    | VaultDeleteFailed Reason
    | VaultMetadataUpdateFailed VaultId
    | VaultCloneFailed VaultId Reason
    | VaultAddUserFailed VaultId Email
    | VaultExported VaultId
    | VaultExportFailed Reason
    | CouldNotCloneVaultWithoutFolder VaultId
    | NoPathSelected


type StatsText
    = Stats
        { stats : Int
        , downloads : Int
        , uploads : Int
        , userKeyState : KeyState
        , totalSlots : Int
        , busySlots : Int
        , idleSlots : Int
        , closedSlots : Int
        }
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
    | VaultDeleteButtonInfo
    | VaultExportButtonInfo
    | AskDeleteVault
    | AskDeleteVaultExtended
    | ExportToFile
    | CancelChanges
    | DeleteFromServer
    | StopSyncing
    | CreateVault
    | SaveVault
    | SyncVaultToFolder
    | YouDontHaveAccessToVaultUsers


type VaultListText
    = HeaderDescription
    | LoadVaultsFailed Reason
    | LoadingVaults
    | LoadRemoteVaults
    | AvailableVaults
    | FetchingRemoteVaultInfo
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
    | KeyCreation


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
    translate text language


{-| Translates a `Text` into a `String` based on `lang`.

    translate (VaultCreated "123") English -- -> "Vault created: 123"
    translate (VaultCreated "123") German  -- -> "Vault wurde erstellt: 123"

-}
translate : Text -> Language -> String
translate text lang =
    case lang of
        English ->
            translateEnglish text

        German ->
            translateGerman text


translateEnglish : Text -> String
translateEnglish text =
    case text of
        NotificationText t ->
            translateEnglishNotificationText t

        StatsText st ->
            translateEnglishStatsText st

        SyncedVaults vaultCount ->
            if vaultCount == 1 then
                " 1 synchronized vault / "
            else
                toString vaultCount ++ " synchronized vaults / "

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
            "Updated " ++ Date.Distance.inWords date now ++ " ago"

        LastUpdateToVault ->
            "Last update "

        LastUpdateToVaultLabel ->
            "Last update to vault "

        NoFilesUploadedYet ->
            "No files uploaded yet"

        UsersWithAccessTooltip ->
            "Users with access"

        TotalVaultSizeTooltip ->
            "Total vault size (with all file revisions on server)"

        TotalVaultRevisionsTooltip ->
            "Total number of file revisions in the vault"

        VaultDialogText vt ->
            translateEnglishVaultDialogText vt

        VaultListText vlt ->
            translateEnglishVaultListText vlt

        Previous ->
            "Previous"

        Next ->
            "Next"

        Cancel ->
            "Cancel"

        OK ->
            "OK"

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

        ConfirmationDialogText cdt ->
            translateEnglishConfirmationDialogText cdt

        Logout ->
            "Logout"

        SoftwareAndAccountSettings ->
            "Software & Account Settings"

        SendUsFeedbackAndBugReports ->
            "Send us feedback & bug reports"

        ViewDaemonLog ->
            "View the Syncrypt background process log"

        ProgramSettings ->
            "Program Settings"

        RefreshVaults ->
            "Refresh Vaults"

        SettingsDialogText text ->
            translateEnglishSettingsDialogText text

        UpdateAvailable version ->
            "Update available, click here to install version " ++ version

        SetupWizardText text ->
            translateEnglishSetupWizardText text


translateEnglishNotificationText : NotificationText -> String
translateEnglishNotificationText t =
    case t of
        VaultCreated vaultId ->
            "Vault created"

        VaultDeleted vaultId ->
            "Vault deleted from server"

        VaultUpdated vaultId ->
            "Vault updated"

        VaultRemoved vaultId ->
            "Vault removed from sync"

        VaultCreateFailed reason ->
            "Failed to create vault: " ++ reason

        VaultRemoveFailed reason ->
            "Failed to remove vault: " ++ reason

        VaultDeleteFailed reason ->
            "Vault deletion failed: " ++ reason

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


translateEnglishStatsText : StatsText -> String
translateEnglishStatsText st =
    case st of
        Stats s ->
            toString (s.busySlots + s.idleSlots)
                ++ " open connections ("
                ++ toString s.idleSlots
                ++ " idle) / "
                ++ toString s.stats
                ++ " file queries / "
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
            "Administration"

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
            "Stop synchronizing this vault on this computer. Will stop all local changes from being uploaded and any remote changes being downloaded to this computer."

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
            "Cancel Changes"

        DeleteFromServer ->
            "Delete from Server"

        StopSyncing ->
            "Stop syncing"

        CreateVault ->
            "Create Vault"

        SaveVault ->
            "Save Vault"

        SyncVaultToFolder ->
            "Sync vault to folder"

        YouDontHaveAccessToVaultUsers ->
            "You don't have access to this vault's user list."


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

        AvailableVaults ->
            "Available Vaults"

        FetchingRemoteVaultInfo ->
            "Fetching remote vault info "

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
            "This vault will stop being synchronized to this computer. Any local file changes won't be uploaded and new files added to the vault won't be downloaded to this computer."


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

        KeyCreation ->
            "Key Creation"


translateGerman : Text -> String
translateGerman text =
    case text of
        NotificationText t ->
            translateGermanNotificationText t

        StatsText st ->
            translateGermanStatsText st

        SyncedVaults vaultCount ->
            if vaultCount == 1 then
                " 1 synchronisierter Vault / "
            else
                toString vaultCount ++ " synchronisierte Vaults / "

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
            "Aktualisiert vor " ++ germanDistance date now

        LastUpdateToVault ->
            "Letzte Änderung am "

        LastUpdateToVaultLabel ->
            "Letzte Änderung am Vault "

        NoFilesUploadedYet ->
            "Noch keine Dateien vorhanden"

        UsersWithAccessTooltip ->
            "Benutzer mit Zugriff"

        TotalVaultSizeTooltip ->
            "Gesamtgröße des Vaults (inkl. aller Dateiversionen)"

        TotalVaultRevisionsTooltip ->
            "Anzahl aller Dateirevisionen in diesem Vault"

        VaultDialogText vt ->
            translateGermanVaultDialogText vt

        VaultListText vlt ->
            translateGermanVaultListText vlt

        Previous ->
            "Zurück"

        Next ->
            "Weiter"

        Cancel ->
            "Abbrechen"

        OK ->
            "OK"

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

        ConfirmationDialogText cdt ->
            translateGermanConfirmationDialogText cdt

        Logout ->
            "Abmelden"

        SoftwareAndAccountSettings ->
            "Software- & Accounteinstellungen"

        SendUsFeedbackAndBugReports ->
            "Sende uns Feedback, Vorschläge und Bugreports"

        ViewDaemonLog ->
            "Syncrypt Logbuch öffnen"

        ProgramSettings ->
            "Einstellungen"

        RefreshVaults ->
            "Vaults aktualisieren"

        SettingsDialogText text ->
            translateGermanSettingsDialogText text

        UpdateAvailable version ->
            "Update verfügbar, klicken zum Installieren"

        SetupWizardText text ->
            translateGermanSetupWizardText text


translateGermanNotificationText : NotificationText -> String
translateGermanNotificationText t =
    case t of
        VaultCreated vaultId ->
            "Vault wurde erstellt"

        VaultDeleted vaultId ->
            "Vault wurde in der Cloud gelöscht"

        VaultUpdated vaultId ->
            "Vault wurde aktualisiert"

        VaultRemoved vaultId ->
            "Vault sync wurde deaktiviert"

        VaultCreateFailed reason ->
            "Es gab einen Fehler beim Erstellen des Vaults: " ++ reason

        VaultRemoveFailed reason ->
            "Es gab einen Fehler beim Deaktivieren des Vaults: " ++ reason

        VaultDeleteFailed reason ->
            "Es gab einen Fehler beim Löschen des Vaults: " ++ reason

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


translateGermanStatsText : StatsText -> String
translateGermanStatsText st =
    case st of
        Stats s ->
            toString (s.busySlots + s.idleSlots)
                ++ " offene Verbindungen ("
                ++ toString s.idleSlots
                ++ " ruhend) / "
                ++ toString s.stats
                ++ " Dateiabfragen / "
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
            "Administration"

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
            "Vault public key fingerprint"

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

        CreateVault ->
            "Vault Erstellen"

        SaveVault ->
            "Vault Speichern"

        SyncVaultToFolder ->
            "Vault in Ordner synchronisieren"

        YouDontHaveAccessToVaultUsers ->
            "Du hast keinen Zugriff auf die Benutzerliste dieses Vaults"


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

        AvailableVaults ->
            "Verfügbare Cloud Vaults"

        FetchingRemoteVaultInfo ->
            "Lade Cloud Vault Metadaten"

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
            "Dieser Vault wird nicht mehr mit der Cloud synchronisiert. Lokale Änderungen werden nicht mehr hochgeladen und Dateien, die von anderen Benutzern hochgeladen wurden, werden nicht mehr auf diesen Computer runtergeladen."


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


translateGermanSetupWizardText text =
    case text of
        WelcomeToSyncrypt ->
            "Willkommen bei Syncrypt"

        WelcomeHeader1 ->
            "Wir leiten dich Schritt für Schritt durch alle nötigen Schritte, um ein neues Syncrypt Profil einzurichten."

        WelcomeHeader2 ->
            "Bitte wähle eine Sprache: "

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

        KeyCreation ->
            "Schlüssel Generierung"


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
