module Translation
    exposing
        ( FolderButtonType(..)
        , NotificationText(..)
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


type VaultListText
    = HeaderDescription
    | LoadVaultsFailed Reason
    | LoadingVaults
    | LoadRemoteVaults
    | AvailableVaults
    | FetchingRemoteVaultInfo
    | ClickOnVaultToClone
    | FetchingRemoteVaultsFailed Reason


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
            "Synced " ++ Date.Distance.inWords date now ++ " ago"

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


translateEnglishNotificationText : NotificationText -> String
translateEnglishNotificationText t =
    case t of
        VaultCreated vaultId ->
            "Vault created: " ++ vaultId

        VaultDeleted vaultId ->
            "Vault deleted from server: " ++ vaultId

        VaultUpdated vaultId ->
            "Vault updated: " ++ vaultId

        VaultRemoved vaultId ->
            "Vault removed from sync: " ++ vaultId

        VaultCreateFailed reason ->
            "Failed to create vault: " ++ reason

        VaultRemoveFailed reason ->
            "Failed to remove vault: " ++ reason

        VaultDeleteFailed reason ->
            "Vault deletion failed: " ++ reason

        VaultMetadataUpdateFailed vaultId ->
            "Failed to update metadata for vault " ++ vaultId

        VaultCloneFailed vaultId reason ->
            "Something went wrong while cloning the vault "
                ++ vaultId
                ++ " : "
                ++ reason

        VaultAddUserFailed vaultId email ->
            "Failed to add user " ++ email ++ " to vault " ++ vaultId

        VaultExported vaultId ->
            "Vault exported: " ++ vaultId

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
                    "Select Folder to clone vault to"

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
                    "This vault is synchronizing files from and to this folder: " ++ path

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
            "Do you really want to delete this vault from the server?"


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
            "Bisher wurden keine Dateien hochgeladen"

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


translateGermanNotificationText t =
    case t of
        VaultCreated vaultId ->
            "Vault wurde erstellt: " ++ vaultId

        VaultDeleted vaultId ->
            "Vault wurde in der cloud gelöscht: " ++ vaultId

        VaultUpdated vaultId ->
            "Vault wurde aktualisiert: " ++ vaultId

        VaultRemoved vaultId ->
            "Vault sync wurde deaktiviert: " ++ vaultId

        VaultCreateFailed reason ->
            "Es gab einen Fehler beim Erstellen des Vaults: " ++ reason

        VaultRemoveFailed reason ->
            "Es gab einen Fehler beim Deaktivieren des Vaults: " ++ reason

        VaultDeleteFailed reason ->
            "Es gab einen Fehler beim Löschen des Vaults: " ++ reason

        VaultMetadataUpdateFailed vaultId ->
            "Es gab einen Fehler beim Aktualisieren der Metadaten des Vaults: "
                ++ vaultId

        VaultCloneFailed vaultId reason ->
            "Es gab einen Fehler beim Download des Vaults "
                ++ vaultId
                ++ " : "
                ++ reason

        VaultAddUserFailed vaultId email ->
            "Es gab einen Fehler beim Hinzufügen des Users "
                ++ email
                ++ " zum Vault: "
                ++ vaultId

        VaultExported vaultId ->
            "Vault wurde exportiert: " ++ vaultId

        VaultExportFailed _ ->
            "Vault Export ist fehlgeschlagen"

        CouldNotCloneVaultWithoutFolder vaultId ->
            "Konnte den Vault nicht klonen - Kein Ordner für die Dateien wurde festgelegt."

        NoPathSelected ->
            "Der Vault konnte nicht erstellt werden, da kein Pfad ausgewählt wurde."


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
                    "Ordner für Synchronisation auswählen"

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
            "Asymmetrischer Verschlüsselungsalgorithmus der für den Vault Schlüssel genutzt wird"

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
            "Soll der Vault wirklich vom Server gelöscht werden?"


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
            Date.Distance.inWordsWithConfig config date now
                ++ " ago"

        Nothing ->
            toString date


dateDistanceConfig : Language -> Date.Distance.Types.Config
dateDistanceConfig language =
    case language of
        English ->
            Date.Distance.defaultConfig

        German ->
            Date.Distance.germanConfig
