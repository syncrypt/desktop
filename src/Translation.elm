module Translation
    exposing
        ( Language(..)
        , Text(..)
        , VaultDialogText(..)
        , t
        , translate
        , timeAgo
        )

import Date exposing (Date)
import Date.Distance
import Data.Vault exposing (VaultId)
import Data.User exposing (Email)
import Data.Daemon exposing (KeyState, Stats)


type Language
    = English
    | German


type alias Reason =
    String


type alias Now =
    Date


type Text
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
    | AskDeleteVault
    | AskDeleteVaultExtended
    | Stats
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
    | VaultListHeaderDescription
    | VaultDialogText VaultDialogText


type VaultDialogText
    = NameAndFilesTab
    | UsersTab
    | UsersTabInfoText Bool -- owns vault?
    | CryptoTab
    | LogTab
    | AdminTab
    | CryptoTabInfoText
    | VaultIdLabel
    | VaultIdInfo
    | FileRevisionsLabel
    | TotalNumberOfFileRevisionsInfo
    | LastModifiedLabel
    | LastModifiedInfo
    | NoChangesSoFar
    | KeyAlgorithmLabel
    | KeyAlgorithmInfo
    | KeyFingerprintLabel
    | KeyFingerprintInfo
    | TransferAlgorithmLabel
    | TransferAlgorithmInfo
    | HashAlgorithmLabel
    | HashAlgorithmInfo
    | AESKeyLengthLabel
    | AESKeyLengthInfo
    | RSAKeyLengthLabel
    | RSAKeyLengthInfo


type alias HasLanguage a =
    { a | language : Language }


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

        AskDeleteVault ->
            "Delete vault?"

        AskDeleteVaultExtended ->
            "Do you really want to delete this vault from the server?"

        Stats s ->
            (toString (s.busySlots + s.idleSlots))
                ++ " open connections ("
                ++ (toString s.idleSlots)
                ++ " idle) / "
                ++ (toString s.stats)
                ++ " file queries / "
                ++ (toString s.downloads)
                ++ " downloads / "
                ++ (toString s.uploads)
                ++ " uploads"

        StatsLoading ->
            "Stats loading ..."

        StatsNotAvailable ->
            "Stats N/A"

        StatsFailedToLoad reason ->
            "Stats failed to load: " ++ reason

        SyncedVaults vaultCount ->
            if vaultCount == 1 then
                " 1 synchronized vault / "
            else
                (toString vaultCount) ++ " synchronized vaults / "

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
            "Synced " ++ (Date.Distance.inWords date now) ++ " ago"

        LastUpdateToVault ->
            "Last update "

        LastUpdateToVaultLabel ->
            "Last update to vault "

        NoFilesUploadedYet ->
            "No files uploaded yet"

        VaultListHeaderDescription ->
            "These vaults are cloned and synchronized on this computer."

        VaultDialogText vt ->
            translateEnglishVaultDialogText vt


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

        CryptoTabInfoText ->
            "Here you can see detailed information on this vault's cryptographic settings, used algorithms and keys."

        VaultIdLabel ->
            "Vault ID"

        VaultIdInfo ->
            "Syncrypt Vault ID"

        FileRevisionsLabel ->
            "File Revisions"

        TotalNumberOfFileRevisionsInfo ->
            "Total number of file revisions in this vault."

        LastModifiedLabel ->
            "Last modified"

        LastModifiedInfo ->
            "Date & time of last update to this vault."

        NoChangesSoFar ->
            "No changes so far."

        KeyAlgorithmLabel ->
            "Key Algorithm"

        KeyAlgorithmInfo ->
            "Asymmetric key algorithm used for vault key"

        KeyFingerprintLabel ->
            "Vault Key Fingerprint"

        KeyFingerprintInfo ->
            "Vault public key fingerprint"

        TransferAlgorithmLabel ->
            "Transfer Algorithm"

        TransferAlgorithmInfo ->
            "Algorithm used for encrypting data transfer"

        HashAlgorithmLabel ->
            "Hash Algorithm"

        HashAlgorithmInfo ->
            "Algorithm used for hashing file contents & names"

        AESKeyLengthLabel ->
            "AES Key Length"

        AESKeyLengthInfo ->
            "Length of symmetric file encryption keys in this vault"

        RSAKeyLengthLabel ->
            "RSA Key Length"

        RSAKeyLengthInfo ->
            "Length of vault private key"


translateGerman : Text -> String
translateGerman text =
    case text of
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

        AskDeleteVault ->
            "Vault löschen?"

        AskDeleteVaultExtended ->
            "Soll der Vault wirklich vom Server gelöscht werden?"

        Stats s ->
            (toString (s.busySlots + s.idleSlots))
                ++ " offene Verbindungen ("
                ++ (toString s.idleSlots)
                ++ " ruhend) / "
                ++ (toString s.stats)
                ++ " Dateiabfragen / "
                ++ (toString s.downloads)
                ++ " Downloads / "
                ++ (toString s.uploads)
                ++ " Uploads"

        StatsLoading ->
            "Stats werden abgerufen ..."

        StatsNotAvailable ->
            "Stats nicht verfügbar"

        StatsFailedToLoad reason ->
            "Fehler beim Laden der Stats: " ++ reason

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
            "Synchronisiert vor " ++ germanDistance date now

        LastUpdateToVault ->
            "Letzte Änderung am "

        LastUpdateToVaultLabel ->
            "Letzte Änderung am Vault "

        NoFilesUploadedYet ->
            "Bisher wurden keine Dateien hochgeladen"

        VaultListHeaderDescription ->
            "Diese Vaults sind auf diesem Computer gespiegelt und werden synchronisiert."

        VaultDialogText vt ->
            translateGermanVaultDialogText vt


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

        CryptoTabInfoText ->
            "Hier kannst du alle kryptographischen Details dieses Vaults einsehen."

        VaultIdLabel ->
            "Vault ID"

        VaultIdInfo ->
            "Syncrypt Vault ID"

        FileRevisionsLabel ->
            "Datei Versionen"

        TotalNumberOfFileRevisionsInfo ->
            "Anzahl aller Dateiversionen in diesem Vault."

        LastModifiedLabel ->
            "Zuletzt aktualisiert"

        LastModifiedInfo ->
            "Zeit der letzten Änderung im Vault."

        NoChangesSoFar ->
            "Bisher keine Veränderungen."

        KeyAlgorithmLabel ->
            "Schlüssel Algorithmus"

        KeyAlgorithmInfo ->
            "Asymmetrischer Verschlüsselungsalgorithmus der für den Vault Schlüssel genutzt wird."

        KeyFingerprintLabel ->
            "Vault Schlüssel Fingerabdruck"

        KeyFingerprintInfo ->
            "Vault public key fingerprint"

        TransferAlgorithmLabel ->
            "Transfer Algorithmus"

        TransferAlgorithmInfo ->
            "Algorithmus für verschlüsselte Datenübertragung"

        HashAlgorithmLabel ->
            "Hash Algorithmus"

        HashAlgorithmInfo ->
            "Datei Hash Algorithmus"

        AESKeyLengthLabel ->
            "AES Schlüssellänge"

        AESKeyLengthInfo ->
            "Länge des symmetrischen Schlüssels für die Dateiverschlüsselung in diesem Vault."

        RSAKeyLengthLabel ->
            "RSA Schlüssellänge"

        RSAKeyLengthInfo ->
            "Länge des privaten Vault RSA Schlüssels."


germanDistance =
    Date.Distance.inWordsWithConfig Date.Distance.germanConfig


type alias HasNowAndLanguage a =
    { a | now : Maybe Date, language : Language }


timeAgo : Date -> HasNowAndLanguage a -> String
timeAgo date { now, language } =
    let
        distanceConfig =
            case language of
                English ->
                    Date.Distance.defaultConfig

                German ->
                    Date.Distance.germanConfig
    in
        case now of
            Just now ->
                Date.Distance.inWordsWithConfig distanceConfig date now
                    ++ " ago"

            Nothing ->
                toString date
