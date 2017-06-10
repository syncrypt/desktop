module Translation exposing (..)

import Syncrypt.Vault exposing (VaultId)
import Syncrypt.User exposing (Email)


type Language
    = English
    | German


type alias Reason =
    String


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
    | CouldNotCloneVaultWithoutFolder VaultId
    | NoPathSelected
    | Stats { stats : Int, downloads : Int, uploads : Int }
    | StatsLoading
    | StatsNotAvailable
    | StatsFailedToLoad Reason
    | SyncedVaults Int
    | VaultsLoading
    | VaultsNotAvailable
    | VaultsFailedToLoad Reason
    | CreateNewVault
    | VaultNotSynced VaultId
    | NameAndFilesTab
    | UsersTab
    | CryptoTab


type alias HasLanguage a =
    { a | language : Language }


t : HasLanguage a -> Text -> String
t { language } text =
    translate language text


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
            "Something went wrong while cloning the vault " ++ vaultId ++ " : " ++ reason

        VaultAddUserFailed vaultId email ->
            "Failed to add user " ++ email ++ " to vault " ++ vaultId

        CouldNotCloneVaultWithoutFolder vaultId ->
            "Could not clone vault - no folder specified"

        NoPathSelected ->
            "No path selected - vault not created"

        Stats { stats, downloads, uploads } ->
            (toString stats)
                ++ " file stats / "
                ++ (toString downloads)
                ++ " downloads / "
                ++ (toString uploads)
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

        NameAndFilesTab ->
            "Name & Files"

        UsersTab ->
            "Users"

        CryptoTab ->
            "Cryptography & Metadata"


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
            "Es gab einen Fehler beim Aktualisieren der Metadaten des Vaults: " ++ vaultId

        VaultCloneFailed vaultId reason ->
            "Es gab einen Fehler beim Download des Vaults " ++ vaultId ++ " : " ++ reason

        VaultAddUserFailed vaultId email ->
            "Es gab einen Fehler beim Hinzufügen des Users " ++ email ++ " zum Vault: " ++ vaultId

        CouldNotCloneVaultWithoutFolder vaultId ->
            "Konnte den Vault nicht klonen - Kein Ordner für die Dateien wurde festgelegt."

        NoPathSelected ->
            "Der Vault konnte nicht erstellt werden, da kein Pfad ausgewählt wurde."

        Stats { stats, downloads, uploads } ->
            (toString stats)
                ++ " File Stats / "
                ++ (toString downloads)
                ++ " Downloads / "
                ++ (toString uploads)
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
                (toString vaultCount) ++ " synchronisierte Vaults / "

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

        NameAndFilesTab ->
            "Name & Dateien"

        UsersTab ->
            "Benutzer"

        CryptoTab ->
            "Cryptographie & Metadaten"
