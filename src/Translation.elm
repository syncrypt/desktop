module Translation
    exposing
        ( Language(..)
        , Text(..)
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
    | NameAndFilesTab
    | UsersTab
    | CryptoTab
    | LogTab
    | AdminTab
    | VaultListHeaderDescription


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

        NameAndFilesTab ->
            "Files"

        UsersTab ->
            "Users"

        CryptoTab ->
            "Metadata"

        LogTab ->
            "Log"

        AdminTab ->
            "Administration"

        VaultListHeaderDescription ->
            "These vaults are cloned and synchronized on this computer."


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

        Stats { stats, downloads, uploads, userKeyState, totalSlots, busySlots, idleSlots, closedSlots } ->
            (toString (busySlots + idleSlots))
                ++ " offene Verbindungen ("
                ++ (toString idleSlots)
                ++ " ruhend) / "
                ++ (toString stats)
                ++ " Dateiabfragen / "
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

        Updated date now ->
            "Synchronisiert vor " ++ (germanDistance date now)

        LastUpdateToVault ->
            "Letzte Änderung am "

        LastUpdateToVaultLabel ->
            "Letzte Änderung am Vault "

        NoFilesUploadedYet ->
            "Bisher wurden keine Dateien hochgeladen"

        NameAndFilesTab ->
            "Dateien"

        UsersTab ->
            "Benutzer"

        CryptoTab ->
            "Metadaten"

        LogTab ->
            "Log"

        AdminTab ->
            "Administration"

        VaultListHeaderDescription ->
            "Diese Vaults sind auf diesem Computer gespiegelt und werden synchronisiert."


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
                ((Date.Distance.inWordsWithConfig distanceConfig date now) ++ " ago")

            Nothing ->
                (toString date)
