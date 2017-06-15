module Model exposing (..)

import Config exposing (Config)
import Dict exposing (Dict)
import Json.Decode as Json exposing (andThen, succeed)
import Json.Decode.Pipeline exposing (decode, optional, required, requiredAt, optionalAt)
import LoginDialog.Model
import RemoteData exposing (RemoteData(..), WebData)
import Syncrypt.User exposing (Email)
import Syncrypt.Vault exposing (FlyingVault, Vault, VaultId)
import Time exposing (Time)
import Translation
import Ui.NotificationCenter
import Util exposing (findFirst)
import VaultDialog.Model
import WizardDialog


type alias Stats =
    { stats : Int
    , downloads : Int
    , uploads : Int
    , totalSlots : Int
    , busySlots : Int
    , idleSlots : Int
    , closedSlots : Int
    }


type alias CurrentUser =
    { firstName : String
    , lastName : String
    , email : Syncrypt.User.Email
    }


type LoginState
    = Unknown
    | LoggedOut
    | LoggedIn CurrentUser


type alias StatusResponse =
    { success : Bool, text : Maybe String }


type alias ExportStatusResponse =
    { success : Bool, filename : String }


type alias Model =
    { config : Config
    , vaults : WebData (List Vault)
    , flyingVaults : WebData (List FlyingVault)
    , state : State
    , stats : WebData Stats
    , sidebarOpen : Bool
    , now : Maybe Time
    , loginDialog : LoginDialog.Model.State
    , vaultDialogs : Dict VaultId VaultDialog.Model.State
    , notificationCenter : Ui.NotificationCenter.Model Msg
    , login : LoginState
    , language : Translation.Language
    , wizardDialog : WizardDialog.State Msg
    }


type State
    = LoadingVaults
    | UpdatingVaults
    | ShowingAllVaults
    | ShowingVaultDetails Vault
    | ShowingFlyingVaultDetails FlyingVault
    | CreatingNewVault
    | CloningVault VaultId


type Msg
    = SetTime Time
    | UpdateLoginState
    | UpdateVaults
    | UpdateFlyingVaults
    | UpdateStats
    | UpdatedLoginState (WebData LoginState)
    | UpdatedVaultsFromApi (WebData (List Vault))
    | UpdatedFlyingVaultsFromApi (WebData (List FlyingVault))
    | UpdatedStatsFromApi (WebData Stats)
    | OpenVaultDetails Vault
    | OpenVaultFolder Vault
    | OpenFlyingVaultDetails FlyingVault
    | CloneVault VaultId
    | ClonedVault VaultId (WebData Vault)
    | CloseVaultDetails VaultId
    | SaveVaultDetails VaultId
    | OpenProgramSettings
    | OpenAccountSettings
    | Logout
    | CreateNewVault
    | CreatedVault VaultDialog.Model.State (WebData Vault)
    | ExportedVault VaultId (WebData ExportStatusResponse)
    | VaultDialog VaultId VaultDialog.Model.Msg
    | FocusOn String
    | NotificationCenter Ui.NotificationCenter.Msg
    | RemoveVaultFromSync VaultId
    | RemovedVaultFromSync (WebData VaultId)
    | DeletedVault (WebData VaultId)
    | VaultUserAdded VaultId Email (WebData Email)
    | VaultMetadataUpdated VaultId (WebData Vault)
    | Login
    | LoginResult Email (WebData StatusResponse)
    | LogoutResult (WebData StatusResponse)
    | LoginDialog LoginDialog.Model.Msg
    | WizardDialog WizardDialog.Msg



-- JSON Decoders


statsDecoder : Json.Decoder Stats
statsDecoder =
    decode Stats
        |> requiredAt [ "stats", "stats" ] Json.int
        |> requiredAt [ "stats", "downloads" ] Json.int
        |> requiredAt [ "stats", "uploads" ] Json.int
        |> requiredAt [ "slots", "total" ] Json.int
        |> optionalAt [ "slots", "busy" ] Json.int 0
        |> optionalAt [ "slots", "idle" ] Json.int 0
        |> optionalAt [ "slots", "closed" ] Json.int 0


loginStateDecoder : Json.Decoder LoginState
loginStateDecoder =
    decode CurrentUser
        |> required "first_name" Json.string
        |> required "last_name" Json.string
        |> required "email" Json.string
        |> andThen (succeed << LoggedIn)


statusResponseDecoder : Json.Decoder StatusResponse
statusResponseDecoder =
    let
        parseStatus =
            Json.string
                |> andThen (\s -> succeed (s == "ok"))
    in
        decode StatusResponse
            |> required "status" parseStatus
            |> optional "text" (Json.maybe Json.string) Nothing


exportStatusResponseDecoder : Json.Decoder ExportStatusResponse
exportStatusResponseDecoder =
    let
        parseStatus =
            Json.string
                |> andThen (\s -> succeed (s == "ok"))
    in
        decode ExportStatusResponse
            |> required "status" parseStatus
            |> required "filename" Json.string



-- Model functions


init : Config -> Model
init config =
    { config = config
    , vaults = NotAsked
    , flyingVaults = NotAsked
    , state = LoadingVaults
    , stats = NotAsked
    , sidebarOpen = False
    , now = Nothing
    , loginDialog = LoginDialog.Model.init
    , vaultDialogs = Dict.fromList [ ( "", VaultDialog.Model.init ) ]
    , notificationCenter =
        Ui.NotificationCenter.init ()
            |> Ui.NotificationCenter.timeout 2500
            |> Ui.NotificationCenter.duration 2500
    , login = Unknown
    , language = Translation.English
    , wizardDialog = WizardDialog.init WizardDialog
    }


vaultWithId : VaultId -> Model -> Vault
vaultWithId vaultId { vaults, flyingVaults } =
    let
        hasId =
            \v -> v.id == vaultId
    in
        case findFirst hasId (RemoteData.withDefault [] vaults) of
            Nothing ->
                case findFirst hasId (RemoteData.withDefault [] flyingVaults) of
                    Nothing ->
                        Syncrypt.Vault.init vaultId

                    Just fv ->
                        fv |> Syncrypt.Vault.asVault

            Just v ->
                v


vaultIds : Model -> List VaultId
vaultIds { vaults, flyingVaults } =
    let
        idsOf =
            List.map .id

        orEmpty =
            RemoteData.withDefault []
    in
        (idsOf (vaults |> orEmpty)) ++ (idsOf (flyingVaults |> orEmpty))


retryOnFailure : WebData a -> msg -> Model -> ( Model, Cmd msg )
retryOnFailure data msg model =
    case data of
        Failure reason ->
            let
                _ =
                    Debug.log "Retrying due to failure: " ( msg, reason )
            in
                ( model, Util.delayMsg model.config.updateInterval msg )

        _ ->
            ( model, Cmd.none )
