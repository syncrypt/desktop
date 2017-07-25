module VaultDialog.Model exposing (..)

import Dict exposing (Dict)
import Ui.Checkbox
import Ui.Input
import Ui.Modal
import Ui.Tabs
import RemoteData exposing (RemoteData(..), WebData)
import Set exposing (Set)
import Path exposing (Path, asPath)
import Syncrypt.Vault exposing (Vault, FlyingVault, VaultId, nameOrId, VaultLogItem)
import Syncrypt.User as User
import ConfirmationDialog


type alias FileName =
    String


type alias FolderItem =
    ( Path, List FileName )


type CloneStatus
    = New
    | Cloned
    | NotCloned


type alias State =
    { id : VaultId
    , cloneStatus : CloneStatus
    , hasChangesPending : Bool
    , title : String
    , icon : Maybe String
    , modal : Ui.Modal.Model
    , confirmationDialog : ConfirmationDialog.Model Msg
    , nameInput : Ui.Input.Model
    , userInput : Ui.Input.Model
    , tabs : Ui.Tabs.Model
    , localFolderPath : Maybe Path
    , localFolderItems : Dict Path (List String)
    , ignoredFolderItems : Set Path
    , expandedFolders : Set Path
    , users : WebData (List User.User)
    , logItems : WebData (List VaultLogItem)
    , usersToAdd : Dict User.Email (List User.UserKey)
    , userKeys : Dict User.Email (WebData (List User.UserKey))
    , vaultFingerprints : WebData (Set User.Fingerprint)
    }


type RequiresConfirmation
    = DeleteVault
    | RemoveVault
    | AddUser


type Msg
    = ModalMsg Ui.Modal.Msg
    | ConfirmationDialogMsg ConfirmationDialog.Msg
    | NameChanged
    | NameInputMsg Ui.Input.Msg
    | UserInputMsg Ui.Input.Msg
    | FileCheckBoxMsg Path Ui.Checkbox.Msg
    | NestedFileList Path FolderItem
    | ToggleIgnorePath Path
    | OpenFolderDialog
    | OpenFolder String
    | SelectedFolder Path
    | TabsMsg Ui.Tabs.Msg
    | CollapseFolder Path
    | ExpandFolder Path
    | Confirm RequiresConfirmation
    | Confirmed RequiresConfirmation
    | AddUserWithKeys User.Email (List User.UserKey)
    | ToggleUserKey User.Email User.UserKey
    | UserKeyCheckbox User.Email User.UserKey Ui.Checkbox.Msg
    | SearchUserKeys User.Email
    | FoundUserKeys User.Email (WebData (List User.UserKey))
    | GetVaultFingerprints
    | FoundVaultFingerprints (WebData (List User.Fingerprint))
    | FetchedUsers (WebData (List User.User))
    | FetchedVaultEventLog (WebData (List VaultLogItem))
    | SetUserInput String
    | OpenIconDialog
    | SelectedIcon String
    | OpenExportDialog
    | SelectedExportFile String


init : State
init =
    { id = ""
    , cloneStatus = New
    , hasChangesPending = False
    , title = "Untitled Vault"
    , icon = Nothing
    , ignoredFolderItems = Set.fromList [ [ ".DS_Store" ], [ ".vault" ] ]
    , localFolderPath = Nothing
    , localFolderItems = Dict.empty
    , expandedFolders = Set.fromList [ [] ]
    , modal =
        Ui.Modal.init
            |> Ui.Modal.closable False
            |> Ui.Modal.backdrop False
    , confirmationDialog =
        ConfirmationDialog.init ConfirmationDialogMsg
    , nameInput =
        Ui.Input.init ()
            |> Ui.Input.placeholder "Vault Name"
            |> Ui.Input.showClearIcon True
    , userInput =
        Ui.Input.init ()
            |> Ui.Input.kind "email"
            |> Ui.Input.placeholder "user@example.com"
            |> Ui.Input.showClearIcon True
    , tabs =
        Ui.Tabs.init ()
    , users = NotAsked
    , logItems = NotAsked
    , usersToAdd = Dict.empty
    , userKeys = Dict.empty
    , vaultFingerprints = NotAsked
    }


initForVault : Vault -> State
initForVault vault =
    let
        default =
            init

        name =
            nameOrId vault

        nameInput =
            Ui.Input.init ()
                |> Ui.Input.placeholder name
                |> Ui.Input.showClearIcon True

        cloneStatus =
            if vault.id == "" then
                New
            else
                Cloned

        folderPath =
            if cloneStatus == New then
                Nothing
            else
                Just (asPath vault.folderPath)
    in
        { default
            | id = vault.id
            , icon = vault.icon
            , cloneStatus = cloneStatus
            , title = name
            , nameInput = nameInput
            , localFolderPath = folderPath
        }


initForFlyingVault : FlyingVault -> State
initForFlyingVault flyingVault =
    let
        default =
            initForVault (Syncrypt.Vault.asVault flyingVault)

        name =
            nameOrId flyingVault

        nameInput =
            Ui.Input.init ()
                |> Ui.Input.placeholder name
                |> Ui.Input.showClearIcon True
    in
        { default
            | id = flyingVault.id
            , icon = flyingVault.icon
            , cloneStatus = NotCloned
            , title = name
            , nameInput = nameInput
            , localFolderPath = Nothing
        }


sortedFolders : State -> List FolderItem
sortedFolders { localFolderItems } =
    localFolderItems
        |> Dict.toList
        |> List.sortBy (\( k, v ) -> k)


isIgnored : Path -> State -> Bool
isIgnored path { ignoredFolderItems } =
    (Set.member path ignoredFolderItems)
        || (ignoredFolderItems
                |> Set.filter (\p -> (List.take (List.length p) path) == p)
                |> Set.isEmpty
                |> not
           )


isUserKeyPending : User.Email -> User.UserKey -> State -> Bool
isUserKeyPending email userKey state =
    case Dict.get email state.usersToAdd of
        Nothing ->
            False

        Just keys ->
            List.member userKey keys


isUserKeyAlreadyAdded : User.UserKey -> State -> Bool
isUserKeyAlreadyAdded userKey state =
    case state.vaultFingerprints of
        Success fingerprints ->
            Set.member userKey.fingerprint fingerprints

        _ ->
            False


isUserKeySelected : User.Email -> User.UserKey -> State -> Bool
isUserKeySelected email userKey state =
    isUserKeyPending email userKey state
        || isUserKeyAlreadyAdded userKey state


isExpanded : Path -> State -> Bool
isExpanded path ({ expandedFolders } as state) =
    Set.member path expandedFolders


collapseFolder : Path -> State -> State
collapseFolder path ({ expandedFolders } as state) =
    { state | expandedFolders = Set.remove path expandedFolders }


expandFolder : Path -> State -> State
expandFolder path ({ expandedFolders } as state) =
    { state | expandedFolders = Set.insert path expandedFolders }


addFolder : FolderItem -> State -> State
addFolder (( path, files ) as f) ({ localFolderItems } as state) =
    let
        addPathToLocalItems path files items =
            let
                isRoot =
                    path == []
            in
                case ( Path.parent path, Dict.get path items ) of
                    ( [], Nothing ) ->
                        items
                            |> Dict.insert path files

                    ( [], Just existingFiles ) ->
                        items
                            |> Dict.insert path (files ++ existingFiles)

                    ( pp, Nothing ) ->
                        items
                            |> Dict.insert path files
                            |> addPathToLocalItems pp []

                    ( pp, Just existingFiles ) ->
                        items
                            |> Dict.insert path (files ++ existingFiles)
                            |> addPathToLocalItems pp []
    in
        { state
            | localFolderItems = addPathToLocalItems path files localFolderItems
        }


toggleIgnorePath : Path -> State -> State
toggleIgnorePath path ({ ignoredFolderItems } as model) =
    case Set.member path model.ignoredFolderItems of
        True ->
            { model
                | ignoredFolderItems = Set.remove path ignoredFolderItems
            }

        False ->
            { model
                | ignoredFolderItems = Set.insert path ignoredFolderItems
            }


folderIsEmpty : Path -> State -> Bool
folderIsEmpty path state =
    case Dict.get path state.localFolderItems of
        Nothing ->
            True

        Just files ->
            List.isEmpty files


toggleUserKey : User.Email -> User.UserKey -> State -> State
toggleUserKey email key state =
    case Dict.get email state.usersToAdd of
        Nothing ->
            { state | usersToAdd = Dict.insert email [ key ] state.usersToAdd }

        Just keys ->
            let
                userKeysSelected =
                    if List.member key keys then
                        List.filter (\k -> k /= key) keys
                    else
                        keys ++ [ key ]
            in
                { state | usersToAdd = Dict.insert email userKeysSelected state.usersToAdd }


userInputEmail : State -> User.Email
userInputEmail state =
    state.userInput.value


keysToAdd : User.Email -> State -> List User.UserKey
keysToAdd email state =
    Dict.get email state.usersToAdd
        |> Maybe.withDefault []


userKeys : User.Email -> State -> WebData (List User.UserKey)
userKeys email state =
    Dict.get email state.userKeys
        |> Maybe.withDefault NotAsked


hasChanged : State -> State
hasChanged state =
    { state | hasChangesPending = True }
