module VaultDialog.Model exposing (..)

import Dict exposing (Dict)
import Ui.Checkbox
import Ui.Input
import Ui.Modal
import Ui.Tabs
import Set exposing (Set)
import Path exposing (Path, asPath)
import Syncrypt.Vault exposing (Vault, VaultId, nameOrId)
import Syncrypt.User as User
import ConfirmationDialog
import Http


type alias FileName =
    String


type alias FolderItem =
    ( Path, List FileName )


type alias State =
    { id : VaultId
    , isNew : Bool
    , hasChangesPending : Bool
    , title : String
    , modal : Ui.Modal.Model
    , confirmationDialog : ConfirmationDialog.Model Msg
    , nameInput : Ui.Input.Model
    , userInput : Ui.Input.Model
    , tabs : Ui.Tabs.Model
    , localFolderPath : Maybe Path
    , localFolderItems : Dict Path (List String)
    , ignoredFolderItems : Set Path
    , expandedFolders : Set Path
    , users : List User.User
    , usersToAdd : Dict User.Email (List User.UserKey)
    , userKeys : Dict User.Email (List User.UserKey)
    , vaultFingerprints : Set User.Fingerprint
    }


type RequiresConfirmation
    = DeleteVault
    | RemoveVault
    | AddUser


type Msg
    = Modal Ui.Modal.Msg
    | ConfirmationDialog ConfirmationDialog.Msg
    | NameChanged
    | NameInput Ui.Input.Msg
    | UserInput Ui.Input.Msg
    | FileCheckBox Path Ui.Checkbox.Msg
    | NestedFileList Path FolderItem
    | ToggleIgnorePath Path
    | OpenFolderDialog VaultId
    | OpenFolder String
    | SelectedFolder Path
    | Tabs Ui.Tabs.Msg
    | CollapseFolder Path
    | ExpandFolder Path
    | Confirm RequiresConfirmation
    | Confirmed RequiresConfirmation
    | AddUserWithKeys User.Email (List User.UserKey)
    | ToggleUserKey User.Email User.UserKey
    | UserKeyCheckbox User.Email User.UserKey Ui.Checkbox.Msg
    | SearchUserKeys User.Email
    | FoundUserKeys User.Email (Result Http.Error (List User.UserKey))
    | FoundVaultFingerprints (Result Http.Error (List User.Fingerprint))
    | FetchedUsers (Result Http.Error (List User.User))
    | SetUserInput String


init : State
init =
    { id = ""
    , isNew = True
    , hasChangesPending = False
    , title = "Untitled Vault"
    , ignoredFolderItems = Set.fromList [ [ ".DS_Store" ], [ ".vault" ] ]
    , localFolderPath = Nothing
    , localFolderItems = Dict.empty
    , expandedFolders = Set.fromList [ [] ]
    , modal =
        Ui.Modal.init
            |> Ui.Modal.closable False
            |> Ui.Modal.backdrop False
    , confirmationDialog =
        ConfirmationDialog.init ConfirmationDialog
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
    , users = []
    , usersToAdd = Dict.empty
    , userKeys = Dict.empty
    , vaultFingerprints = Set.empty
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

        isNew =
            vault.id == ""

        folderPath =
            if isNew then
                Nothing
            else
                Just (asPath vault.folderPath)
    in
        { default
            | id = vault.id
            , isNew = isNew
            , title = name
            , nameInput = nameInput
            , localFolderPath = folderPath
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


isUserKeyPending email userKey state =
    case Dict.get email state.usersToAdd of
        Nothing ->
            False

        Just keys ->
            List.member userKey keys


isUserKeyAlreadyAdded userKey state =
    Set.member userKey.fingerprint state.vaultFingerprints


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


userKeys : User.Email -> State -> List User.UserKey
userKeys email state =
    Dict.get email state.userKeys
        |> Maybe.withDefault []


hasChanged : State -> State
hasChanged state =
    { state | hasChangesPending = True }
