module VaultDialog.Model exposing (..)

import Dict exposing (Dict)
import Ui.Checkbox
import Ui.Input
import Ui.Modal
import Ui.Tabs
import Set exposing (Set)
import Path exposing (Path, asPath)
import Syncrypt.Vault exposing (Vault, VaultId, nameOrId)
import ConfirmationDialog


type alias FileName =
    String


type alias FolderItem =
    ( Path, List FileName )


type alias State =
    { id : VaultId
    , isNew : Bool
    , title : String
    , modal : Ui.Modal.Model
    , confirmationDialog : ConfirmationDialog.Model Msg
    , nameInput : Ui.Input.Model
    , tabs : Ui.Tabs.Model
    , localFolderPath : Maybe Path
    , localFolderItems : Dict Path (List String)
    , ignoredFolderItems : Set Path
    , expandedFolders : Set Path
    }


type Msg
    = Modal Ui.Modal.Msg
    | ConfirmationDialog ConfirmationDialog.Msg
    | NameInput Ui.Input.Msg
    | FileCheckBox Path Ui.Checkbox.Msg
    | NestedFileList Path FolderItem
    | ToggleIgnorePath Path
    | OpenFolderDialog VaultId
    | SelectedFolder Path
    | Tabs Ui.Tabs.Msg
    | CollapseFolder Path
    | ExpandFolder Path
    | AskDeleteVault
    | ConfirmedVaultDeletion


init : State
init =
    { id = ""
    , isNew = True
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
    , tabs =
        Ui.Tabs.init ()
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
    in
        { default
            | id = vault.id
            , isNew = False
            , title = name
            , nameInput = nameInput
            , localFolderPath = Just (asPath vault.folderPath)
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
