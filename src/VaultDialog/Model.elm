module VaultDialog.Model exposing (..)

import Dict exposing (Dict)
import Ui.Checkbox
import Ui.Input
import Ui.Modal
import Set exposing (Set)
import Util
import Syncrypt.Vault exposing (Vault, VaultId, nameOrId)


type alias FileName =
    String


type alias Path =
    List String


type alias FolderItem =
    ( Path, List FileName )


type alias State =
    { id : VaultId
    , title : String
    , modal : Ui.Modal.Model
    , nameInput : Ui.Input.Model
    , localFolderPath : Maybe Path
    , localFolderItems : Dict Path (List String)
    , ignoredFolderItems : Set Path
    }


type Msg
    = Modal Ui.Modal.Msg
    | NameInput Ui.Input.Msg
    | FileCheckBox Path Ui.Checkbox.Msg
    | NestedFileList Path FolderItem
    | ToggleIgnorePath Path
    | OpenFolderDialog VaultId
    | SelectedFolder Path


init : State
init =
    { id = ""
    , title = "Untitled Vault"
    , ignoredFolderItems = Set.fromList [ [ ".DS_Store" ], [ ".vault" ] ]
    , localFolderPath = Nothing
    , localFolderItems = Dict.empty
    , modal =
        Ui.Modal.init
            |> Ui.Modal.closable True
            |> Ui.Modal.backdrop False
    , nameInput =
        Ui.Input.init ()
            |> Ui.Input.placeholder "Vault Name"
            |> Ui.Input.showClearIcon True
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


addFolder : FolderItem -> State -> State
addFolder (( path, files ) as f) ({ localFolderItems } as state) =
    let
        addPathToLocalItems path files items =
            let
                isRoot =
                    path == []

                parentPath =
                    path
                        |> List.reverse
                        |> List.drop 1
                        |> List.reverse
            in
                case ( parentPath, Dict.get path items ) of
                    ( [], Nothing ) ->
                        items
                            |> Dict.insert path files

                    ( [], Just existingFiles ) ->
                        items
                            |> Dict.insert path (files ++ existingFiles)

                    ( _, Nothing ) ->
                        items
                            |> Dict.insert path files
                            |> addPathToLocalItems parentPath []

                    ( _, Just existingFiles ) ->
                        items
                            |> Dict.insert path (files ++ existingFiles)
                            |> addPathToLocalItems parentPath []
    in
        { state | localFolderItems = addPathToLocalItems path files localFolderItems }


toggleIgnorePath : Path -> State -> State
toggleIgnorePath path ({ ignoredFolderItems } as model) =
    case Set.member path model.ignoredFolderItems of
        True ->
            { model | ignoredFolderItems = Set.remove path ignoredFolderItems }

        False ->
            { model | ignoredFolderItems = Set.insert path ignoredFolderItems }


folderName : Path -> String
folderName path =
    case path of
        [] ->
            "./"

        [ x ] ->
            x

        _ :: rest ->
            folderName rest


name : Path -> String
name path =
    case path of
        [] ->
            "."

        [ f ] ->
            f

        _ :: rest ->
            name rest


asPath : String -> Path
asPath pathString =
    if String.startsWith "/" pathString then
        String.split ("/") pathString
    else
        String.split "\\" pathString


inRoot : Path -> Bool
inRoot path =
    List.length path == 1


parentPath : Path -> Path
parentPath path =
    path
        |> Util.allButLast
