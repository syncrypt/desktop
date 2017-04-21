module VaultCreationDialog.Model exposing (..)

import Dict exposing (Dict)
import Ui.Checkbox
import Ui.Input
import Ui.Modal
import Set exposing (Set)


type alias Path =
    String


type alias FolderPath =
    List Path


type alias FolderFiles =
    List Path


type alias FolderItem =
    ( FolderPath, List Path )


type alias State =
    { title : String
    , modal : Ui.Modal.Model
    , nameInput : Ui.Input.Model
    , localFolderPath : Maybe FolderPath
    , localFolderItems : Dict FolderPath (List Path)
    , ignoredFolderItems : Set (List Path)
    }


type Msg
    = Modal Ui.Modal.Msg
    | NameInput Ui.Input.Msg
    | FileCheckBox (List Path) Ui.Checkbox.Msg
    | NestedFileList FolderPath FolderItem
    | ToggleIgnorePath (List Path)
    | OpenFolderDialog
    | SelectedFolder FolderPath


init : State
init =
    { title = "Untitled Vault"
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


findFirst : (a -> Bool) -> List a -> Maybe a
findFirst check list =
    case list of
        [] ->
            Nothing

        val :: rest ->
            if check val then
                Just val
            else
                findFirst check rest


sortedFolders : State -> List FolderItem
sortedFolders { localFolderItems } =
    localFolderItems
        |> Dict.toList
        |> List.sortBy (\( k, v ) -> k)


isIgnored : List Path -> State -> Bool
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


toggleIgnorePath : FolderPath -> State -> State
toggleIgnorePath path ({ ignoredFolderItems } as model) =
    case Set.member path model.ignoredFolderItems of
        True ->
            { model | ignoredFolderItems = Set.remove path ignoredFolderItems }

        False ->
            { model | ignoredFolderItems = Set.insert path ignoredFolderItems }


folderName : FolderPath -> String
folderName path =
    case path of
        [] ->
            "./"

        [ x ] ->
            x

        _ :: rest ->
            folderName rest


name : List Path -> String
name path =
    case path of
        [] ->
            "."

        [ f ] ->
            f

        _ :: rest ->
            name rest


inRoot : FolderPath -> Bool
inRoot path =
    List.length path == 1


parentPath path =
    case path of
        [] ->
            []

        [ x ] ->
            []

        x :: rest ->
            parentPath rest
