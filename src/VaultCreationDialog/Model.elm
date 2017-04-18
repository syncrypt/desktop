module VaultCreationDialog.Model exposing (..)

import Ui.Modal
import Ui.Input
import Ui.Checkbox


type alias Path =
    String


type alias JSFolderItem =
    { isDir : Bool, path : Path }


type FolderItem
    = File Path
    | Folder Path (List FolderItem)


type alias State =
    { title : String
    , modal : Ui.Modal.Model
    , nameInput : Ui.Input.Model
    , localFolderPath : Maybe String
    , localFolderItems : Maybe (List FolderItem)
    , ignoreFiles : List FolderItem
    }


type Msg
    = Modal Ui.Modal.Msg
    | NameInput Ui.Input.Msg
    | FileList Path (List FolderItem)
    | FileCheckBox FolderItem Ui.Checkbox.Msg
    | NestedFileList FolderItem (List FolderItem)


init : State
init =
    { title = "Untitled Vault"
    , ignoreFiles = []
    , localFolderPath = Just "/tmp/foo"
    , localFolderItems =
        -- TODO: load these from file system
        Just
            [ Folder "Research" []
            , Folder "Music"
                [ File "Song 1.mp3"
                , File "Song 2.mp3"
                , Folder "Pictures"
                    [ File "Party.jpg"
                    , File "Logo.png"
                    ]
                ]
            , File "Docs"
            ]
    , modal =
        Ui.Modal.init
            |> Ui.Modal.closable True
            |> Ui.Modal.backdrop False
    , nameInput =
        Ui.Input.init ()
            |> Ui.Input.placeholder "Vault Name"
            |> Ui.Input.showClearIcon True
    }


parseFolderItems : List JSFolderItem -> List FolderItem
parseFolderItems =
    List.map parseFolderItem


parseFolderItem : JSFolderItem -> FolderItem
parseFolderItem { isDir, path } =
    if isDir then
        File path
    else
        Folder path []


parseFolderWithChildItems : Path -> List JSFolderItem -> FolderItem
parseFolderWithChildItems path folderItems =
    Folder path (parseFolderItems folderItems)


isIgnored : FolderItem -> State -> Bool
isIgnored fi state =
    List.member fi state.ignoreFiles


addNestedFolderItems : FolderItem -> List FolderItem -> State -> State
addNestedFolderItems parent children ({ localFolderItems } as state) =
    let
        appendChildrenToFolder : FolderItem -> FolderItem
        appendChildrenToFolder fi =
            case ( fi, parent ) of
                ( Folder path1 children1, Folder path2 children2 ) ->
                    if path1 == path2 then
                        Folder path1 (children1 ++ children2)
                    else
                        fi

                _ ->
                    fi

        items =
            case localFolderItems of
                Nothing ->
                    children

                Just items ->
                    items
                        |> List.map appendChildrenToFolder
    in
        { state | localFolderItems = Just items }
