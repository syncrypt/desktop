module VaultCreationDialog.Model exposing (..)

import Ui.Modal
import Ui.Input
import Ui.Checkbox


type alias Path =
    String


type alias JSFolderContent =
    { isDir : Bool, path : Path }


type FolderContent
    = File Path
    | Folder Path


type alias State =
    { title : String
    , modal : Ui.Modal.Model
    , nameInput : Ui.Input.Model
    , localFolderPath : Maybe String
    , localFolderContents : Maybe (List FolderContent)
    , ignoreFiles : List FolderContent
    }


type Msg
    = Modal Ui.Modal.Msg
    | NameInput Ui.Input.Msg
    | FileList String (List FolderContent)
    | FileCheckBox FolderContent Ui.Checkbox.Msg


init : State
init =
    { title = "Untitled Vault"
    , ignoreFiles = []
    , localFolderPath = Just "/tmp/foo"
    , localFolderContents =
        Just
            [ Folder "Folder 1"
            , File "File 1"
            , File "File 2"
            , File "File 3"
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


parseFolderContents : List JSFolderContent -> List FolderContent
parseFolderContents =
    List.map parseFolderContent


parseFolderContent : JSFolderContent -> FolderContent
parseFolderContent fc =
    if fc.isDir then
        File fc.path
    else
        Folder fc.path


isIgnored : FolderContent -> State -> Bool
isIgnored fc state =
    List.member fc state.ignoreFiles
