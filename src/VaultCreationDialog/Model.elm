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
    | Folder Path (List FolderContent)


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
    | FileList Path (List FolderContent)
    | FileCheckBox FolderContent Ui.Checkbox.Msg
    | NestedFileList FolderContent (List FolderContent)


init : State
init =
    { title = "Untitled Vault"
    , ignoreFiles = []
    , localFolderPath = Just "/tmp/foo"
    , localFolderContents =
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


parseFolderContents : List JSFolderContent -> List FolderContent
parseFolderContents =
    List.map parseFolderContent


parseFolderContent : JSFolderContent -> FolderContent
parseFolderContent { isDir, path } =
    if isDir then
        File path
    else
        Folder path []


parseFolderWithChildContents : Path -> List JSFolderContent -> FolderContent
parseFolderWithChildContents path folderContents =
    Folder path (parseFolderContents folderContents)


isIgnored : FolderContent -> State -> Bool
isIgnored fc state =
    List.member fc state.ignoreFiles


addNestedFolderContents : FolderContent -> List FolderContent -> State -> State
addNestedFolderContents parent children ({ localFolderContents } as state) =
    let
        appendChildrenToFolder : FolderContent -> FolderContent
        appendChildrenToFolder fc =
            case ( fc, parent ) of
                ( Folder path1 children1, Folder path2 children2 ) ->
                    if path1 == path2 then
                        Folder path1 (children1 ++ children2)
                    else
                        fc

                _ ->
                    fc

        contents =
            case localFolderContents of
                Nothing ->
                    children

                Just contents ->
                    contents
                        |> List.map appendChildrenToFolder
    in
        { state | localFolderContents = Just contents }
