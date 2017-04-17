port module VaultCreationDialog exposing (..)

import Html exposing (Html, button, div, form, input, label, span, text)
import Html.Attributes exposing (class)
import Ui.Modal
import Ui.Input
import Ui.Container
import Ui.Checkbox
import VaultCreationDialog.Model
    exposing
        ( parseFolderContents
        , FolderContent(..)
        , JSFolderContent
        , State
        , Msg(Modal, NameInput, FileList, FileCheckBox)
        , isIgnored
        )
import Model exposing (Model)


port getFileList : String -> Cmd msg


port fileList : (( String, List JSFolderContent ) -> msg) -> Sub msg


subscriptions : Sub Model.Msg
subscriptions =
    let
        fileListMsg ( path, files ) =
            FileList path (parseFolderContents files)
    in
        fileList fileListMsg
            |> Sub.map Model.VaultCreationDialog


view : Model -> Html Model.Msg
view model =
    let
        viewConfig =
            { address = (Modal >> Model.VaultCreationDialog)
            , contents = contents model
            , footer = []
            , title = "Create New Vault"
            }
    in
        Ui.Modal.view viewConfig model.vaultCreationDialog.modal


contents : Model -> List (Html Model.Msg)
contents ({ vaultCreationDialog } as model) =
    [ div [ class "VaultCreationDialog-Content" ]
        [ nameInput vaultCreationDialog
        , fileSelectionContainer model
        ]
    ]


nameInput : State -> Html Model.Msg
nameInput { nameInput } =
    Ui.Input.view nameInput
        |> Html.map (NameInput >> Model.VaultCreationDialog)


fileSelectionContainer : Model -> Html Model.Msg
fileSelectionContainer { vaultCreationDialog } =
    let
        contents =
            case vaultCreationDialog.localFolderContents of
                Nothing ->
                    []

                Just contents ->
                    folderContents contents vaultCreationDialog

        settings =
            { direction = "column"
            , compact = False
            , align = "start"
            }
    in
        Ui.Container.view settings [] contents


folderContents : List FolderContent -> State -> List (Html Model.Msg)
folderContents contents state =
    List.map
        (\fc -> folderContent fc state)
        contents


folderContent : FolderContent -> State -> Html Model.Msg
folderContent fc state =
    div [ class "VaultCreationDialog-FolderContent" ]
        [ div [ class "VaultCreationDialog-FolderContent-Name" ]
            [ fileCheckbox fc state
            , text (name fc)
            , div [ class "VaultCreationDialog-FolderContentWithChildren" ]
                (folderChildContents fc state)
            ]
        ]


folderChildContents : FolderContent -> State -> List (Html Model.Msg)
folderChildContents fc state =
    case fc of
        Folder path children ->
            if isIgnored fc state then
                []
            else
                folderContents children state

        _ ->
            []


fileCheckbox : FolderContent -> State -> Html Model.Msg
fileCheckbox fc state =
    let
        checkbox =
            Ui.Checkbox.view (fileCheckboxSettings fc state)
                |> Html.map (FileCheckBox fc >> Model.VaultCreationDialog)
    in
        div [ class "VaultCreationDialog-FolderContent-Checkbox" ]
            [ checkbox ]


fileCheckboxSettings : FolderContent -> State -> Ui.Checkbox.Model
fileCheckboxSettings fc state =
    { disabled = False
    , readonly = False
    , value = not (isIgnored fc state)
    , uid = name fc
    }


name : FolderContent -> String
name fc =
    case fc of
        File name ->
            name

        Folder name _ ->
            name
