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
    Sub.map Model.VaultCreationDialog
        (fileList (\( path, files ) -> FileList path (parseFolderContents files)))


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
    in
        Ui.Container.view { direction = "column", compact = False, align = "start" }
            []
            contents


folderContents contents state =
    List.map
        (\fc -> folderContent fc (isIgnored fc state))
        contents


folderContent : FolderContent -> Bool -> Html Model.Msg
folderContent fc isIgnored =
    div [ class "VaultCreationDialog-FolderContent" ]
        [ checkbox fc isIgnored
        , div [ class "VaultCreationDialog-FolderContent-Name" ]
            [ text (name fc) ]
        ]


checkbox : FolderContent -> Bool -> Html Model.Msg
checkbox fc isIgnored =
    div [ class "VaultCreationDialog-FolderContent-Checkbox" ]
        [ Ui.Checkbox.view
            { disabled = False, readonly = False, value = not isIgnored, uid = name fc }
            |> Html.map (FileCheckBox fc >> Model.VaultCreationDialog)
        ]


name fc =
    case fc of
        File name ->
            name

        Folder name ->
            name
