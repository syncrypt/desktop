port module VaultCreationDialog exposing (..)

import Html exposing (Html, button, div, form, input, label, span, text)
import Html.Attributes exposing (class, for, id)
import Html.Events exposing (onClick)
import Model exposing (Model)
import Ui.Checkbox
import Ui.Container
import Ui.Input
import Ui.Modal
import Dialog exposing (labeledRight)
import VaultCreationDialog.Model
    exposing
        ( parseFolderItems
        , FolderItem(..)
        , JSFolderItem
        , State
        , Msg(Modal, NameInput, FileList, FileCheckBox, FolderItemToggle)
        , isIgnored
        )


port getFileList : String -> Cmd msg


port fileList : (( String, List JSFolderItem ) -> msg) -> Sub msg


subscriptions : Sub Model.Msg
subscriptions =
    let
        fileListMsg ( path, files ) =
            FileList path (parseFolderItems files)
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
        items =
            case vaultCreationDialog.localFolderItems of
                Nothing ->
                    []

                Just items ->
                    folderItems items vaultCreationDialog

        settings =
            { direction = "column"
            , compact = False
            , align = "start"
            }
    in
        Ui.Container.view settings [] items


folderItems : List FolderItem -> State -> List (Html Model.Msg)
folderItems items state =
    List.map
        (\fi -> folderItem fi state)
        items


folderItem : FolderItem -> State -> Html Model.Msg
folderItem fi state =
    let
        checkbox =
            fileCheckbox fi state

        item =
            checkbox
                |> labeledRight
                    [ onClick (Model.VaultCreationDialog (FolderItemToggle fi)) ]
                    (name fi)

        childItems =
            folderChildItems fi state
    in
        div [ class "VaultCreationDialog-FolderItem" ]
            (item :: childItems)


folderChildItems : FolderItem -> State -> List (Html Model.Msg)
folderChildItems fi state =
    case fi of
        Folder path children ->
            let
                className =
                    if isIgnored fi state then
                        "VaultCreationDialog-FolderChildItem-Hidden"
                    else
                        "VaultCreationDialog-FolderChildItem"
            in
                [ div [ class className ]
                    (folderItems children state)
                ]

        _ ->
            []


fileCheckbox : FolderItem -> State -> Html Model.Msg
fileCheckbox fi state =
    let
        checkbox =
            Ui.Checkbox.view (fileCheckboxSettings fi state)
                |> Html.map (FileCheckBox fi >> Model.VaultCreationDialog)
    in
        div [ class "VaultCreationDialog-FolderItem-Checkbox" ]
            [ checkbox ]


fileCheckboxSettings : FolderItem -> State -> Ui.Checkbox.Model
fileCheckboxSettings fi state =
    { disabled = False
    , readonly = False
    , value = not (isIgnored fi state)
    , uid = name fi
    }


name : FolderItem -> String
name fi =
    case fi of
        File name ->
            name

        Folder name _ ->
            name
