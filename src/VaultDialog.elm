module VaultDialog exposing (..)

import Dialog exposing (labeledRight)
import Html exposing (Html, button, div, form, input, label, span, text)
import Html.Attributes exposing (class, for, id, style)
import Html.Events exposing (onClick)
import Model exposing (Model)
import Ui.Button
import Ui.Checkbox
import Ui.Container
import Ui.Input
import Ui.Modal
import VaultDialog.Model exposing (FileName, FolderItem, Msg(..), Path, State, folderName, inRoot, isIgnored, name, sortedFolders)
import VaultDialog.Update exposing (dialogState)
import Syncrypt.Vault exposing (VaultId)
import VaultDialog.Ports
import Dict


subscriptions : VaultId -> Sub Model.Msg
subscriptions vaultId =
    let
        fileListMsg ( rootPath, folderItem ) =
            NestedFileList rootPath folderItem
    in
        Sub.batch
            [ VaultDialog.Ports.fileList fileListMsg
                |> Sub.map (Model.VaultDialog vaultId)
            , VaultDialog.Ports.selectedFolder SelectedFolder
                |> Sub.map (Model.VaultDialog vaultId)
            ]


viewAll : Model -> List (Html Model.Msg)
viewAll ({ vaultDialogs } as model) =
    vaultDialogs
        |> Dict.keys
        |> List.map (\vaultId -> view vaultId model)


view : VaultId -> Model -> Html Model.Msg
view vaultId model =
    let
        viewConfig =
            { address = (Modal >> Model.VaultDialog vaultId)
            , contents = contents vaultId model
            , footer = []
            , title = "Create New Vault"
            }
    in
        Ui.Modal.view viewConfig (dialogState vaultId model).modal


contents : VaultId -> Model -> List (Html Model.Msg)
contents vaultId model =
    let
        state =
            dialogState vaultId model

        html =
            [ div [ class "VaultCreationDialog-Content" ]
                [ nameInput state
                , openFolderButton
                , fileSelectionContainer state
                ]
            ]
    in
        List.map (Html.map (Model.VaultDialog vaultId)) html


openFolderButton : Html Msg
openFolderButton =
    Ui.Button.model "Select Folder" "primary" "medium"
        |> Ui.Button.view OpenFolderDialog


nameInput : State -> Html Msg
nameInput state =
    Ui.Input.view state.nameInput
        |> Html.map NameInput


fileSelectionContainer : State -> Html Msg
fileSelectionContainer state =
    let
        settings =
            { direction = "column"
            , compact = False
            , align = "start"
            }
    in
        Ui.Container.view settings [] (renderFolders state)


renderFolders : State -> List (Html Msg)
renderFolders state =
    case sortedFolders state of
        ( _, rootFileNames ) :: folders ->
            let
                rootFiles =
                    List.map (renderFile state []) rootFileNames

                rootFolders =
                    List.map (renderFolder state) folders
            in
                [ div []
                    (List.foldr (::)
                        []
                        (rootFiles ++ rootFolders)
                    )
                ]

        [] ->
            []


renderFolder : State -> FolderItem -> Html Msg
renderFolder state (( path, files ) as fi) =
    div [ class "VaultCreationDialog-FolderItem" ] <|
        (inFolderPath path
            [ span [] [ fileCheckbox path state ]
            , div (hiddenIfIgnored path state [])
                [ div [ class "VaultCreationDialog-FolderItem-Nested" ]
                    (List.map (renderFile state path) files)
                ]
            ]
        )


renderFile : State -> Path -> FileName -> Html Msg
renderFile state folderPath path =
    let
        filePath =
            folderPath ++ [ path ]
    in
        div [ class "VaultCreationDialog-File" ] <|
            [ fileCheckbox filePath state
            ]


hiddenIfIgnored : Path -> State -> List (Html.Attribute msg) -> List (Html.Attribute msg)
hiddenIfIgnored path state attributes =
    if isIgnored path state then
        (class "VaultCreationDialog-FolderItem-Hidden") :: attributes
    else
        attributes


inFolderPath : Path -> List (Html Msg) -> List (Html Msg)
inFolderPath path contents =
    case path of
        [] ->
            []

        [ p ] ->
            contents

        x :: rest ->
            [ div [ class "VaultCreationDialog-FolderItem-Nested" ]
                (inFolderPath rest contents)
            ]


fileCheckbox : Path -> State -> Html Msg
fileCheckbox path state =
    let
        checkbox =
            Ui.Checkbox.view (fileCheckboxSettings path state)
                |> Html.map (FileCheckBox path)

        checkboxWithLabel =
            checkbox
                |> labeledRight
                    [ onClick (ToggleIgnorePath path) ]
                    (folderName path)
    in
        span [ class "VaultCreationDialog-FolderItem-Checkbox" ]
            [ checkboxWithLabel ]


fileCheckboxSettings : Path -> State -> Ui.Checkbox.Model
fileCheckboxSettings path state =
    { disabled = False
    , readonly = False
    , value = not (isIgnored path state)
    , uid = name path
    }
