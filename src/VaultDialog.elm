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
import Ui.Tabs
import VaultDialog.Model exposing (FileName, FolderItem, Msg(..), Path, State, folderName, inRoot, isIgnored, name, sortedFolders)
import VaultDialog.Update exposing (dialogState)
import Syncrypt.Vault exposing (VaultId)
import VaultDialog.Ports
import Dict


subscriptions : Model -> Sub Model.Msg
subscriptions _ =
    let
        fileListMsg ( vaultId, rootPath, folderItem ) =
            Model.VaultDialog vaultId (NestedFileList rootPath folderItem)

        selectedFolderMsg ( vaultId, path ) =
            Model.VaultDialog vaultId (SelectedFolder path)
    in
        Sub.batch
            [ VaultDialog.Ports.fileList fileListMsg
            , VaultDialog.Ports.selectedFolder selectedFolderMsg
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
            , title =
                case vaultId of
                    "" ->
                        "Create New Vault"

                    id ->
                        "Vault " ++ id
            }

        state =
            dialogState vaultId model
    in
        Ui.Modal.view viewConfig state.modal


contents : VaultId -> Model -> List (Html Model.Msg)
contents vaultId model =
    let
        state =
            dialogState vaultId model

        tabsViewConfig =
            { address = (Tabs >> Model.VaultDialog vaultId)
            , contents = tabContents vaultId state model
            }

        html =
            [ Ui.Tabs.view tabsViewConfig state.tabs
            , div [ class "VaultDialog-Buttons" ]
                [ saveButton vaultId
                , cancelButton vaultId
                ]
            ]
    in
        html


tabContents : VaultId -> State -> Model -> List ( String, Html Model.Msg )
tabContents vaultId state model =
    let
        -- converter from Msg -> Model.Msg
        msg =
            Html.map (Model.VaultDialog vaultId)
    in
        [ ( "Basic"
          , div [ class "VaultDialog-Tab-Content" ]
                [ msg <| nameInput state
                , msg <| openFolderButton vaultId
                , msg <| fileSelectionContainer state
                ]
          )
        , ( "Users"
          , div [ class "VaultDialog-Tab-Content" ]
                [ msg <| text "Vault Users:"
                ]
          )
        ]


cancelButton : VaultId -> Html Model.Msg
cancelButton vaultId =
    span [ class "VaultDialog-CancelButton" ]
        [ Ui.Button.model "Cancel" "secondary" "medium"
            |> Ui.Button.view (Model.CloseVaultDetails vaultId)
        ]


saveButton : VaultId -> Html Model.Msg
saveButton vaultId =
    span [ class "VaultDialog-SaveButton" ]
        [ Ui.Button.model "Save" "primary" "medium"
            |> Ui.Button.view (Model.SaveVaultDetails vaultId)
        ]


openFolderButton : VaultId -> Html Msg
openFolderButton vaultId =
    Ui.Button.model "Select Folder" "primary" "medium"
        |> Ui.Button.view (OpenFolderDialog vaultId)


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
    div [ class "VaultDialog-FolderItem" ] <|
        (inFolderPath path
            [ span [] [ fileCheckbox path state ]
            , div (hiddenIfIgnored path state [])
                [ div [ class "VaultDialog-FolderItem-Nested" ]
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
        div [ class "VaultDialog-File" ] <|
            [ fileCheckbox filePath state
            ]


hiddenIfIgnored : Path -> State -> List (Html.Attribute msg) -> List (Html.Attribute msg)
hiddenIfIgnored path state attributes =
    if isIgnored path state then
        (class "VaultDialog-FolderItem-Hidden") :: attributes
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
            [ div [ class "VaultDialog-FolderItem-Nested" ]
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
        span [ class "VaultDialog-FolderItem-Checkbox" ]
            [ checkboxWithLabel ]


fileCheckboxSettings : Path -> State -> Ui.Checkbox.Model
fileCheckboxSettings path state =
    { disabled = False
    , readonly = False
    , value = not (isIgnored path state)
    , uid = name path
    }
