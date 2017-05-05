module VaultDialog exposing (..)

import Dialog exposing (labeledLeft, labeledRight)
import Html exposing (Html, button, div, form, input, label, span, text)
import Html.Attributes exposing (class, classList, for, id, style)
import Html.Events exposing (onClick)
import Model exposing (Model)
import Ui.Button
import Ui.Checkbox
import Ui.Container
import Ui.Input
import Ui.Modal
import Ui.Tabs
import VaultDialog.Model
    exposing
        ( FileName
        , FolderItem
        , Msg(..)
        , State
        , isIgnored
        , isExpanded
        , sortedFolders
        , folderIsEmpty
        )
import Path exposing (Path)
import VaultDialog.Update exposing (dialogState)
import Syncrypt.Vault exposing (VaultId)
import VaultDialog.Ports
import Dict
import ConfirmationDialog


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
            { address = (Model.VaultDialog vaultId << Modal)
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
        div [ class "VaultDialog" ]
            [ Ui.Modal.view viewConfig state.modal
            ]


contents : VaultId -> Model -> List (Html Model.Msg)
contents vaultId model =
    let
        state =
            dialogState vaultId model

        tabsViewConfig =
            { address = (Model.VaultDialog vaultId << Tabs)
            , contents = tabContents vaultId state model
            }
    in
        [ Ui.Tabs.view tabsViewConfig state.tabs
        , ConfirmationDialog.view state
            |> Html.map (Model.VaultDialog state.id)
        , div [ class "VaultDialog-Buttons" ]
            [ deleteButton state vaultId
            , saveButton vaultId
            , cancelButton vaultId
            ]
        ]


tabContents : VaultId -> State -> Model -> List ( String, Html Model.Msg )
tabContents vaultId state model =
    let
        -- converter from Msg -> Model.Msg
        msg =
            Html.map (Model.VaultDialog vaultId)
    in
        [ ( "Basic"
          , div [ class "VaultDialog-Tab-Content" ]
                [ dialogInput <| nameInput vaultId state
                , dialogInput <| msg <| openFolderButton vaultId
                , dialogInput <| msg <| fileSelectionContainer state
                ]
          )
        , ( "Users"
          , div [ class "VaultDialog-Tab-Content" ]
                [ msg <| text "Vault Users:"
                ]
          )
        ]


dialogInput : Html msg -> Html msg
dialogInput body =
    div [ class "VaultDialog-Input" ]
        [ body ]


cancelButton : VaultId -> Html Model.Msg
cancelButton vaultId =
    span [ class "VaultDialog-Button-Cancel" ]
        [ Ui.Button.model "Cancel" "secondary" "small"
            |> Ui.Button.view (Model.CloseVaultDetails vaultId)
        ]


deleteButton : State -> VaultId -> Html Model.Msg
deleteButton state vaultId =
    span
        [ classList
            [ ( "Hidden", state.isNew )
            , ( "VaultDialog-Button-Delete", True )
            ]
        ]
        [ Ui.Button.model "Delete" "danger" "small"
            |> Ui.Button.view (Model.VaultDialog vaultId AskDeleteVault)
        ]


saveButton : VaultId -> Html Model.Msg
saveButton vaultId =
    span [ class "VaultDialog-Button-Save" ]
        [ Ui.Button.model "Save" "primary" "small"
            |> Ui.Button.view (Model.SaveVaultDetails vaultId)
        ]


openFolderButton : VaultId -> Html Msg
openFolderButton vaultId =
    let
        msg =
            OpenFolderDialog vaultId
    in
        Ui.Button.model "Select Folder" "primary" "small"
            |> Ui.Button.view msg
            |> labeledLeft [ class "VaultDialog-InputLabel" ]
                Nothing
                "Folder"


nameInput : VaultId -> State -> Html Model.Msg
nameInput vaultId state =
    Ui.Input.view state.nameInput
        |> Html.map (Model.VaultDialog vaultId << NameInput)
        |> labeledLeft [ class "VaultDialog-InputLabel" ]
            (Just (Model.FocusOn state.nameInput.uid))
            "Name"


fileSelectionContainer : State -> Html Msg
fileSelectionContainer state =
    let
        settings =
            { direction = "column"
            , compact = False
            , align = "start"
            }

        hasFiles =
            not <| Dict.isEmpty state.localFolderItems

        body =
            if hasFiles then
                [ Ui.Container.view settings [] (renderFolders state)
                    |> labeledLeft [ class "VaultDialog-InputLabel" ]
                        Nothing
                        "Files"
                ]
            else
                []
    in
        div [ class "VaultDialog-FileSelection" ]
            body


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
renderFolder state ( path, files ) =
    if isExpanded path state then
        div [ class "VaultDialog-FolderItem" ] <|
            (inFolderPath path
                [ span []
                    [ fileCheckbox path state
                    , folderCollapseToggle path state
                    ]
                , div (hiddenIfIgnored path state [])
                    [ div [ class "VaultDialog-FolderItem-Nested" ]
                        (List.map (renderFile state path) files)
                    ]
                ]
            )
    else
        div [ class "VaultDialog-FolderItem-Collapsed" ]
            (inFolderPath path
                [ span []
                    [ fileCheckbox path state
                    , folderCollapseToggle path state
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


folderCollapseToggle : Path -> State -> Html Msg
folderCollapseToggle path state =
    if isExpanded path state then
        button
            [ onClick (CollapseFolder path)
            , class "VaultDialog-FolderItem-Collapse-Toggle"
            ]
            [ text "-" ]
    else
        button
            [ onClick (ExpandFolder path)
            , classList
                [ ( "VaultDialog-FolderItem-Collapse-Toggle", True )
                , ( "Hidden", isIgnored path state || folderIsEmpty path state )
                ]
            ]
            [ text "+" ]


fileCheckbox : Path -> State -> Html Msg
fileCheckbox path state =
    let
        checkbox =
            Ui.Checkbox.view (fileCheckboxSettings path state)
                |> Html.map (FileCheckBox path)

        checkboxWithLabel =
            checkbox
                |> labeledRight []
                    (Just (ToggleIgnorePath path))
                    (Path.folderName path)
    in
        span [ class "VaultDialog-FolderItem-Checkbox" ]
            [ checkboxWithLabel ]


fileCheckboxSettings : Path -> State -> Ui.Checkbox.Model
fileCheckboxSettings path state =
    { disabled = False
    , readonly = False
    , value = not (isIgnored path state)
    , uid = Path.name path
    }
