port module VaultCreationDialog exposing (..)

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
import VaultCreationDialog.Model
    exposing
        ( Path
        , FileName
        , FolderItem
        , State
        , Msg(..)
        , isIgnored
        , name
        , folderName
        , sortedFolders
        , inRoot
        )


port openFolder : () -> Cmd msg


port selectedFolder : (Path -> msg) -> Sub msg


port getFileList : Path -> Cmd msg


port fileList : (( Path, FolderItem ) -> msg) -> Sub msg


subscriptions : Sub Model.Msg
subscriptions =
    let
        fileListMsg ( rootPath, folderItem ) =
            NestedFileList rootPath folderItem
    in
        Sub.batch
            [ fileList fileListMsg
                |> Sub.map Model.VaultCreationDialog
            , selectedFolder SelectedFolder
                |> Sub.map Model.VaultCreationDialog
            ]


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
        , openFolderButton
        , fileSelectionContainer model
            |> Html.map Model.VaultCreationDialog
        ]
    ]


openFolderButton : Html Model.Msg
openFolderButton =
    Ui.Button.model "Select Folder" "primary" "medium"
        |> Ui.Button.view OpenFolderDialog
        |> Html.map Model.VaultCreationDialog


nameInput : State -> Html Model.Msg
nameInput { nameInput } =
    Ui.Input.view nameInput
        |> Html.map (NameInput >> Model.VaultCreationDialog)


fileSelectionContainer : Model -> Html Msg
fileSelectionContainer { vaultCreationDialog } =
    let
        settings =
            { direction = "column"
            , compact = False
            , align = "start"
            }
    in
        Ui.Container.view settings [] (renderFolders vaultCreationDialog)


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
