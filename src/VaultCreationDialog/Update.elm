module VaultCreationDialog.Update exposing (..)

import Model exposing (Model)
import Ui.Modal
import Ui.Input
import VaultCreationDialog
import VaultCreationDialog.Model
    exposing
        ( FolderItem
        , Msg(..)
        , State
        , isIgnored
        , addFolder
        , toggleIgnorePath
        , folderName
        )
import Dialog exposing (asModalIn)
import Platform.Cmd exposing (map)
import Dict


open : Model -> ( Model, Cmd Model.Msg )
open model =
    (model.vaultCreationDialog.modal
        |> Ui.Modal.open
        |> asModalIn model.vaultCreationDialog
        |> asStateIn model
    )
        ! []


close : Model -> ( Model, Cmd Model.Msg )
close ({ vaultCreationDialog } as model) =
    (vaultCreationDialog.modal
        |> Ui.Modal.close
        |> asModalIn vaultCreationDialog
        |> asStateIn model
    )
        ! []


update : VaultCreationDialog.Model.Msg -> Model -> ( Model, Cmd Model.Msg )
update msg ({ vaultCreationDialog } as model) =
    case msg of
        Modal msg ->
            (vaultCreationDialog.modal
                |> Ui.Modal.update msg
                |> asModalIn vaultCreationDialog
                |> asStateIn model
            )
                ! []

        NameInput msg ->
            let
                ( nameInput, cmd ) =
                    Ui.Input.update msg vaultCreationDialog.nameInput
            in
                ({ vaultCreationDialog | nameInput = nameInput }
                    |> asStateIn model
                )
                    ! [ cmd |> map (NameInput >> Model.VaultCreationDialog) ]

        FileCheckBox path _ ->
            (vaultCreationDialog
                |> toggleIgnorePath path
                |> asStateIn model
            )
                ! []

        NestedFileList rootPath f ->
            if Just rootPath /= vaultCreationDialog.localFolderPath then
                model
                    ! []
            else
                (vaultCreationDialog
                    |> addFolder f
                    |> asStateIn model
                )
                    ! []

        ToggleIgnorePath path ->
            (vaultCreationDialog
                |> toggleIgnorePath path
                |> asStateIn model
            )
                ! []

        OpenFolderDialog ->
            model
                ! [ VaultCreationDialog.openFolder () ]

        SelectedFolder path ->
            let
                ( nameInput, nameInputMsg ) =
                    Ui.Input.setValue (folderName path) vaultCreationDialog.nameInput
            in
                ({ vaultCreationDialog
                    | localFolderPath = Just path
                    , localFolderItems = Dict.empty
                    , nameInput = nameInput
                 }
                    |> asStateIn model
                )
                    ! [ VaultCreationDialog.getFileList path
                      , Cmd.map (NameInput >> Model.VaultCreationDialog) nameInputMsg
                      ]


asStateIn : Model -> State -> Model
asStateIn model state =
    { model | vaultCreationDialog = state }
