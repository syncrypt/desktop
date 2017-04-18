module VaultCreationDialog.Update exposing (..)

import Model exposing (Model)
import Ui.Modal
import Ui.Input
import VaultCreationDialog.Model
    exposing
        ( FolderItem
        , Msg(Modal, NameInput, FileList, FileCheckBox, NestedFileList, FolderItemToggle)
        , State
        , isIgnored
        , addNestedFolderItems
        )
import Dialog exposing (asModalIn)
import Platform.Cmd exposing (map)


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

        FileList path items ->
            ({ vaultCreationDialog
                | localFolderItems = Just items
                , localFolderPath = Just path
             }
                |> asStateIn model
            )
                ! []

        FileCheckBox fi _ ->
            (model |> toggleIgnoreFile fi)
                ! []

        NestedFileList fi fis ->
            (vaultCreationDialog
                |> addNestedFolderItems fi fis
                |> asStateIn model
            )
                ! []

        FolderItemToggle fi ->
            (toggleIgnoreFile fi model)
                ! []


toggleIgnoreFile : FolderItem -> Model -> Model
toggleIgnoreFile fi ({ vaultCreationDialog } as model) =
    let
        ignoreFiles =
            if isIgnored fi vaultCreationDialog then
                vaultCreationDialog.ignoreFiles
                    |> List.filter ((/=) fi)
            else
                fi :: vaultCreationDialog.ignoreFiles
    in
        { vaultCreationDialog | ignoreFiles = ignoreFiles }
            |> asStateIn model


asStateIn : Model -> State -> Model
asStateIn model state =
    { model | vaultCreationDialog = state }
