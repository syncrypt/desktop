module VaultCreationDialog.Update exposing (..)

import Model exposing (Model)
import Ui.Modal
import Ui.Input
import VaultCreationDialog.Model
    exposing
        ( FolderItem
        , Msg(Modal, NameInput, FileList, FileCheckBox, NestedFileList)
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

        FileCheckBox fc _ ->
            (model |> toggleIgnoreFile fc)
                ! []

        NestedFileList fc fcs ->
            (vaultCreationDialog
                |> addNestedFolderItems fc fcs
                |> asStateIn model
            )
                ! []


toggleIgnoreFile : FolderItem -> Model -> Model
toggleIgnoreFile fc ({ vaultCreationDialog } as model) =
    let
        ignoreFiles =
            if isIgnored fc vaultCreationDialog then
                vaultCreationDialog.ignoreFiles
                    |> List.filter ((/=) fc)
            else
                fc :: vaultCreationDialog.ignoreFiles
    in
        { vaultCreationDialog | ignoreFiles = ignoreFiles }
            |> asStateIn model


asStateIn : Model -> State -> Model
asStateIn model state =
    { model | vaultCreationDialog = state }
