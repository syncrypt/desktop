module VaultCreationDialog.Update exposing (..)

import Model exposing (Model)
import Ui.Modal
import Ui.Input
import VaultCreationDialog.Model
    exposing
        ( Msg(Modal, NameInput, FileList, FileCheckBox)
        , State
        , isIgnored
        )
import Dialog exposing (asModalIn)


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
                    ! [ cmd |> Cmd.map (NameInput >> Model.VaultCreationDialog) ]

        FileList path contents ->
            ({ vaultCreationDialog
                | localFolderContents = Just contents
                , localFolderPath = Just path
             }
                |> asStateIn model
            )
                ! []

        FileCheckBox folderContent _ ->
            (toggleIgnoreFile folderContent model)
                ! []


toggleIgnoreFile fc ({ vaultCreationDialog } as model) =
    let
        ignoreFiles =
            if isIgnored fc vaultCreationDialog then
                List.filter ((/=) fc)
                    vaultCreationDialog.ignoreFiles
            else
                fc :: vaultCreationDialog.ignoreFiles
    in
        { vaultCreationDialog | ignoreFiles = ignoreFiles }
            |> asStateIn model


asStateIn : Model -> State -> Model
asStateIn model state =
    { model | vaultCreationDialog = state }
