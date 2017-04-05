module VaultCreationDialog.Update exposing (..)

import Model exposing (Model)
import Ui.Modal
import Ui.Input
import VaultCreationDialog.Model exposing (Msg(Modal, NameInput), State)
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


asStateIn : Model -> State -> Model
asStateIn model state =
    { model | vaultCreationDialog = state }
