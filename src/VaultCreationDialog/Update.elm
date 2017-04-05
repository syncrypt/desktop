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
close model =
    (model.vaultCreationDialog.modal
        |> Ui.Modal.close
        |> asModalIn model.vaultCreationDialog
        |> asStateIn model
    )
        ! []


update : VaultCreationDialog.Model.Msg -> Model -> ( Model, Cmd Model.Msg )
update msg model =
    case msg of
        Modal msg ->
            let
                newModel =
                    model.vaultCreationDialog.modal
                        |> Ui.Modal.update msg
                        |> asModalIn model.vaultCreationDialog
                        |> asStateIn model
            in
                newModel ! []

        NameInput msg ->
            let
                ( nameInput, cmd ) =
                    Ui.Input.update msg model.vaultCreationDialog.nameInput

                oldState =
                    model.vaultCreationDialog
            in
                ({ oldState | nameInput = nameInput }
                    |> asStateIn model
                )
                    ! [ cmd
                            |> Cmd.map NameInput
                            |> Cmd.map Model.VaultCreationDialog
                      ]


asStateIn : Model -> State -> Model
asStateIn model state =
    { model | vaultCreationDialog = state }
