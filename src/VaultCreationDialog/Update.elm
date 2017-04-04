module VaultCreationDialog.Update exposing (..)

import Model exposing (Model)
import Ui.Modal
import Ui.Input
import VaultCreationDialog.Model exposing (Msg(Modal, NameInput), State)


open : Model -> Model
open model =
    updateModal model <| Ui.Modal.open model.vaultCreationDialog.modal


close : Model -> Model
close model =
    updateModal model <| Ui.Modal.close model.vaultCreationDialog.modal


updateModal : Model -> Ui.Modal.Model -> Model
updateModal model modalState =
    let
        oldState =
            model.vaultCreationDialog
    in
        { model | vaultCreationDialog = { oldState | modal = modalState } }


updateState : Model -> State -> Model
updateState model vaultCreationState =
    { model | vaultCreationDialog = vaultCreationState }


update : VaultCreationDialog.Model.Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Modal msg ->
            let
                newModel =
                    updateModal model <| Ui.Modal.update msg model.vaultCreationDialog.modal
            in
                newModel ! []

        NameInput msg ->
            let
                ( nameInput, cmd ) =
                    Ui.Input.update msg model.vaultCreationDialog.nameInput

                oldState =
                    model.vaultCreationDialog
            in
                updateState model { oldState | nameInput = nameInput }
                    ! [ Cmd.map NameInput cmd ]
