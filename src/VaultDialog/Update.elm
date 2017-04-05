module VaultDialog.Update exposing (..)

import Syncrypt.Vault exposing (Vault)
import VaultDialog.Model exposing (..)
import Ui.Input
import Ui.Modal
import Model exposing (Model, State(ShowingVaultDetails))


open : Vault -> Model -> Model
open vault model =
    let
        -- ignore cmd for now
        ( nameInput, _ ) =
            model.vaultDialog.nameInput
                |> Ui.Input.placeholder (Maybe.withDefault "N/A" vault.name)
                |> Ui.Input.setValue (Maybe.withDefault "" vault.name)

        state =
            model.vaultDialog

        newState =
            { state
                | modal = Ui.Modal.open state.modal
                , nameInput = nameInput
            }
    in
        { model | vaultDialog = newState }


close : Model -> Model
close model =
    updateModal model <| Ui.Modal.close model.vaultDialog.modal


updateModal : Model -> Ui.Modal.Model -> Model
updateModal model modalState =
    let
        oldState =
            model.vaultDialog
    in
        { model | vaultDialog = { oldState | modal = modalState } }


updateState : Model -> VaultDialog.Model.State -> Model
updateState model dialogState =
    { model | vaultDialog = dialogState }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Modal msg ->
            (updateModal model <| Ui.Modal.update msg model.vaultDialog.modal)
                ! []

        NameInput msg ->
            let
                ( nameInput, cmd ) =
                    Ui.Input.update msg state.nameInput

                state =
                    model.vaultDialog

                newState =
                    { state | nameInput = nameInput }
            in
                (updateState model newState)
                    ! [ Cmd.map NameInput cmd ]
