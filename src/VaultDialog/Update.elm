module VaultDialog.Update exposing (..)

import Syncrypt.Vault exposing (Vault)
import VaultDialog.Model exposing (..)
import Ui.Input
import Ui.Modal
import Model exposing (Model, State(ShowingVaultDetails))


open : Vault -> Model -> ( Model, Cmd Model.Msg )
open vault model =
    let
        -- ignore cmd for now
        ( nameInput, cmd ) =
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
        ( { model | vaultDialog = newState }
        , cmd
            |> Cmd.map NameInput
            |> Cmd.map Model.VaultDialog
        )


close : Model -> ( Model, Cmd Model.Msg )
close model =
    ( updateModal model <| Ui.Modal.close model.vaultDialog.modal, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Model.Msg )
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
                    ! [ cmd
                            |> Cmd.map NameInput
                            |> Cmd.map Model.VaultDialog
                      ]


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
