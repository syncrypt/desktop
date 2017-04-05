module VaultDialog.Update exposing (..)

import Syncrypt.Vault exposing (Vault)
import VaultDialog.Model exposing (..)
import Ui.Input
import Ui.Modal
import Model exposing (Model, State(ShowingVaultDetails))
import Dialog exposing (asModalIn)


open : Vault -> Model -> ( Model, Cmd Model.Msg )
open vault ({ vaultDialog } as model) =
    let
        ( nameInput, cmd ) =
            vaultDialog.nameInput
                |> Ui.Input.placeholder (Maybe.withDefault "N/A" vault.name)
                |> Ui.Input.setValue (Maybe.withDefault "" vault.name)
    in
        (vaultDialog.modal
            |> Ui.Modal.open
            |> asModalIn (nameInput |> asNameInputIn vaultDialog)
            |> asStateIn model
        )
            ! [ cmd
                    |> Cmd.map NameInput
                    |> Cmd.map Model.VaultDialog
              ]


close : Model -> ( Model, Cmd Model.Msg )
close model =
    (model.vaultDialog.modal
        |> Ui.Modal.close
        |> asModalIn model.vaultDialog
        |> asStateIn model
    )
        ! [ Cmd.none ]


update : Msg -> Model -> ( Model, Cmd Model.Msg )
update msg model =
    case msg of
        Modal msg ->
            (model.vaultDialog.modal
                |> Ui.Modal.update msg
                |> asModalIn model.vaultDialog
                |> asStateIn model
            )
                ! []

        NameInput msg ->
            let
                ( nameInput, cmd ) =
                    Ui.Input.update msg model.vaultDialog.nameInput
            in
                (nameInput
                    |> asNameInputIn model.vaultDialog
                    |> asStateIn model
                )
                    ! [ cmd
                            |> Cmd.map NameInput
                            |> Cmd.map Model.VaultDialog
                      ]


asStateIn : Model -> VaultDialog.Model.State -> Model
asStateIn model state =
    { model | vaultDialog = state }


asNameInputIn : VaultDialog.Model.State -> Ui.Input.Model -> VaultDialog.Model.State
asNameInputIn state nameInput =
    { state | nameInput = nameInput }
