module VaultCreationDialog exposing (..)

import Html exposing (Html, button, div, form, input, label, span, text)
import Html.Attributes exposing (class)
import Ui.Modal
import Ui.Input
import VaultCreationDialog.Model exposing (State, Msg(Modal, NameInput))
import Model exposing (Model)


view : Model -> Html Model.Msg
view { vaultCreationDialog } =
    let
        viewConfig =
            { address = (Modal >> Model.VaultCreationDialog)
            , contents = contents vaultCreationDialog
            , footer = []
            , title = "Create New Vault"
            }
    in
        Ui.Modal.view viewConfig vaultCreationDialog.modal


contents : State -> List (Html Model.Msg)
contents state =
    [ div [ class "VaultCreationDialog-Content" ]
        [ nameInput state
        ]
    ]


nameInput : State -> Html Model.Msg
nameInput { nameInput } =
    Ui.Input.view nameInput
        |> Html.map (NameInput >> Model.VaultCreationDialog)
