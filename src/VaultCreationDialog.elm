module VaultCreationDialog exposing (..)

import Html exposing (Html, button, div, form, input, label, span, text)
import Ui.Modal
import Ui.Input
import Css.Util
import VaultCreationDialog.Model exposing (State, Msg(Modal, NameInput))
import VaultCreationDialog.Css exposing (..)
import Model exposing (Model)


{ id, class, classList } =
    Css.Util.namespacedHelpers VaultCreationDialog.Css.namespace


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
    [ div [ class [ Content ] ]
        [ nameInput state
        ]
    ]


nameInput : State -> Html Model.Msg
nameInput { nameInput } =
    Ui.Input.view nameInput
        |> Html.map (NameInput >> Model.VaultCreationDialog)
