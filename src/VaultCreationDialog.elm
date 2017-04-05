module VaultCreationDialog exposing (..)

import Html exposing (Html, button, div, form, input, label, span, text)
import Ui.Modal
import Ui.Input
import Css.Util
import VaultCreationDialog.Model exposing (..)
import VaultCreationDialog.Css exposing (..)
import Model exposing (Model)


{ id, class, classList } =
    Css.Util.namespacedHelpers VaultCreationDialog.Css.namespace


view : Model -> Html Msg
view { vaultCreationDialog } =
    let
        viewConfig =
            { address = Modal
            , contents = contents vaultCreationDialog
            , footer = []
            , title = "Create New Vault"
            }
    in
        Ui.Modal.view viewConfig vaultCreationDialog.modal


contents : State -> List (Html Msg)
contents state =
    [ div [ class [ Content ] ]
        [ nameInput state
        ]
    ]


nameInput : State -> Html Msg
nameInput { nameInput } =
    Ui.Input.view nameInput
        |> Html.map NameInput
