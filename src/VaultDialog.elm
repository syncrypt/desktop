module VaultDialog exposing (..)

import Html exposing (Html, button, div, h1, hr, node, span, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Ui.Modal
import Ui.Input
import Model exposing (Model, State(ShowingVaultDetails))
import VaultDialog.Model exposing (Msg(Modal, NameInput))


view : Model -> Html Model.Msg
view ({ vaultDialog } as model) =
    let
        viewConfig =
            { address = (Modal >> Model.VaultDialog)
            , contents = contents model
            , footer = []
            , title = "Vault Details for " ++ currentVaultId model
            }
    in
        Ui.Modal.view viewConfig vaultDialog.modal


currentVaultId : Model -> String
currentVaultId model =
    case model.state of
        ShowingVaultDetails v ->
            v.id

        _ ->
            "Unknown ID"


contents : Model -> List (Html Model.Msg)
contents model =
    [ div [ class "VaultDialog-Content" ]
        [ text <| "Modal content for vault: " ++ currentVaultId model
        , nameInput model.vaultDialog
        ]
    ]


nameInput : VaultDialog.Model.State -> Html Model.Msg
nameInput { nameInput } =
    Ui.Input.view nameInput
        |> Html.map (NameInput >> Model.VaultDialog)
