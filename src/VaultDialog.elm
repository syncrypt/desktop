module VaultDialog exposing (..)

import Html exposing (Html, button, div, h1, hr, node, span, text)
import Html.Events exposing (onClick)
import Css.Util
import VaultDialog.Css exposing (..)
import Ui.Modal
import Ui.Input
import Model exposing (Model, State(ShowingVaultDetails))
import Syncrypt.Vault exposing (Vault)
import VaultDialog.Model exposing (Msg(Modal, NameInput))


{ id, class, classList } =
    Css.Util.namespacedHelpers VaultDialog.Css.namespace


view : Model -> Html Msg
view model =
    let
        viewConfig =
            { address = Modal
            , contents = contents model
            , footer = []
            , title = "Vault Details for " ++ currentVaultId model
            }
    in
        Ui.Modal.view viewConfig model.vaultDialog.modal


currentVaultId model =
    case model.state of
        ShowingVaultDetails v ->
            v.id

        _ ->
            "Unknown ID"


contents : Model -> List (Html Msg)
contents model =
    [ div [ class [ Content ] ]
        [ text <| "Modal content for vault: " ++ currentVaultId model
        , nameInput model.vaultDialog
        ]
    ]


nameInput : VaultDialog.Model.State -> Html Msg
nameInput { nameInput } =
    Ui.Input.view nameInput
        |> Html.map NameInput
