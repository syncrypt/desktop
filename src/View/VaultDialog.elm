module View.VaultDialog exposing (..)

import Date exposing (Date)
import Html exposing (Html, button, div, hr, node, text, h1, span)
import Html.Attributes exposing (attribute, class, height, id, width)
import Html.Events exposing (onClick)
import Model exposing (..)
import Syncrypt.Vault exposing (FlyingVault, Status(..), Vault, NameOrId, nameOrId, asVault)
import View.Css.VaultDialog exposing (..)
import Html.CssHelpers
import Date.Distance


{ id, class, classList } =
    Html.CssHelpers.withNamespace View.Css.VaultDialog.classNamespace


view : Vault -> Model -> Html Msg
view vault model =
    case model.state of
        ShowingVaultDetails vault ->
            div [ id VaultDialogModal, class [ Modal ] ]
                [ div [ class [ Content ] ]
                    [ div [ class [ Close ], onClick CloseVaultDetails ]
                        []
                    , text <| "Modal content for vault: " ++ vault.id
                    ]
                ]

        _ ->
            text "nothing!"
