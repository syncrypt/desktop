module View.VaultList exposing (..)

import Html exposing (Html, button, div, text, node, hr, canvas)
import Html.Attributes exposing (class, id, attribute, height, width)
import Html.Events exposing (onClick)
import MD5
import Model.Vault exposing (Vault, vaultName)


type VaultListAction
    = OpenVaultDetails


vaultItemClass vault =
    "card vault-card-selected"


vaultIcon vault =
    div [ class "vault-icon" ]
        [ canvas
            [ width 100
            , height 100
            , attribute "data-jdenticon-hash" (MD5.hex vault.id)
            ]
            []
        ]


vaultItem vault =
    div [ class (vaultItemClass vault), onClick OpenVaultDetails ]
        [ vaultIcon vault
        , div [ class "vault-info" ]
            [ div [ class "vault-title" ]
                [ text (vaultName vault) ]
            , hr [] []
            , div [ class "vault-info-item" ]
                [ div [ class "vault-updated-at" ]
                    []
                ]
            ]
        ]
