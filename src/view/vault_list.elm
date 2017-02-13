module View.VaultList exposing (..)

import Html exposing (Html, button, div, text, node, hr, canvas)
import Html.Attributes exposing (class, id, attribute, height, width)
import Html.Events exposing (onClick)
import MD5
import Syncrypt.Vault exposing (Vault, vaultName, State(..))
import String
import Model exposing (..)


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


vaultItemSyncStateClass vault =
    "vault-status-" ++ (vault.state |> toString |> String.toLower)


vaultUpdatedAtInfo vault =
    let
        remainingBody =
            case vault.modificationDate of
                Nothing ->
                    []

                Just date ->
                    [ node "TimeAgo"
                        [ attribute "data-tip" "Time since last file was uploaded"
                        , attribute "date" vault.modification_date
                        ]
                        []
                    ]
    in
        case vault.state of
            Initializing ->
                div [] ((text "Generating key&hellip;") :: remainingBody)

            _ ->
                div [] remainingBody


vaultInfoItem vault =
    div [ class "vault-info-item" ]
        [ div [ class "vault-updated-at" ]
            [ div [ class (vaultItemSyncStateClass vault) ]
                []
            ]
        ]


vaultItem vault =
    div [ class (vaultItemClass vault), onClick (OpenVaultDetails vault) ]
        [ vaultIcon vault
        , div [ class "vault-info" ]
            [ div [ class "vault-title" ]
                [ text (vaultName vault) ]
            , hr [] []
            , vaultInfoItem vault
            ]
        ]
