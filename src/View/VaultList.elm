module View.VaultList exposing (..)

import Html exposing (Html, button, div, text, node, hr, canvas)
import Html.Attributes exposing (class, id, attribute, height, width)
import Html.Events exposing (onClick)
import MD5
import Syncrypt.Vault exposing (Vault, vaultName, Status(..))
import String
import Model exposing (..)
import Date


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
    "vault-status-" ++ (vault.status |> toString |> String.toLower)


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


vaultUpdatedAt vault =
    div [ class "vault-updated-at" ]
        [ div [ class (vaultItemSyncStateClass vault) ]
            [ case vault.modificationDate of
                Just date ->
                    text ("Last update: " ++ (toString date))

                Nothing ->
                    div [] []
            ]
        ]


vaultInfoItem vault =
    div [ class "vault-info-item" ]
        [ text ("ID: " ++ vault.id)
        , vaultUpdatedAt vault
        ]


flyingVaultInfoItem vault =
    div [ class "flying-vault-info-item" ]
        [ text ("ID: " ++ vault.id) ]


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


flyingVaultItem flyingVault =
    div [ class "flying-vault-item", onClick (OpenFlyingVaultDetails flyingVault) ]
        [ vaultIcon flyingVault
        , div [ class "vault-info" ]
            [ div [ class "vault-title" ]
                [ text (vaultName flyingVault) ]
            , hr [] []
            , flyingVaultInfoItem flyingVault
            ]
        ]
