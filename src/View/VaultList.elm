module View.VaultList exposing (..)

import Html exposing (Html, button, div, text, node, hr, canvas)
import Html.Attributes exposing (class, id, attribute, height, width)
import Html.Events exposing (onClick)
import MD5
import Syncrypt.Vault exposing (Vault, nameOrId, Status(..), FlyingVault)
import String
import Model exposing (..)


itemClass : Vault -> String
itemClass vault =
    "card vault-card-selected"


vaultIcon : { a | id : String } -> Html msg
vaultIcon vault =
    div [ class "vault-icon" ]
        [ canvas
            [ width 25
            , height 25
            , attribute "data-jdenticon-hash" (MD5.hex vault.id)
            ]
            []
        ]


vaultItemSyncStateClass : Vault -> String
vaultItemSyncStateClass vault =
    "vault-status-" ++ (vault.status |> toString |> String.toLower)


vaultUpdatedAtInfo : Vault -> Html msg
vaultUpdatedAtInfo vault =
    let
        remainingBody =
            case vault.modificationDate of
                Nothing ->
                    []

                Just date ->
                    [ node "TimeAgo"
                        [ attribute "data-tip" "Time since last file was uploaded"
                        , attribute "date" (toString date)
                        ]
                        []
                    ]
    in
        case vault.status of
            Initializing ->
                div [] ((text "Generating key&hellip;") :: remainingBody)

            _ ->
                div [] remainingBody


vaultUpdatedAt : Vault -> Html msg
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


vaultInfoItem : Vault -> Html msg
vaultInfoItem vault =
    div [ class "vault-info-item" ]
        [ text ("ID: " ++ vault.id)
        , vaultUpdatedAt vault
        ]


flyingVaultInfoItem : { a | id : String } -> Html msg
flyingVaultInfoItem vault =
    div [ class "flying-vault-info-item" ]
        [ text ("ID: " ++ vault.id), text "foi" ]


vaultItem : Vault -> Html Msg
vaultItem vault =
    div [ class (itemClass vault), onClick (OpenVaultDetails vault) ]
        [ vaultIcon vault
        , div [ class "vault-info" ]
            [ div [ class "vault-title" ]
                [ text (nameOrId vault) ]
            , hr [] []
            , vaultInfoItem vault
            ]
        ]


flyingVaultItem : FlyingVault -> Html Msg
flyingVaultItem flyingVault =
    div [ class "flying-vault-item", onClick (OpenFlyingVaultDetails flyingVault) ]
        [ vaultIcon flyingVault
        , div [ class "vault-info" ]
            [ div [ class "vault-title" ]
                [ text (nameOrId flyingVault) ]
            , hr [] []
            , flyingVaultInfoItem flyingVault
            ]
        ]
