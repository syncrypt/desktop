module View.VaultList exposing (..)

import Date exposing (Date)
import Html exposing (Html, button, canvas, div, hr, node, text)
import Html.Attributes exposing (attribute, class, height, id, width)
import Html.Events exposing (onClick)
import MD5
import Model exposing (..)
import Syncrypt.Vault exposing (FlyingVault, Status(..), Vault, nameOrId)
import Css
import Css.Colors
import Util exposing (bytesReadable)


-- HTML views


itemClass : Vault -> String
itemClass vault =
    "card vault-card-selected"


jdenticonAttr : { a | id : String } -> Html.Attribute msg
jdenticonAttr vault =
    attribute "data-jdenticon-hash" (MD5.hex vault.id)


vaultIcon : { a | id : String } -> Html msg
vaultIcon vault =
    div [ class "vault-icon" ]
        [ canvas [ width 25, height 25, jdenticonAttr vault ]
            []
        ]


vaultItemSyncStateClass : Vault -> String
vaultItemSyncStateClass vault =
    "vault-status-" ++ (vault.status |> toString |> String.toLower)


vaultUpdatedAtInfo : { a | modificationDate : Maybe Date } -> Html msg
vaultUpdatedAtInfo vault =
    case vault.modificationDate of
        Nothing ->
            text ""

        Just date ->
            node "TimeAgo"
                [ attribute "data-tip" "Time since last file was uploaded"
                , attribute "date" (toString date)
                ]
                []



-- vaultInfoItem : Vault -> List (Html msg) -> Html msg


vaultInfoItem : Vault -> List (Html a) -> Html a
vaultInfoItem vault bodyItems =
    div [ class "vault-info-item" ]
        ((text vault.id) :: bodyItems)


vaultStatus : Vault -> Html msg
vaultStatus vault =
    let
        updatedAt =
            vaultUpdatedAtInfo vault
    in
        case vault.status of
            Initializing ->
                vaultInfoItem vault
                    [ text "Generating key&hellip;"
                    , updatedAt
                    ]

            _ ->
                vaultInfoItem vault
                    [ updatedAt ]


vaultActivity : Vault -> Html msg
vaultActivity vault =
    vaultInfoItem vault
        [ div [ class "vault-activity", attribute "data-tip" "Total vault size with all file revisions" ]
            [ text (bytesReadable vault.size) ]
        ]


vaultUserCount : Vault -> Html msg
vaultUserCount vault =
    vaultInfoItem vault
        [ div
            [ class "vault-info-item" ]
            [ div [ class "vault-users", attribute "data-tip" "Users with access to vault" ]
                [ text "{vault.user_count || 0}" ]
            ]
        ]


flyingVaultInfoItem : FlyingVault -> Html msg
flyingVaultInfoItem vault =
    div [ class "flying-vault-info-item" ]
        [ text ("ID: " ++ vault.id)
        , vaultUpdatedAtInfo vault
        ]


vaultRemoveFromSyncButton : Vault -> Html Msg
vaultRemoveFromSyncButton vault =
    div
        [ class "vault-remove-button"
        , attribute "data-for" "button-tooltip"
        , attribute "data-tip" "Remove vault from sync"
        , onClick (VaultList (RemoveVaultFromSync vault))
        ]
        []


vaultItem : Vault -> Html Msg
vaultItem vault =
    div [ class (itemClass vault), onClick (OpenVaultDetails vault) ]
        [ vaultIcon vault
        , div [ class "vault-info" ]
            [ div [ class "vault-title" ]
                [ text (nameOrId vault) ]
            , hr [] []
            , vaultStatus vault
            , vaultActivity vault
            , vaultUserCount vault
            , vaultRemoveFromSyncButton vault
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



-- CSS
