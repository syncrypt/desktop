module View.VaultList exposing (..)

import Date exposing (Date)
import Html exposing (Html, button, canvas, div, hr, node, text)
import Html.Attributes exposing (attribute, class, height, id, width)
import Html.Events exposing (onClick)
import MD5
import Model exposing (..)
import Syncrypt.Vault exposing (FlyingVault, Status(..), Vault, NameOrId, nameOrId, asVault)
import Util exposing (bytesReadable)
import View.Css.VaultList exposing (..)
import Html.CssHelpers


{-| Custom HTML helpers using our CSS types
-}
{ id, class, classList } =
    Html.CssHelpers.withNamespace "VaultListView-"


type alias HasId a =
    { a | id : String }


type alias HasModificationDate a =
    { a | modificationDate : Maybe Date }


jdenticonAttr : HasId a -> Html.Attribute msg
jdenticonAttr vault =
    attribute "data-jdenticon-hash" (MD5.hex vault.id)


vaultIcon : HasId a -> Html msg
vaultIcon vault =
    div [ class [ VaultIcon ] ]
        [ canvas [ width 25, height 25, jdenticonAttr vault ]
            []
        ]



-- vaultItemSyncStateClass : Vault -> String


vaultItemSyncStateClass vault =
    VaultStatus vault.status


updatedAtInfo : HasModificationDate a -> Maybe (Html msg) -> Html msg
updatedAtInfo vault updatedAtHeader =
    let
        header =
            Maybe.withDefault (text "") updatedAtHeader
    in
        div [ class [ VaultUpdatedAt ] ]
            [ header
            , case vault.modificationDate of
                Nothing ->
                    text ""

                Just date ->
                    node "TimeAgo"
                        [ attribute "data-tip" "Time since last file was uploaded"
                        , attribute "date" (toString date)
                        ]
                        []
            ]


vaultUpdatedAtInfo : Vault -> Html msg
vaultUpdatedAtInfo vault =
    updatedAtInfo vault
        (Just
            (div [ class [ vaultItemSyncStateClass vault ] ]
                []
            )
        )


flyingVaultUpdatedAtInfo : FlyingVault -> Html msg
flyingVaultUpdatedAtInfo flyingVault =
    updatedAtInfo flyingVault Nothing


vaultInfoItem : HasId a -> List (Html msg) -> Html msg
vaultInfoItem vault bodyItems =
    div [ class [ VaultInfoItem ] ]
        bodyItems


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
        [ div [ class [ VaultActivity ], attribute "data-tip" "Total vault size with all file revisions" ]
            [ text (bytesReadable vault.size) ]
        ]


vaultUserCount : Vault -> Html msg
vaultUserCount vault =
    vaultInfoItem vault
        [ div
            [ class [ VaultInfoItem ] ]
            [ div [ class [ VaultUsers ], attribute "data-tip" "Users with access to vault" ]
                [ text (toString vault.userCount) ]
            ]
        ]


flyingVaultInfoItem : FlyingVault -> Html msg
flyingVaultInfoItem vault =
    div [ class [ FlyingVaultInfoItem ] ]
        [ text ("ID: " ++ vault.id)
        , flyingVaultUpdatedAtInfo vault
        ]


vaultRemoveFromSyncButton : Vault -> Html Msg
vaultRemoveFromSyncButton vault =
    div
        [ class [ VaultRemoveButton ]
        , attribute "data-for" "button-tooltip"
        , attribute "data-tip" "Remove vault from sync"
        , onClick (RemoveVaultFromSync vault)
        ]
        []


openVaultFolderButton : Vault -> Html Msg
openVaultFolderButton vault =
    div
        [ class [ VaultFolderButton ]
        , attribute "data-for" "button-tooltip"
        , attribute "data-tip" "Open Vault Folder"
        , onClick (OpenVaultFolder vault)
        ]
        []


vaultInfo : NameOrId vault -> List (Html msg) -> Html msg
vaultInfo vault nodes =
    let
        vaultHeader =
            [ div [ class [ VaultTitle ] ]
                [ text (nameOrId vault) ]
            , div [ class [ VaultId ] ]
                [ text vault.id ]
            , hr [] []
            ]
    in
        div [ class [ VaultInfo ] ]
            (vaultHeader ++ nodes)


vaultItem : Model -> Vault -> Html Msg
vaultItem model vault =
    div [ class (itemClass model vault), onClick (OpenVaultDetails vault) ]
        [ vaultIcon vault
        , vaultInfo vault
            [ vaultStatus vault
            , vaultActivity vault
            , vaultUserCount vault
            , vaultRemoveFromSyncButton vault
            , openVaultFolderButton vault
            ]
        ]


flyingVaultItem : FlyingVault -> Html Msg
flyingVaultItem flyingVault =
    div [ class [ Card, FlyingVaultCard ], onClick (OpenFlyingVaultDetails flyingVault) ]
        [ vaultIcon flyingVault
        , vaultInfo flyingVault
            [ flyingVaultInfoItem flyingVault
            , vaultActivity (flyingVault |> asVault)
            , vaultUserCount (flyingVault |> asVault)
            ]
        ]


view : Model -> Html Msg
view model =
    div [ class [ VaultList ] ]
        (List.map (vaultItem model) model.vaults)
