module View.VaultList exposing (..)

import Date exposing (Date)
import Date.Distance as Distance
import Html exposing (Html, button, canvas, div, h1, hr, node, span, text)
import Html.Attributes exposing (attribute, class, height, id, width)
import Html.CssHelpers
import Html.Events exposing (onClick)
import MD5
import Model exposing (..)
import Set
import Syncrypt.Vault exposing (FlyingVault, NameOrId, Status(..), Vault, asVault, nameOrId)
import Util exposing (bytesReadable)
import View.Css.Util
import View.Css.VaultList exposing (..)


{-| Custom HTML helpers using our CSS types
-}
{ id, class, classList } =
    View.Css.Util.namespacedHelpers View.Css.VaultList.namespace


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
        [ canvas [ width 100, height 100, jdenticonAttr vault ]
            []
        ]


vaultItemSyncStateClass : Vault -> CssClass
vaultItemSyncStateClass vault =
    VaultStatus vault.status


updatedAtInfo : HasModificationDate a -> Maybe (Html msg) -> Model -> Html msg
updatedAtInfo vault updatedAtHeader model =
    let
        header =
            Maybe.withDefault (text "") updatedAtHeader
    in
        div [ class [ VaultUpdatedAt ] ]
            [ header
            , case ( vault.modificationDate, model.now ) of
                ( Nothing, _ ) ->
                    text ""

                ( Just date, Nothing ) ->
                    text ""

                ( Just date, Just now ) ->
                    text <| (Distance.inWords date now) ++ " ago"
            ]


vaultUpdatedAtInfo : Vault -> Model -> Html msg
vaultUpdatedAtInfo vault =
    updatedAtInfo vault
        (Just
            (div [ class [ vaultItemSyncStateClass vault ] ]
                []
            )
        )


flyingVaultUpdatedAtInfo : FlyingVault -> Model -> Html msg
flyingVaultUpdatedAtInfo flyingVault =
    updatedAtInfo flyingVault <| Just <| text "Updated "


vaultInfoItem : HasId a -> List (Html msg) -> Html msg
vaultInfoItem vault bodyItems =
    div [ class [ VaultInfoItem ] ]
        bodyItems


vaultStatus : Vault -> Model -> Html msg
vaultStatus vault model =
    vaultInfoItem vault
        [ vaultUpdatedAtInfo vault model
        ]


vaultActivity : Vault -> Html msg
vaultActivity vault =
    vaultInfoItem vault
        [ div [ class [ VaultActivity ], attribute "data-tip" "Total vault size with all file revisions" ]
            [ text (bytesReadable vault.size) ]
        ]


vaultUserCount : Vault -> Html msg
vaultUserCount vault =
    vaultInfoItem vault
        [ div [ class [ VaultUsers ], attribute "data-tip" "Users with access to vault" ]
            [ text (toString vault.userCount) ]
        ]


flyingVaultInfoItem : FlyingVault -> Model -> Html msg
flyingVaultInfoItem vault model =
    div [ class [ VaultInfoItem ] ]
        [ flyingVaultUpdatedAtInfo vault model
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


vaultItemOnClick : Model -> Vault -> Msg
vaultItemOnClick model vault =
    let
        openVault =
            OpenVaultDetails vault
    in
        case model.state of
            ShowingVaultDetails v ->
                if vault == v then
                    CloseVaultDetails
                else
                    openVault

            _ ->
                openVault


flyingVaultItemOnClick : Model -> FlyingVault -> Msg
flyingVaultItemOnClick model flyingVault =
    let
        openFlyingVault =
            OpenFlyingVaultDetails flyingVault
    in
        case model.state of
            ShowingFlyingVaultDetails v ->
                if flyingVault == v then
                    CloseVaultDetails
                else
                    openFlyingVault

            _ ->
                openFlyingVault


vaultItem : Model -> Vault -> Html Msg
vaultItem model vault =
    div [ class (vaultItemClass model vault), onClick (vaultItemOnClick model vault) ]
        [ vaultIcon vault
        , vaultInfo vault
            [ vaultStatus vault model
            , vaultUserCount vault
            , vaultActivity vault
            , vaultRemoveFromSyncButton vault
            , openVaultFolderButton vault
            ]
        ]


flyingVaultItem : Model -> FlyingVault -> Html Msg
flyingVaultItem model flyingVault =
    div [ class (flyingVaultItemClass model flyingVault), onClick (flyingVaultItemOnClick model flyingVault) ]
        [ vaultIcon flyingVault
        , vaultInfo flyingVault
            [ flyingVaultInfoItem flyingVault model
            , vaultActivity (flyingVault |> asVault)
            , vaultUserCount (flyingVault |> asVault)
            ]
        ]


newVaultItem : Html Msg
newVaultItem =
    div
        [ attribute "data-tip" "Create a new vault / Add an existing vault folder"
        , attribute "data-offset" "{'bottom': -15, 'left': 0}"
        , attribute "data-for" "new-vault-item-tooltip"
        , class [ VaultPlus ]
        ]
        [ div [ class [ VaultPlusIcon ], onClick CreateNewVault ]
            []
        ]


vaultList : Model -> Html Msg
vaultList model =
    let
        vaultItems =
            List.map (vaultItem model) model.vaults

        vaultListInfo =
            div [ class [ VaultListInfo ] ]
                [ span [ class [ Title ] ]
                    [ text "Local Vaults" ]
                , span [ class [ Subtitle ] ]
                    [ text "These vaults are cloned and synchronized on this computer." ]
                ]
    in
        div [ class [ VaultList ] ]
            ((vaultListInfo :: vaultItems) ++ [ newVaultItem ])


flyingVaultList : Model -> Html Msg
flyingVaultList model =
    let
        flyingVaultItems =
            (List.map (flyingVaultItem model) (unsyncedFlyingVaults model))
    in
        div [ class [ FlyingVaultList ] ]
            ([ hr [ class [ FlyingVaultSeparator ] ]
                []
             , div [ class [ VaultListInfo ] ]
                [ span [ class [ Title ] ]
                    [ text "Available Vaults" ]
                , span [ class [ Subtitle ] ]
                    [ text "Click on a vault to clone it to your computer" ]
                ]
             ]
                ++ flyingVaultItems
            )


unsyncedFlyingVaults : Model -> List FlyingVault
unsyncedFlyingVaults model =
    let
        vaultIds =
            Set.fromList (List.map (\v -> v.id) model.vaults)

        flyingVaultIds =
            Set.fromList (List.map (\v -> v.id) model.flyingVaults)

        diff =
            Set.diff flyingVaultIds vaultIds
    in
        model.flyingVaults
            |> List.filter (\fv -> Set.member fv.id diff)


view : Model -> Html Msg
view model =
    div []
        [ vaultList model
        , flyingVaultList model
        ]
