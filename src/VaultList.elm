module VaultList exposing (..)

import Animation exposing (Animation(..), animation, animations)
import Date exposing (Date)
import Date.Distance as Distance
import Html exposing (Html, button, canvas, div, h1, hr, img, node, span, text)
import Html.Attributes exposing (attribute, class, height, id, src, width)
import Html.Events exposing (onClick)
import Model exposing (..)
import Set
import Syncrypt.Vault exposing (FlyingVault, NameOrId, Status(..), Vault, asVault, nameOrId)
import Util exposing (bytesReadable, tooltipItem, Direction(..), TooltipLength(..))


type alias HasId a =
    { a | id : String, icon : Maybe String }


type alias HasModificationDate a =
    { a | modificationDate : Maybe Date }


vaultIcon : HasId a -> Html msg
vaultIcon vault =
    img [ class "VaultList-VaultIcon", src (Maybe.withDefault "" vault.icon) ]
        []


vaultItemSyncStateClass : Vault -> String
vaultItemSyncStateClass vault =
    "VaultList-VaultStatus-" ++ (toString vault.status)


updatedAtInfo : HasModificationDate a -> Maybe (Html msg) -> Model -> Html msg
updatedAtInfo vault updatedAtHeader model =
    let
        header =
            Maybe.withDefault (text "") updatedAtHeader
    in
        div [ class "VaultList-VaultUpdatedAt" ]
            [ tooltipItem Top
                Auto
                "Last update to vault"
                [ header
                , case ( vault.modificationDate, model.now ) of
                    ( Nothing, _ ) ->
                        text "No files uploaded yet"

                    ( Just date, Nothing ) ->
                        text (toString date)

                    ( Just date, Just now ) ->
                        text <| (Distance.inWords date now) ++ " ago"
                ]
            ]


vaultUpdatedAtInfo : Vault -> Model -> Html msg
vaultUpdatedAtInfo vault =
    updatedAtInfo vault
        (Just
            (div [ class (vaultItemSyncStateClass vault) ]
                []
            )
        )


flyingVaultUpdatedAtInfo : FlyingVault -> Model -> Html msg
flyingVaultUpdatedAtInfo flyingVault =
    let
        updatedText =
            case flyingVault.modificationDate of
                Nothing ->
                    "No Updates"

                Just _ ->
                    "Updated "
    in
        updatedAtInfo flyingVault <| Just <| text updatedText


vaultInfoItem : HasId a -> List (Html msg) -> Html msg
vaultInfoItem vault bodyItems =
    div [ class "VaultList-VaultInfoItem" ]
        bodyItems


vaultStatus : Vault -> Model -> Html msg
vaultStatus vault model =
    vaultInfoItem vault
        [ vaultUpdatedAtInfo vault model
        ]


vaultActivity : Vault -> Html msg
vaultActivity vault =
    vaultInfoItem vault
        [ div [ class "VaultList-VaultActivity" ]
            [ tooltipItem Top
                Auto
                "Total vault size (with all file revisions on server)"
                [ text (bytesReadable vault.size) ]
            ]
        ]


vaultUserCount : Vault -> Html msg
vaultUserCount vault =
    vaultInfoItem vault
        [ div [ class "VaultList-VaultUsers" ]
            [ tooltipItem Bottom
                Auto
                "Users with access to vault"
                [ text (toString vault.userCount) ]
            ]
        ]


flyingVaultInfoItem : FlyingVault -> Model -> Html msg
flyingVaultInfoItem vault model =
    div [ class "VaultList-VaultInfoItem" ]
        [ flyingVaultUpdatedAtInfo vault model
        ]


vaultRemoveFromSyncButton : Vault -> Html Msg
vaultRemoveFromSyncButton vault =
    div
        [ class "VaultList-VaultRemoveButton"
        , attribute "data-for" "button-tooltip"
        , attribute "data-tip" "Remove vault from sync"
        , onClick (RemoveVaultFromSync vault.id)
        ]
        []


openVaultFolderButton : Vault -> Html Msg
openVaultFolderButton vault =
    div
        [ class "VaultList-VaultFolderButton"
        , attribute "data-for" "button-tooltip"
        , attribute "data-tip" "Open Vault Folder"
        , onClick (OpenVaultFolder vault)
        ]
        []


vaultInfo : NameOrId vault -> List (Html msg) -> Html msg
vaultInfo vault nodes =
    let
        vaultHeader =
            [ div [ class "VaultList-VaultTitle" ]
                [ text (nameOrId vault) ]
            , div [ class "VaultList-VaultId" ]
                [ text vault.id ]
            , hr [] []
            ]
    in
        div [ class "VaultList-VaultInfo" ]
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
                    CloseVaultDetails v.id
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
                    CloseVaultDetails v.id
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
        , class "VaultList-VaultPlus"
        ]
        [ div [ class "VaultList-VaultPlusIcon", onClick CreateNewVault ]
            []
        ]


vaultList : Model -> Html Msg
vaultList model =
    let
        vaultItems =
            List.map (vaultItem model) model.vaults

        vaultListInfo =
            div [ class "VaultList-VaultListInfo" ]
                [ span [ class "VaultList-Title" ]
                    [ text "Local Vaults" ]
                , span [ class "VaultList-Subtitle" ]
                    [ text "These vaults are cloned and synchronized on this computer." ]
                ]
    in
        div [ class "VaultList-VaultList" ]
            ((vaultListInfo :: vaultItems) ++ [ newVaultItem ])


flyingVaultList : Model -> Html Msg
flyingVaultList model =
    let
        flyingVaultItems =
            (List.map (flyingVaultItem model) (unsyncedFlyingVaults model))
    in
        div [ class "VaultList-FlyingVaultList" ]
            ([ hr [ class "VaultList-FlyingVaultSeparator" ]
                []
             , div [ class "VaultList-VaultListInfo" ]
                [ span [ class "VaultList-Title" ]
                    [ text "Available Vaults" ]
                , span [ class "VaultList-Subtitle" ]
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


vaultItemClass : Model -> Vault -> String
vaultItemClass model vault =
    let
        defaultClass =
            "VaultList-Card VaultList-VaultCard"
    in
        case model.state of
            ShowingVaultDetails selectedVault ->
                if vault == selectedVault then
                    "VaultList-Card VaultList-VaultCardSelected"
                else
                    defaultClass

            _ ->
                defaultClass


flyingVaultItemClass : Model -> FlyingVault -> String
flyingVaultItemClass model flyingVault =
    let
        defaultClass =
            "VaultList-Card VaultList-FlyingVaultCard"
    in
        case model.state of
            ShowingFlyingVaultDetails selectedVault ->
                if flyingVault == selectedVault then
                    "VaultList-Card VaultList-VaultCardSelected"
                else
                    defaultClass

            _ ->
                defaultClass
