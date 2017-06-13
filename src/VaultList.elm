module VaultList exposing (..)

import Date exposing (Date)
import Html exposing (Html, button, canvas, div, h1, hr, img, node, span, text)
import Html.Attributes exposing (attribute, class, height, id, src, width)
import Html.Events exposing (onClick)
import Model exposing (..)
import RemoteData exposing (RemoteData(..))
import Set
import Syncrypt.Vault exposing (FlyingVault, NameOrId, Status(..), Vault, asVault, nameOrId)
import Translation as T exposing (t)
import Util exposing (Direction(..), TooltipLength(..), bytesReadable, tooltipItem)


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
                (t T.LastUpdateToVaultLabel model)
                [ header
                , case ( vault.modificationDate, model.now ) of
                    ( Nothing, _ ) ->
                        text (t T.NoFilesUploadedYet model)

                    ( Just date, Nothing ) ->
                        text (toString date)

                    ( Just date, Just now ) ->
                        text <| (t (T.Updated date (Date.fromTime now)) model)
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
flyingVaultUpdatedAtInfo flyingVault model =
    let
        updatedText =
            case flyingVault.modificationDate of
                Nothing ->
                    text ""

                Just _ ->
                    text <| t T.LastUpdateToVault model
    in
        updatedAtInfo flyingVault (Just updatedText) model


vaultInfoItem : HasId a -> List (Html msg) -> Html msg
vaultInfoItem vault bodyItems =
    div [ class "VaultList-VaultInfoItem" ]
        bodyItems


vaultStatus : Vault -> Model -> Html msg
vaultStatus vault model =
    vaultInfoItem vault
        [ vaultUpdatedAtInfo vault model ]


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
        [ flyingVaultUpdatedAtInfo vault model ]


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
        vaultListInfo =
            div [ class "VaultList-VaultListInfo" ]
                [ span [ class "VaultList-Title" ]
                    [ text "Local Vaults" ]
                , span [ class "VaultList-Subtitle" ] <|
                    case model.vaults of
                        Success _ ->
                            [ text <| t T.VaultListHeaderDescription model ]

                        Failure reason ->
                            [ text <| "Failed to load vaults: " ++ toString reason ]

                        Loading ->
                            [ text "Loading vaults" ]

                        NotAsked ->
                            []
                ]

        vaultItems =
            RemoteData.withDefault [] model.vaults
                |> List.map (vaultItem model)
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
                , span [ class "VaultList-Subtitle" ] <|
                    case model.flyingVaults of
                        NotAsked ->
                            [ span
                                [ class "VaultList-UpdateFlyingVaultsButton"
                                , onClick UpdateFlyingVaults
                                ]
                                [ text "Load remote vaults" ]
                            ]

                        Loading ->
                            [ text "Fetching remote vault info..." ]

                        Failure reason ->
                            [ text <| "Error fetching remote vaults: " ++ toString reason ]

                        Success _ ->
                            [ text "Click on a vault to clone it to your computer" ]
                ]
             ]
                ++ flyingVaultItems
            )


unsyncedFlyingVaults : Model -> List FlyingVault
unsyncedFlyingVaults model =
    let
        vaults =
            RemoteData.withDefault [] model.vaults

        flyingVaults =
            RemoteData.withDefault [] model.flyingVaults

        vaultIds =
            Set.fromList (List.map (\v -> v.id) vaults)

        flyingVaultIds =
            Set.fromList (List.map (\v -> v.id) flyingVaults)

        diff =
            Set.diff flyingVaultIds vaultIds
    in
        flyingVaults
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
