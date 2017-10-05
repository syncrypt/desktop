module VaultList exposing (..)

import Data.Vault
    exposing
        ( FlyingVault
        , NameOrId
        , Status(..)
        , Vault
        , asVault
        , nameOrId
        )
import Date exposing (Date)
import Html exposing (Html, button, canvas, div, h1, hr, img, node, span, text)
import Html.Attributes exposing (attribute, class, height, id, src, width)
import Html.Events exposing (onClick)
import Model exposing (..)
import RemoteData exposing (RemoteData(..))
import Set
import Translation as T exposing (t)
import Util
    exposing
        ( Direction(..)
        , TooltipLength(..)
        , bytesReadable
        , tooltipItem
        )


type alias HasId a =
    { a | id : String, icon : Maybe String }


type alias HasModificationDate a =
    { a | modificationDate : Maybe Date }


vaultItemSyncStateClass : Vault -> String
vaultItemSyncStateClass vault =
    "VaultStatus-" ++ (toString vault.status)


updatedAtInfo : HasModificationDate a -> Maybe (Html msg) -> Model -> Html msg
updatedAtInfo vault updatedAtHeader model =
    let
        header =
            Maybe.withDefault (text "") updatedAtHeader

        updatedText =
            case ( vault.modificationDate, model.now ) of
                ( Nothing, _ ) ->
                    text (t T.NoFilesUploadedYet model)

                ( Just date, Nothing ) ->
                    text (toString date)

                ( Just date, Just now ) ->
                    text <| (t (T.Updated date now) model)
    in
        div [ class "VaultUpdatedAt" ]
            [ tooltipItem Top
                Auto
                (t T.LastUpdateToVaultLabel model)
                [ header, updatedText ]
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
    div [ class "VaultInfoItem" ]
        bodyItems


vaultStatus : Vault -> Model -> Html msg
vaultStatus vault model =
    vaultUpdatedAtInfo vault model


vaultActivity : Vault -> Html msg
vaultActivity vault =
    vaultInfoItem vault
        [ div [ class "VaultActivity" ]
            [ tooltipItem Top
                Auto
                "Total vault size (with all file revisions on server)"
                [ text (bytesReadable vault.size) ]
            ]
        ]


vaultUserCount : Vault -> Html msg
vaultUserCount vault =
    vaultInfoItem vault
        [ div [ class "VaultUsers" ]
            [ tooltipItem Bottom
                Auto
                "Users with access to vault"
                [ text (toString vault.userCount) ]
            ]
        ]


flyingVaultInfoItem : FlyingVault -> Model -> Html msg
flyingVaultInfoItem vault model =
    div [ class "VaultInfoItem" ]
        [ flyingVaultUpdatedAtInfo vault model ]


vaultRemoveFromSyncButton : Vault -> Html Msg
vaultRemoveFromSyncButton vault =
    div
        [ class "VaultRemoveButton"
        , attribute "data-for" "button-tooltip"
        , attribute "data-tip" "Remove vault from sync"
        , onClick (RemoveVaultFromSync vault.id)
        ]
        []


openVaultFolderButton : Vault -> Html Msg
openVaultFolderButton vault =
    div
        [ class "VaultFolderButton"
        , attribute "data-for" "button-tooltip"
        , attribute "data-tip" "Open Vault Folder"
        , onClick (OpenVaultFolder vault)
        ]
        []


vaultInfo :
    NameOrId vault
    -> List (Html msg)
    -> List (Html msg)
    -> Model
    -> Html msg
vaultInfo vault body footerNodes model =
    let
        vaultHeader =
            [ div [ class "Header" ]
                [ div [ class "VaultTitle" ]
                    [ text (nameOrId vault) ]
                ]
            ]

        vaultBody =
            [ div [ class "Body" ]
                body
            ]

        vaultFooter =
            [ div [ class "Footer" ]
                footerNodes
            ]
    in
        div [ class "VaultInfo" ]
            (vaultHeader ++ vaultBody ++ vaultFooter)


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
    div
        [ class (vaultItemClass model vault)
        , onClick (vaultItemOnClick model vault)
        ]
        [ model
            |> vaultInfo vault
                [ vaultStatus vault model ]
                [ vaultUserCount vault
                , vaultActivity vault
                , vaultRemoveFromSyncButton vault
                , openVaultFolderButton vault
                ]
        ]


flyingVaultItem : Model -> FlyingVault -> Html Msg
flyingVaultItem model flyingVault =
    div
        [ class (flyingVaultItemClass model flyingVault)
        , onClick (flyingVaultItemOnClick model flyingVault)
        ]
        [ model
            |> vaultInfo flyingVault
                [ flyingVaultInfoItem flyingVault model ]
                [ vaultActivity (flyingVault |> asVault)
                , vaultUserCount (flyingVault |> asVault)
                ]
        ]


newVaultItemButton : Html Msg
newVaultItemButton =
    div
        [ attribute "data-tip" "Create a new vault / Add an existing vault folder"
        , attribute "data-offset" "{'bottom': -15, 'left': 0}"
        , attribute "data-for" "new-vault-item-tooltip"
        , class "VaultPlus"
        ]
        [ div [ class "VaultPlusIcon", onClick CreateNewVault ]
            []
        ]


vaultList : Model -> Html Msg
vaultList model =
    let
        vaultListInfoSubtitle =
            case model.vaults of
                Success _ ->
                    [ text <| t T.VaultListHeaderDescription model ]

                Failure reason ->
                    [ text <| "Failed to load vaults: " ++ toString reason ]

                Loading ->
                    [ text "Loading vaults" ]

                NotAsked ->
                    []

        vaultListInfo =
            div [ class "VaultListInfo" ]
                [ span [ class "Title" ]
                    [ text "Vaults" ]
                , span [ class "Subtitle" ]
                    vaultListInfoSubtitle
                ]

        vaultItems =
            RemoteData.withDefault [] model.vaults
                |> List.map (vaultItem model)
    in
        div [ class "VaultList" ]
            ((vaultListInfo :: vaultItems) ++ [ newVaultItemButton ])


flyingVaultList : Model -> Html Msg
flyingVaultList model =
    let
        flyingVaultItems =
            model
                |> unsyncedFlyingVaults
                |> List.map (flyingVaultItem model)

        subtitle =
            case model.flyingVaults of
                NotAsked ->
                    span
                        [ class "UpdateFlyingVaultsButton"
                        , onClick UpdateFlyingVaults
                        ]
                        [ text "Load remote vaults" ]

                Loading ->
                    let
                        suffix =
                            case model.now of
                                Just now ->
                                    case Date.second now % 4 of
                                        0 ->
                                            ""

                                        1 ->
                                            "."

                                        2 ->
                                            ".."

                                        _ ->
                                            "..."

                                Nothing ->
                                    "."
                    in
                        text <| "Fetching remote vault info " ++ suffix

                Failure reason ->
                    text <| "Error fetching remote vaults: " ++ toString reason

                Success _ ->
                    text "Click on a vault to clone it to your computer"
    in
        div [ class "VaultList" ] <|
            [ div [ class "VaultListInfo" ]
                [ span [ class "Title" ]
                    [ text "Available Vaults" ]
                , span [ class "Subtitle" ]
                    [ subtitle ]
                ]
            ]
                ++ flyingVaultItems


unsyncedFlyingVaults : Model -> List FlyingVault
unsyncedFlyingVaults model =
    let
        vaults =
            RemoteData.withDefault [] model.vaults

        flyingVaults =
            RemoteData.withDefault [] model.flyingVaults

        vaultIds =
            vaults
                |> List.map (\v -> v.id)
                |> Set.fromList

        flyingVaultIds =
            flyingVaults
                |> List.map (\v -> v.id)
                |> Set.fromList

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
            "Card VaultCard"
    in
        case model.state of
            ShowingVaultDetails selectedVault ->
                if vault == selectedVault then
                    "Card VaultCardSelected"
                else
                    defaultClass

            _ ->
                defaultClass


flyingVaultItemClass : Model -> FlyingVault -> String
flyingVaultItemClass model flyingVault =
    let
        defaultClass =
            "Card FlyingVaultCard"
    in
        case model.state of
            ShowingFlyingVaultDetails selectedVault ->
                if flyingVault == selectedVault then
                    "Card VaultCardSelected"
                else
                    defaultClass

            _ ->
                defaultClass
