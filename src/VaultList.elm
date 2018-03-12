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
import Dict
import Html exposing (Html, button, canvas, div, h1, hr, img, node, span, text)
import Html.Attributes exposing (attribute, class, height, id, src, width)
import Html.Events exposing (onClick)
import Model exposing (..)
import RemoteData exposing (RemoteData(..))
import Set
import Translation as T exposing (t)
import Util
    exposing
        ( Position(..)
        , TooltipLength(..)
        , bytesReadable
        , tooltipItem
        )


type alias HasId a =
    { a | id : String }


type alias HasModificationDate a =
    { a | modificationDate : Maybe Date }


vaultItemSyncStateClass : Vault -> Model -> String
vaultItemSyncStateClass vault model =
    "VaultStatus-" ++ toString (Model.vaultStatus vault model)


updatedAtInfo : HasModificationDate a -> Maybe (Html msg) -> Model -> Html msg
updatedAtInfo vault updatedAtHeader model =
    let
        header =
            Maybe.withDefault (text "") updatedAtHeader

        updatedText =
            case ( vault.modificationDate, model.now ) of
                ( Nothing, _ ) ->
                    text <| t T.NoFilesUploadedYet model

                ( Just date, Nothing ) ->
                    text <| toString date

                ( Just date, Just now ) ->
                    text <| t (T.Updated date now) model
    in
    div [ class "VaultUpdatedAt" ]
        [ tooltipItem
            { position = Top
            , length = Auto
            , text = t T.LastUpdateToVaultLabel model
            }
            [ header, updatedText ]
        ]


vaultUpdatedAtInfo : Vault -> Model -> Html msg
vaultUpdatedAtInfo vault model =
    updatedAtInfo vault
        (Just <|
            div [ class (vaultItemSyncStateClass vault model) ]
                []
        )
        model


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


vaultActivity : Vault -> Model -> Html msg
vaultActivity vault model =
    vaultInfoItem vault
        [ div [ class "VaultActivity" ]
            [ tooltipItem
                { position = Top
                , length = Auto
                , text = t T.TotalVaultSizeTooltip model
                }
                [ text (bytesReadable vault.size) ]
            ]
        ]


type alias HasUserCountAndId a =
    { a | userCount : Int, id : String }


vaultUserCount : HasUserCountAndId a -> Model -> Html msg
vaultUserCount vault model =
    vaultInfoItem vault
        [ div [ class "VaultUsers" ]
            [ tooltipItem
                { position = Top
                , length = Auto
                , text = t T.UsersWithAccessTooltip model
                }
                [ text (toString vault.userCount) ]
            ]
        ]


type alias HasRevisionCountAndId a =
    { a | revisionCount : Int, id : String }


vaultRevisionCount : HasRevisionCountAndId a -> Model -> Html msg
vaultRevisionCount vault model =
    vaultInfoItem vault
        [ div [ class "VaultRevisions" ]
            [ tooltipItem
                { position = Left
                , length = Auto
                , text = t T.TotalVaultRevisionsTooltip model
                }
                [ text (toString vault.revisionCount) ]
            ]
        ]


flyingVaultInfoItem : FlyingVault -> Model -> Html msg
flyingVaultInfoItem vault model =
    div [ class "VaultInfoItem" ]
        [ flyingVaultUpdatedAtInfo vault model ]


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
                [ vaultUserCount vault model
                , vaultActivity vault model
                , vaultRevisionCount vault model
                ]
        ]


flyingVaultItem : Model -> FlyingVault -> Html Msg
flyingVaultItem model flyingVault =
    let
        vault =
            flyingVault |> asVault
    in
    div
        [ class (flyingVaultItemClass model flyingVault)
        , onClick (flyingVaultItemOnClick model flyingVault)
        ]
        [ model
            |> vaultInfo flyingVault
                [ flyingVaultInfoItem flyingVault model ]
                [ vaultUserCount vault model
                , vaultActivity vault model
                , vaultRevisionCount vault model
                ]
        ]


newVaultItemButton : Html Msg
newVaultItemButton =
    div [ class "VaultPlusIcon", onClick CreateNewVault ]
        []


vaultList : Model -> Html Msg
vaultList model =
    let
        vaultListInfoSubtitle =
            case model.vaults of
                Success _ ->
                    [ text <| t (T.VaultListText T.HeaderDescription) model ]

                Failure reason ->
                    [ text <|
                        t
                            (T.VaultListText <|
                                T.LoadVaultsFailed (toString reason)
                            )
                            model
                    ]

                Loading ->
                    [ text <| t (T.VaultListText T.LoadingVaults) model ]

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
                        [ text <| t (T.VaultListText T.LoadRemoteVaults) model ]

                Loading ->
                    text <|
                        t (T.VaultListText T.FetchingRemoteVaultInfo) model
                            ++ Util.animatedDots model.now

                Failure reason ->
                    text <|
                        t
                            (T.VaultListText <|
                                T.FetchingRemoteVaultsFailed <|
                                    toString reason
                            )
                            model

                Success _ ->
                    text <|
                        t (T.VaultListText T.ClickOnVaultToClone) model
    in
    div [ class "VaultList" ] <|
        [ div [ class "VaultListInfo" ]
            [ span [ class "Title" ]
                [ text <| t (T.VaultListText T.AvailableVaults) model ]
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
