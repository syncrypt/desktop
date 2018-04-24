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
import Html exposing (Html, div, span, text)
import Html.Attributes exposing (class)
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


type VaultItemStateType
    = Vault Vault
    | FlyingVault FlyingVault


vaultItemSyncStateClass : VaultItemStateType -> Model -> String
vaultItemSyncStateClass vault model =
    let
        status =
            case vault of
                Vault { status } ->
                    status

                FlyingVault _ ->
                    Unsynced
    in
    "VaultStatus-" ++ toString status


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
            div [ class (vaultItemSyncStateClass (Vault vault) model) ]
                []
        )
        model


flyingVaultUpdatedAtInfo : FlyingVault -> Model -> Html msg
flyingVaultUpdatedAtInfo flyingVault model =
    updatedAtInfo flyingVault
        (Just <|
            div [ class (vaultItemSyncStateClass (FlyingVault flyingVault) model) ]
                []
        )
        model


vaultInfoItem : HasId a -> List (Html msg) -> Html msg
vaultInfoItem vault bodyItems =
    div [ class "VaultInfoItem" ]
        bodyItems


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
    { a
        | userCount : Int
        , id : String
    }


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
                [ vaultUpdatedAtInfo vault model ]
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
                [ flyingVaultUpdatedAtInfo flyingVault model ]
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


flyingVaultListSubtitle : List FlyingVault -> Model -> Html Msg
flyingVaultListSubtitle availableFlyingVaults model =
    let
        tt vaultListText =
            text <| t vaultListText model

        ttt vaultListText extraText =
            text <|
                t vaultListText model
                    ++ extraText
    in
    case model.flyingVaults of
        NotAsked ->
            span
                [ class "UpdateFlyingVaultsButton"
                , onClick UpdateFlyingVaults
                ]
                [ tt <| T.VaultListText T.LoadRemoteVaults ]

        Loading ->
            ttt (T.VaultListText T.FetchingRemoteVaultInfo)
                (Util.animatedDots model.now)

        Failure reason ->
            tt <|
                T.VaultListText <|
                    T.FetchingRemoteVaultsFailed (toString reason)

        Success [] ->
            tt <| T.VaultListText T.YouDontHaveAnyRemoteVaultsYet

        Success _ ->
            case availableFlyingVaults of
                [] ->
                    tt <| T.VaultListText T.YouHaveClonedAllAvailableVaults

                _ ->
                    tt <| T.VaultListText T.ClickOnVaultToClone


flyingVaultList : Model -> Html Msg
flyingVaultList model =
    let
        availableFlyingVaults =
            unsyncedFlyingVaults model

        subtitle =
            flyingVaultListSubtitle availableFlyingVaults model
    in
    div [ class "VaultList" ] <|
        [ div [ class "VaultListInfo" ]
            [ span [ class "Title" ]
                [ text <| t (T.VaultListText T.AvailableVaults) model ]
            , span [ class "Subtitle" ]
                [ subtitle ]
            ]
        ]
            ++ List.map (flyingVaultItem model) availableFlyingVaults


unsyncedFlyingVaults : Model -> List FlyingVault
unsyncedFlyingVaults model =
    let
        vaults =
            RemoteData.withDefault [] model.vaults

        flyingVaults =
            RemoteData.withDefault [] model.flyingVaults

        vaultIds =
            vaults
                |> List.map (\v -> v.remoteId)
                |> Set.fromList

        flyingVaultIds =
            flyingVaults
                |> List.map (\v -> v.remoteId)
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
