module VaultList exposing (..)

import Animation
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
import Html.Attributes exposing (class, id)
import Html.Events exposing (onClick)
import Model exposing (..)
import RemoteData exposing (RemoteData(..))
import Set
import Tooltip
import Translation as T exposing (t)
import Util
    exposing
        ( Position(..)
        , bytesReadable
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
        [ Tooltip.item
            { position = Top
            , length = Tooltip.Auto
            , text = t T.LastUpdateToVaultLabel model
            , visible = False
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
            [ Tooltip.item
                { position = Top
                , length = Tooltip.Auto
                , text = t T.TotalVaultSizeTooltip model
                , visible = False
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
            [ Tooltip.item
                { position = Top
                , length = Tooltip.Auto
                , text = t T.UsersWithAccessTooltip model
                , visible = False
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
            [ Tooltip.item
                { position = Left
                , length = Tooltip.Auto
                , text = t T.TotalVaultRevisionsTooltip model
                , visible = False
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
                    [ text <| t (T.VaultListTxt T.HeaderDescription) model ]

                Failure reason ->
                    [ text <|
                        t
                            (T.VaultListTxt <|
                                T.LoadVaultsFailed (toString reason)
                            )
                            model
                    ]

                Loading ->
                    [ text <| t (T.VaultListTxt T.LoadingVaults) model ]

                NotAsked ->
                    []

        vaultListInfo =
            div [ class "VaultListInfo" ]
                [ span [ class "Title" ]
                    [ text <| t (T.VaultListTxt T.Vaults) model ]
                , span [ class "Subtitle" ]
                    vaultListInfoSubtitle
                ]

        vaultItems =
            RemoteData.withDefault [] model.vaults
                |> List.map (vaultItem model)

        itemsWithTooltip =
            case vaultItems of
                [] ->
                    [ Tooltip.viewIfActive vaultListTooltip
                        T.translate
                        model
                        [ newVaultItemButton ]
                    ]

                _ ->
                    Tooltip.viewIfActive vaultListTooltip T.translate model vaultItems
                        :: [ newVaultItemButton ]
    in
    div [ class "VaultList" ]
        (vaultListInfo :: itemsWithTooltip)


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
            text ""

        Loading ->
            ttt (T.VaultListTxt T.FetchingRemoteVaultInfo)
                (model.now
                    |> Maybe.map Util.animatedDots
                    |> Maybe.withDefault "."
                )

        Failure reason ->
            tt <|
                T.VaultListTxt <|
                    T.FetchingRemoteVaultsFailed (toString reason)

        Success [] ->
            tt <| T.VaultListTxt T.YouDontHaveAnyRemoteVaultsYet

        Success _ ->
            case availableFlyingVaults of
                [] ->
                    tt <| T.VaultListTxt T.YouHaveClonedAllAvailableVaults

                _ ->
                    tt <| T.VaultListTxt T.ClickOnVaultToClone


flyingVaultList : Model -> Html Msg
flyingVaultList model =
    let
        availableFlyingVaults =
            unsyncedFlyingVaults model

        subtitle =
            flyingVaultListSubtitle availableFlyingVaults model

        loadingAnimation =
            Animation.loadingCircle Animation.LargeCircle model
    in
    div [ class "VaultList" ] <|
        [ Tooltip.viewIfActive flyingVaultListTooltip
            T.translate
            model
            ([ div [ class "VaultListInfo" ]
                [ span [ class "Title" ]
                    [ text <| t (T.VaultListTxt T.AvailableVaults) model ]
                , span [ class "Subtitle" ]
                    [ subtitle
                    , if model.flyingVaults == Loading then
                        div []
                            [ loadingAnimation ]
                      else
                        text ""
                    ]
                ]
             ]
                ++ List.map (flyingVaultItem model) availableFlyingVaults
            )
        ]


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
    span []
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
