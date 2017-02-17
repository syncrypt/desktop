module View.Css.VaultList exposing (..)

import Css exposing (..)
import Css.Elements exposing (canvas, hr)
import Css.Namespace exposing (namespace)
import Model exposing (..)
import Syncrypt.Vault exposing (FlyingVault, Status(..), Vault)
import View.Css.Util exposing (..)


type CssClass
    = Card
    | VaultCard
    | FlyingVaultCard
    | VaultPlus
    | VaultPlusIcon
    | VaultCardSelected
    | VaultIcon
    | VaultInfo
    | VaultId
    | VaultStatus Status
    | VaultUpdatedAt
    | VaultInfoItem
    | VaultActivity
    | FlyingVaultInfoItem
    | VaultRemoveButton
    | VaultFolderButton
    | VaultTitle
    | FlyingVaultItem
    | VaultUsers
    | VaultList
    | VaultListInfo
    | FlyingVaultList
    | FlyingVaultSeparator
    | Title
    | Subtitle



-- type CssIds
--     = ?


css : Stylesheet
css =
    (stylesheet << namespace "VaultListView-")
        (vaultStatuses
            ++ [ vaultList
               , card
               , flyingVaultSeperator
               , flyingVaultCard
               , vaultCardSelected
               , vaultCard
               , vaultListInfo
               , vaultPlus
               , vaultPlusIcon
               , vaultInfo
               ]
        )



-- Snippets


vaultList : Snippet
vaultList =
    class VaultList
        [ marginTop (px 10)
        , marginBottom (px 80)
        ]


card : Snippet
card =
    class Card
        [ display inlineBlock
        , border3 (px 1) solid (hex "e6e6e6")
        , height (px 130)
        , width (px 250)
        , overflow hidden
        , marginLeft (px 20)
        , marginTop (px 10)
        , padding (px 10)
        , position relative
        , transition "all 0.25s"
        , backgroundColor (hex "efddda")
        , highlightWithPointerOnHover
        ]


flyingVaultSeperator : Snippet
flyingVaultSeperator =
    class FlyingVaultSeparator
        [ width (pct 50)
        , borderColor (hex "efddda")
        , borderWidth "thin"
        , borderCollapse collapse
        , property "opacity" "0.66"
        , marginTop (px 50)
        , marginBottom (px 50)
        ]


vaultStatusWith : CssClass -> List Mixin -> Snippet
vaultStatusWith status additionalRules =
    let
        defaultRules =
            [ backgroundSize2 (px 22) (px 22)
            , marginLeft (px -2)
            , marginTop (px -5)
            , marginRight (px 5)
            , width (px 22)
            , height (px 22)
            , backgroundRepeat noRepeat
            , float left
            , animation "rotating 2s linear infinite"
            ]
    in
        class status (defaultRules ++ additionalRules)


vaultStatuses : List Snippet
vaultStatuses =
    [ vaultStatusWith (VaultStatus Synced)
        [ backgroundImage (url "../assets/check.png") ]
    , vaultStatusWith (VaultStatus Initializing)
        [ backgroundImage (url "../assets/update_vault.png") ]
    , vaultStatusWith (VaultStatus Syncing)
        [ backgroundImage (url "../assets/update_vault.png") ]
    , vaultStatusWith (VaultStatus Initializing)
        [ backgroundImage (url "../assets/update_vault.png") ]
    , vaultStatusWith (VaultStatus Ready)
        [ backgroundImage (url "../assets/update_vault.png") ]
    ]


flyingVaultCard : Snippet
flyingVaultCard =
    class FlyingVaultCard
        [ border3 (px 1) dashed (hex "e6e6e6")
        , cursor pointer
        , display inlineBlock
        , marginLeft (px 20)
        , marginTop (px 10)
        , padding (px 10)
        , transition "all 0.5s"
        , backgroundColor (hex "aaa")
        , descendants
            [ canvas [ opacity (num 0.3) ]
            ]
        , mixin
            [ hover
                [ backgroundColor (hex "efddda")
                ]
            ]
        ]


vaultCardSelected : Snippet
vaultCardSelected =
    class VaultCardSelected
        [ display inlineBlock
        , backgroundColor (hex "efeeea")
        ]


vaultCard : Snippet
vaultCard =
    class VaultCard
        [ transition "all 0.25s"
        , mixin
            [ hover
                [ opacity (num 0.7)
                , transition "all 0.25s"
                , cursor pointer
                ]
            ]
        ]



-- TODO:
-- .vault-card-selected .vault-title,
-- .vault-card:hover .vault-title {
--   color: #000000;
-- }
-- Mixins


highlightWithPointerOnHover : Mixin
highlightWithPointerOnHover =
    mixin
        [ hover
            [ opacity (num 0.7)
            , transition "all 0.25s"
            , cursor pointer
            ]
        ]


vaultListInfo : Snippet
vaultListInfo =
    class VaultListInfo
        [ overflow hidden
        , marginLeft (px 20)
        , color
            (hex "eee")
        , descendants
            [ class Title
                [ fontSize (px 25)
                , display block
                ]
            , class Subtitle
                [ fontSize (px 15)
                , marginBottom (px 10)
                , display block
                ]
            ]
        ]


vaultPlus : Snippet
vaultPlus =
    class VaultPlus
        [ display inlineBlock
        , height (px 130)
        , width (px 250)
        , overflow hidden
        , marginLeft (px 20)
        , marginTop (px 10)
        , padding (px 10)
        , position relative
        , transition "all 0.25s"
        ]


vaultPlusIcon : Snippet
vaultPlusIcon =
    class VaultPlusIcon
        [ height (px 90)
        , width (px 90)
        , backgroundImage (url "../assets/add_orange.png")
        , backgroundSize2 (px 90) (px 90)
        , backgroundRepeat noRepeat
        , display inlineBlock
        , position relative
        , top (px 20)
        , left (px 80)
        , opacity (num 0.5)
        , mixin
            [ hover
                [ backgroundImage (url "../assets/add.png")
                , opacity (num 1.0)
                , cursor pointer
                ]
            ]
        ]


vaultInfo : Snippet
vaultInfo =
    class VaultInfo
        [ paddingLeft (px 70)
        , marginRight (px 18)
        , descendants
            [ hr
                [ borderColor (hex "d1c8c6")
                , marginTop (px 4)
                ]
            ]
        ]


vaultTitle : Snippet
vaultTitle =
    class VaultTitle
        [ fontWeight (int 600)
        , fontSize (px 14)
        , color (hex "666")
        , textAlign left
        , width (pct 100)
        , height (em 1)
        , marginTop (px 10)
        , marginRight (px 10)
        , textOverflow ellipsis
        , overflow hidden
        ]



-- Helpers


vaultItemClass : Model -> Vault -> List CssClass
vaultItemClass model vault =
    let
        defaultClass =
            [ Card, VaultCard ]
    in
        case model.state of
            ShowingVaultDetails selectedVault ->
                if vault == selectedVault then
                    [ Card, VaultCardSelected ]
                else
                    defaultClass

            _ ->
                defaultClass


flyingVaultItemClass : Model -> FlyingVault -> List CssClass
flyingVaultItemClass model flyingVault =
    let
        defaultClass =
            [ Card, FlyingVaultCard ]
    in
        case model.state of
            ShowingFlyingVaultDetails selectedVault ->
                if flyingVault == selectedVault then
                    [ Card, VaultCardSelected ]
                else
                    defaultClass

            _ ->
                defaultClass
