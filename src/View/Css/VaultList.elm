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
    | StatusName



-- type CssIds
--     = ?


css : Stylesheet
css =
    (stylesheet << namespace "VaultListView-") <|
        vaultStatuses
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
               , vaultIcon
               , vaultTitle
               , class VaultId [ fontSize (px 8) ]
               , vaultInfoItem
               , vaultUpdatedAt
               , vaultUsers
               , vaultActivity
               , vaultRemoveButton
               , vaultFolderButton
               ]



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
        [ backgroundImage (url "../assets/check.png") ]
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
        , descendants
            [ class VaultTitle
                [ color (hex "000")
                ]
            ]
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
                , color (hex "000")
                ]
            ]
        , descendants
            [ class VaultTitle
                [ color (hex "000")
                ]
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


vaultIcon : Snippet
vaultIcon =
    class VaultIcon
        [ height (px 50)
        , width (px 50)
        , float left
        , descendants
            [ canvas
                [ height (px 50)
                , width (px 50)
                , marginLeft (px 5)
                , marginRight (px 20)
                , marginTop (px 15)
                , padding (px 0)
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


vaultInfoItem : Snippet
vaultInfoItem =
    class VaultInfoItem
        [ paddingBottom (px 25)
        , position relative
        , top (px -12)
        , descendants
            [ class StatusName
                [ fontSize (px 15)
                , top (px 16)
                , paddingLeft (px 25)
                , position relative
                ]
            ]
        ]


vaultUpdatedAt : Snippet
vaultUpdatedAt =
    class VaultUpdatedAt
        [ fontSize (px 12)
        , width (px 140)
        ]


vaultUsers : Snippet
vaultUsers =
    class VaultUsers
        [ backgroundImage (url "../assets/avatar.png")
        , backgroundSize (px 20)
        , backgroundRepeat noRepeat
        , paddingLeft (px 25)
        , fontWeight (int 300)
        , fontSize (px 15)
        , color (hex "888888")
        , width (px 80)
          --, marginLeft (px 120)
          --, position relative
          --, top (px -20)
        ]


vaultActivity : Snippet
vaultActivity =
    class VaultActivity
        [ backgroundImage (url "../assets/vault.png")
        , backgroundSize (px 18)
        , backgroundRepeat noRepeat
        , paddingLeft (px 25)
        , fontWeight (int 300)
        , fontSize (px 15)
        , color (hex "666666")
        , width (px 80)
        , whiteSpace noWrap
        , position relative
        , top (px -15)
        ]



-- -- TODO: keyframes function doesn't exist in API.
-- rotating : Snippet
-- rotating =
--     keyframes "rotating "
--         [ "from { transform: rotate(0deg); }"
--         , "to { transform: rotate(360deg); }"
--         ]
-- -- Need this:
-- -- @keyframes rotating {
-- --   from { transform: rotate(0deg); }
-- --   to { transform: rotate(360deg); }
-- -- }


vaultRemoveButton : Snippet
vaultRemoveButton =
    class VaultRemoveButton
        [ display none
        , backgroundImage (url "../assets/remove_vault.png")
        , backgroundRepeat noRepeat
        , backgroundSize2 (px 25) (px 25)
        , width (px 25)
        , height (px 25)
        , opacity (num 0.44)
        , position absolute
        , top (px 2)
        , right (px 2)
        , buttonHover
        ]


vaultFolderButton : Snippet
vaultFolderButton =
    class VaultFolderButton
        [ display none
        , backgroundImage (url "../assets/folder.png")
        , backgroundRepeat noRepeat
        , backgroundSize2 (px 20) (px 20)
        , width (px 20)
        , height (px 20)
        , opacity (num 0.44)
        , position absolute
        , bottom (px 10)
        , right (px 4)
        , buttonHover
        ]



-- Mixins


buttonHover =
    mixin
        [ hover
            [ cursor pointer
            , opacity (num 1.0)
            ]
        ]


highlightWithPointerOnHover : Mixin
highlightWithPointerOnHover =
    mixin
        [ hover
            [ opacity (num 0.7)
            , transition "all 0.25s"
            , cursor pointer
            ]
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
