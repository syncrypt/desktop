module View.Css.VaultList exposing (..)

import Css exposing (..)
import Css.Namespace exposing (namespace)
import Syncrypt.Vault exposing (Status(..), Vault)
import View.Css.Util exposing (..)
import Model exposing (..)


type CssClass
    = Card
    | VaultCard
    | FlyingVaultCard
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
    | FlyingVaultList
    | FlyingVaultSeparator



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
               ]
        )


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
            ]
    in
        class status (defaultRules ++ additionalRules)


vaultStatuses : List Snippet
vaultStatuses =
    [ vaultStatusWith (VaultStatus Synced)
        [ backgroundImage (url "../assets/check.png") ]
    , vaultStatusWith (VaultStatus Syncing)
        [ backgroundImage (url "../assets/check.png") ]
    , vaultStatusWith (VaultStatus Initializing)
        [ backgroundImage (url "../assets/check.png") ]
    ]


flyingVaultCard : Snippet
flyingVaultCard =
    class FlyingVaultCard
        [ border3 (px 1) dashed (hex "e6e6e6") ]


vaultCardSelected : Snippet
vaultCardSelected =
    class VaultCardSelected
        [ display inlineBlock
        , backgroundColor (hex "efeeea")
        ]



-- helpers


itemClass : Model -> Vault -> List CssClass
itemClass model vault =
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
