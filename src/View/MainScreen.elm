module View.MainScreen exposing (..)

import Html exposing (Html, button, div, h1, node, text)
import Html.Attributes exposing (..)
import Html.CssHelpers
import Html.Events exposing (onClick)
import Model exposing (..)
import View.Css.MainScreen exposing (..)
import View.VaultList


{-| Custom HTML helpers using our CSS types
-}
{ id, class, classList } =
    Html.CssHelpers.withNamespace "MainScreenView-"


view : Model -> Html Msg
view model =
    div []
        [ header
        , View.VaultList.view model
        , footer model
        ]


header : Html Msg
header =
    div [ class [ Header ] ]
        [ div [ class [ Buttons ] ]
            [ node "ReactTooltip"
                [ attribute "delayShow" "250"
                , attribute "effect" "solid"
                , id "header-tooltip"
                , attribute "place" "bottom"
                , attribute "type" "dark"
                ]
                []
            , text "    "
            , node "IconButton"
                [ attribute
                    "data-for"
                    "header-tooltip"
                , attribute "data-tip" "Account Settings"
                , attribute "icon" "settings"
                , onClick OpenAccountSettings
                ]
                []
            , text "    "
            , node "IconButton"
                [ attribute "data-for" "header-tooltip"
                , attribute "data-tip" "Logout"
                , attribute "icon" "logout"
                , onClick Logout
                ]
                []
            , text "  "
            ]
        ]


footer : Model -> Html Msg
footer { stats, vaults } =
    let
        statsStr =
            [ stats.stats, stats.downloads, stats.uploads ]
                |> List.map toString
                |> String.join " / "
    in
        div [ class [ Footer ] ]
            [ text <|
                (vaults |> List.length |> toString)
                    ++ " Vault(s) / "
                    ++ statsStr
            ]
