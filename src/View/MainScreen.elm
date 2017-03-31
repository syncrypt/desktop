module View.MainScreen exposing (..)

import Html exposing (Html, button, div, h1, node, span, text)
import Html.Attributes exposing (..)
import Html.CssHelpers
import Html.Events exposing (onClick)
import Model exposing (..)
import View.Css.MainScreen exposing (..)
import View.VaultList
import View.VaultDialog
import View.VaultCreationDialog


{-| Custom HTML helpers using our CSS types
-}
{ id, class, classList } =
    Html.CssHelpers.withNamespace "MainScreenView-"


view : Model -> Html Msg
view model =
    case model.state of
        ShowingVaultDetails vault ->
            layout model
                []

        _ ->
            layout model []


currentClass : Model -> List CssClass
currentClass model =
    if model.sidebarOpen then
        [ Container ]
    else
        [ Container, Expanded ]


layout : Model -> List (Html Msg) -> Html Msg
layout model nodes =
    div [ class (currentClass model) ]
        [ header
        , div [ class [ Container ] ]
            (nodes ++ [ View.VaultList.view model ])
        , footer model
        , View.VaultDialog.view model
        , Html.map VaultCreationDialog (View.VaultCreationDialog.view model.vaultCreationDialog)
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
            [ span [ class [ Stats ] ]
                [ text <|
                    (vaults |> List.length |> toString)
                        ++ " Vault(s) / "
                        ++ statsStr
                ]
            ]
