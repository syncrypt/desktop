module View.VaultDialog exposing (..)

import Html exposing (Html, button, div, h1, hr, node, span, text)
import Html.CssHelpers
import Html.Events exposing (onClick)
import Model exposing (..)
import View.Css.Util
import View.Css.VaultDialog exposing (..)


{ id, class, classList } =
    View.Css.Util.namespacedHelpers View.Css.VaultDialog.namespace


view : Model -> Html Msg
view model =
    case model.state of
        ShowingVaultDetails vault ->
            div [ id VaultDialogModal, class [ Modal, Visible ] ]
                [ div [ class [ Content ] ]
                    [ div [ class [ Close ], onClick CloseVaultDetails ]
                        []
                    , text <| "Modal content for vault: " ++ vault.id
                    ]
                ]

        _ ->
            div [ id VaultDialogModal, class [ Modal, Hidden ] ]
                [ div [ class [ Content ] ]
                    [ div [ class [ Close ], onClick CloseVaultDetails ]
                        []
                    , text ""
                    ]
                ]


layout : List (Html Msg) -> Model -> Html Msg
layout body model =
    let
        visible =
            case body of
                [] ->
                    Hidden

                _ ->
                    Visible
    in
        div [ id VaultDialogModal, class [ Modal, visible ] ]
            [ div [ class [ Content ] ]
                ((div [ class [ Close ], onClick CloseVaultDetails ]
                    []
                 )
                    :: body
                )
            ]
