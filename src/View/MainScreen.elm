module View.MainScreen exposing (..)

import Html exposing (Html, button, div, text, h1)
import Html.Attributes exposing (class)


-- import Html.Events exposing (onClick)

import View.VaultList
import Model exposing (..)


view : Model -> Html Msg
view model =
    let
        vaultsHeader =
            h1 [] [ text "Local Vaults:" ]

        flyingVaultsHeader =
            case model.flyingVaults of
                [] ->
                    text ""

                _ ->
                    h1 []
                        [ text "Remote Vaults:" ]

        vaultCards =
            div [ class "vault-list" ]
                (List.map View.VaultList.vaultItem model.vaults)

        flyingVaultCards =
            div [ class "flying-vault-list" ]
                (List.map View.VaultList.flyingVaultItem model.flyingVaults)
    in
        div []
            [ vaultsHeader
            , vaultCards
            , flyingVaultsHeader
            , flyingVaultCards
            ]
