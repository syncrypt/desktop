module SettingsDialog.View exposing (view)

import ConfirmationDialog
import Html
    exposing
        ( Html
        , button
        , div
        , form
        , h4
        , img
        , input
        , label
        , span
        , text
        )


-- import Html.Attributes exposing (class, classList, for, id, src, style)
-- import Html.Events exposing (onClick)

import SettingsDialog.Model exposing (..)


view : HasSettingsDialog a msg -> Html msg
view { settingsDialog } =
    settingsDialog
        |> ConfirmationDialog.view
        |> Html.map settingsDialog.toMsg
