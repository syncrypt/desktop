module SettingsDialog exposing (..)

import ConfirmationDialog
import Html exposing (Html, button, div, form, h4, img, input, label, span, text)


-- import Html.Attributes exposing (class, classList, for, id, src, style)
-- import Html.Events exposing (onClick)

import SettingsDialog.Model exposing (..)


type alias HasSettingsDialog a msg =
    { a | settingsDialog : State msg }


view : HasSettingsDialog a msg -> Html msg
view { settingsDialog } =
    settingsDialog
        |> ConfirmationDialog.view
        |> Html.map settingsDialog.toMsg


update : Msg -> HasSettingsDialog a msg -> ( HasSettingsDialog a msg, Cmd msg )
update msg ({ settingsDialog } as model) =
    case msg of
        ConfirmationDialog (ConfirmationDialog.Close) ->
            (close model)
                ! []

        ConfirmationDialog msg ->
            (settingsDialog
                |> ConfirmationDialog.update msg
                |> asStateIn model
            )
                ! []

        Close ->
            (close model)
                ! []


open : HasSettingsDialog a msg -> HasSettingsDialog a msg
open ({ settingsDialog } as model) =
    { model
        | settingsDialog =
            ConfirmationDialog.open "Coming Soon"
                "Not yet implemented"
                Close
                settingsDialog
    }


close ({ settingsDialog } as model) =
    { model | settingsDialog = ConfirmationDialog.close settingsDialog }


asStateIn : HasSettingsDialog a msg -> State msg -> HasSettingsDialog a msg
asStateIn model state =
    { model | settingsDialog = state }
