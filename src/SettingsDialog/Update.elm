module SettingsDialog.Update exposing (update)

import ConfirmationDialog
import SettingsDialog.Model exposing (..)
import Ui.Modal
import Dialog exposing (asModalIn)


update : Msg -> HasSettingsDialog a msg -> ( HasSettingsDialog a msg, Cmd msg )
update msg ({ settingsDialog } as model) =
    case msg of
        ConfirmationDialogMsg ConfirmationDialog.Close ->
            (close model)
                ! []

        ConfirmationDialogMsg msg ->
            (settingsDialog
                |> ConfirmationDialog.update msg
                |> asStateIn model
            )
                ! []

        Close ->
            (close model)
                ! []

        LanguageSelection lang ->
            { model | language = lang }
                ! []

        ModalMsg msg ->
            (settingsDialog.modal
                |> Ui.Modal.update msg
                |> asModalIn settingsDialog
                |> asStateIn model
            )
                ! []
