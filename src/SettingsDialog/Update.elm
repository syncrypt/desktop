module SettingsDialog.Update exposing (update, asStateIn)

import ConfirmationDialog
import SettingsDialog.Model exposing (..)


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


asStateIn : HasSettingsDialog a msg -> State msg -> HasSettingsDialog a msg
asStateIn model state =
    { model | settingsDialog = state }
