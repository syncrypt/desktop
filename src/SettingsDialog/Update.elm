module SettingsDialog.Update exposing (update)

import ConfirmationDialog
import SettingsDialog.Model exposing (..)
import Ui.Modal
import Dialog exposing (asModalIn)
import Daemon
import Model


update : Msg -> Model.Model -> ( Model.Model, Cmd Model.Msg )
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
                ! [ Daemon.updateGUIConfig
                        model
                        { isFirstLaunch = model.isFirstLaunch
                        , language = lang
                        }
                  ]

        ModalMsg msg ->
            (settingsDialog.modal
                |> Ui.Modal.update msg
                |> asModalIn settingsDialog
                |> asStateIn model
            )
                ! []
