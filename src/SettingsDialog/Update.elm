module SettingsDialog.Update exposing (update)

import ConfirmationDialog
import Daemon
import Dialog exposing (asModalIn)
import Model
import SettingsDialog.Model exposing (..)
import SettingsDialog.Ports
import Ui.Modal


update : Msg -> Model.Model -> ( Model.Model, Cmd Model.Msg )
update msg ({ settingsDialog } as model) =
    case msg of
        ConfirmationDialogMsg ConfirmationDialog.Close ->
            close model
                ! []

        ConfirmationDialogMsg msg ->
            (settingsDialog
                |> ConfirmationDialog.update msg
                |> asStateIn model
            )
                ! []

        Close ->
            close model
                ! []

        RestartDaemon ->
            model ! [ Daemon.restart model ]

        ShutdownDaemon ->
            model ! [ Daemon.shutdown model ]

        StartDaemon ->
            model ! [ SettingsDialog.Ports.startDaemon () ]

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
