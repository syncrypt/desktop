module SettingsDialog.Update exposing (toggleChangePasswordForm, update)

import ConfirmationDialog
import Daemon
import Dialog exposing (asModalIn)
import Model
import SettingsDialog.Model as SModel exposing (..)
import SettingsDialog.Ports as Ports
import Ui.Input
import Ui.Modal


update : Msg -> Model.Model -> ( Model.Model, Cmd Model.Msg )
update msg ({ settingsDialog } as model) =
    let
        dialogMsg msg =
            Model.SettingsDialogMsg << msg

        dialogCmd msg ( model, cmd ) =
            ( model
            , cmd |> Cmd.map (dialogMsg msg)
            )
    in
    case msg of
        ConfirmationDialogMsg ConfirmationDialog.Close ->
            ( close model
            , Cmd.none
            )

        ConfirmationDialogMsg msg ->
            ( settingsDialog
                |> ConfirmationDialog.update msg
                |> asStateIn model
            , Cmd.none
            )

        Close ->
            ( close model
            , Cmd.none
            )

        LanguageSelection lang ->
            ( { model | language = lang }
            , Daemon.updateGUIConfig model
                { isFirstLaunch = model.isFirstLaunch
                , language = lang
                }
            )

        ModalMsg msg ->
            ( settingsDialog.modal
                |> Ui.Modal.update msg
                |> asModalIn settingsDialog
                |> asStateIn model
            , Cmd.none
            )

        ToggleChangePasswordForm ->
            let
                ( model2, cmd ) =
                    toggleChangePasswordForm model
            in
            ( model2 |> SModel.hasNotChanged
            , Cmd.map Model.SettingsDialogMsg cmd
            )

        OpenPasswordResetPage ->
            ( model
            , Ports.openPasswordResetInBrowser ()
            )

        OldPasswordInputMsg msg ->
            let
                ( pwInput, cmd ) =
                    Ui.Input.update msg settingsDialog.oldPasswordInput
                        |> dialogCmd SModel.OldPasswordInputMsg
            in
            ( { settingsDialog | oldPasswordInput = pwInput }
                |> asStateIn model
                |> hasChanged
            , cmd
            )

        NewPasswordInputMsg msg ->
            let
                ( pwInput, cmd ) =
                    Ui.Input.update msg settingsDialog.newPasswordInput
                        |> dialogCmd SModel.NewPasswordInputMsg
            in
            ( { settingsDialog | newPasswordInput = pwInput }
                |> asStateIn model
                |> hasChanged
            , cmd
            )

        ConfirmChangePassword ->
            ( model
            , Cmd.none
            )


toggleChangePasswordForm : HasSettingsDialog a -> ( HasSettingsDialog a, Cmd SModel.Msg )
toggleChangePasswordForm ({ settingsDialog } as model) =
    let
        ( newState, cmd ) =
            case settingsDialog.showChangePasswordForm of
                True ->
                    { settingsDialog
                        | showChangePasswordForm = False
                    }
                        |> resetChangePasswordForm

                False ->
                    ( { settingsDialog
                        | showChangePasswordForm = True
                      }
                    , Cmd.none
                    )
    in
    ( newState
        |> asStateIn model
    , cmd
    )


resetChangePasswordForm : State -> ( State, Cmd SModel.Msg )
resetChangePasswordForm state =
    let
        ( oldPasswordInput, cmd1 ) =
            state.oldPasswordInput
                |> Ui.Input.setValue ""

        ( newPasswordInput, cmd2 ) =
            state.newPasswordInput
                |> Ui.Input.setValue ""
    in
    ( { state
        | oldPasswordInput = oldPasswordInput
        , newPasswordInput = newPasswordInput
      }
    , Cmd.batch
        [ cmd1 |> Cmd.map OldPasswordInputMsg
        , cmd2 |> Cmd.map NewPasswordInputMsg
        ]
    )
