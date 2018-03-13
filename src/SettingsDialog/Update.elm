module SettingsDialog.Update exposing (toggleChangePasswordForm, update)

import ConfirmationDialog
import Daemon
import Dialog exposing (asModalIn)
import Model
import Ports
import SettingsDialog.Model as SModel
    exposing
        ( HasSettingsDialog
        , Msg(..)
        , State
        , asStateIn
        , close
        , hasChanged
        )
import Ui.Input
import Ui.Modal


update : Msg -> Model.Model -> ( Model.Model, Cmd Model.Msg )
update msg ({ settingsDialog } as model) =
    case msg of
        ConfirmationDialogMsg ConfirmationDialog.Close ->
            ( close model
            , Cmd.none
            )

        ConfirmationDialogMsg dialogMsg ->
            ( settingsDialog
                |> ConfirmationDialog.update dialogMsg
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

        ModalMsg modalMsg ->
            ( settingsDialog.modal
                |> Ui.Modal.update modalMsg
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

        OldPasswordInputMsg inputMsg ->
            model
                |> updateInput inputMsg
                    { input = settingsDialog.oldPasswordInput
                    , cmdMsg = SModel.OldPasswordInputMsg
                    , setInput =
                        \input state -> { state | oldPasswordInput = input }
                    }

        NewPasswordInputMsg inputMsg ->
            model
                |> updateInput inputMsg
                    { input = settingsDialog.newPasswordInput
                    , cmdMsg = SModel.NewPasswordInputMsg
                    , setInput =
                        \input state -> { state | newPasswordInput = input }
                    }

        NewPasswordConfirmationInputMsg inputMsg ->
            model
                |> updateInput inputMsg
                    { input = settingsDialog.newPasswordConfirmationInput
                    , cmdMsg = SModel.NewPasswordConfirmationInputMsg
                    , setInput =
                        \input state -> { state | newPasswordConfirmationInput = input }
                    }

        ConfirmChangePassword ->
            -- TODO
            ( model
            , Cmd.none
            )


dialogMsg : (a -> Msg) -> a -> Model.Msg
dialogMsg msg =
    Model.SettingsDialogMsg << msg


dialogCmd : (a -> Msg) -> ( b, Cmd a ) -> ( b, Cmd Model.Msg )
dialogCmd msg ( model, cmd ) =
    ( model
    , cmd
        |> Cmd.map (dialogMsg msg)
    )


type alias InputUpdateParams =
    { input : Ui.Input.Model
    , cmdMsg : Ui.Input.Msg -> Msg
    , setInput : Ui.Input.Model -> State -> State
    }


updateInput :
    Ui.Input.Msg
    -> InputUpdateParams
    -> HasSettingsDialog a
    -> ( HasSettingsDialog a, Cmd Model.Msg )
updateInput msg { input, cmdMsg, setInput } model =
    let
        ( newInput, cmd ) =
            input
                |> Ui.Input.update msg
                |> dialogCmd cmdMsg
    in
    ( model.settingsDialog
        |> setInput newInput
        |> asStateIn model
        |> hasChanged
    , cmd
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

        ( newPasswordConfirmationInput, cmd3 ) =
            state.newPasswordConfirmationInput
                |> Ui.Input.setValue ""
    in
    ( { state
        | oldPasswordInput = oldPasswordInput
        , newPasswordInput = newPasswordInput
        , newPasswordConfirmationInput = newPasswordConfirmationInput
      }
    , Cmd.batch
        [ cmd1 |> Cmd.map OldPasswordInputMsg
        , cmd2 |> Cmd.map NewPasswordInputMsg
        , cmd3 |> Cmd.map NewPasswordConfirmationInputMsg
        ]
    )
