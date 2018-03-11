module LoginDialog.Update exposing (update)

import LoginDialog.Model exposing (HasLoginDialog, Msg(..), State, asStateIn)
import Model exposing (Model)
import Ports
import Ui.Input


update : Msg -> HasLoginDialog a -> ( HasLoginDialog a, Cmd Model.Msg )
update dialogMsg ({ loginDialog } as model) =
    case dialogMsg of
        EmailInput msg ->
            ( updateEmailInput msg model
            , Cmd.none
            )

        PasswordInput msg ->
            ( updatePasswordInput msg model
            , Cmd.none
            )

        Modal _ ->
            ( model
            , Cmd.none
            )

        OpenPasswordResetPage ->
            ( model
            , Ports.openPasswordResetInBrowser ()
            )


updateEmailInput : Ui.Input.Msg -> HasLoginDialog a -> HasLoginDialog a
updateEmailInput msg ({ loginDialog } as model) =
    let
        ( emailInput, cmd ) =
            Ui.Input.update msg loginDialog.emailInput
    in
    { loginDialog | emailInput = emailInput }
        |> asStateIn model


updatePasswordInput : Ui.Input.Msg -> HasLoginDialog a -> HasLoginDialog a
updatePasswordInput msg ({ loginDialog } as model) =
    let
        ( passwordInput, cmd ) =
            Ui.Input.update msg loginDialog.passwordInput
    in
    { loginDialog | passwordInput = passwordInput }
        |> asStateIn model
