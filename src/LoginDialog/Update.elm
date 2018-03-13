module LoginDialog.Update exposing (update)

import LoginDialog.Model exposing (HasLoginDialog, Msg(..), asStateIn)
import Model
import Ports
import Ui.Input


update : Msg -> HasLoginDialog a -> ( HasLoginDialog a, Cmd Model.Msg )
update dialogMsg model =
    case dialogMsg of
        EmailInput msg ->
            model
                |> updateEmailInput msg

        PasswordInput msg ->
            model
                |> updatePasswordInput msg

        Modal _ ->
            ( model
            , Cmd.none
            )

        OpenPasswordResetPage ->
            ( model
            , Ports.openPasswordResetInBrowser ()
            )


updateEmailInput : Ui.Input.Msg -> HasLoginDialog a -> ( HasLoginDialog a, Cmd Model.Msg )
updateEmailInput msg ({ loginDialog } as model) =
    let
        ( emailInput, cmd ) =
            Ui.Input.update msg loginDialog.emailInput
    in
    ( { loginDialog | emailInput = emailInput }
        |> asStateIn model
    , cmd
        |> Cmd.map (Model.LoginDialogMsg << EmailInput)
    )


updatePasswordInput : Ui.Input.Msg -> HasLoginDialog a -> ( HasLoginDialog a, Cmd Model.Msg )
updatePasswordInput msg ({ loginDialog } as model) =
    let
        ( passwordInput, cmd ) =
            Ui.Input.update msg loginDialog.passwordInput
    in
    ( { loginDialog | passwordInput = passwordInput }
        |> asStateIn model
    , cmd
        |> Cmd.map (Model.LoginDialogMsg << PasswordInput)
    )
