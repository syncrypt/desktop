module LoginDialog.Update exposing (update)

import LoginDialog.Model exposing (HasLoginDialog, Msg(..), State, asStateIn)
import Model exposing (Model)
import Ui.Input


update : Msg -> HasLoginDialog a -> ( HasLoginDialog a, Cmd Model.Msg )
update dialogMsg ({ loginDialog } as model) =
    case dialogMsg of
        EmailInput msg ->
            let
                ( emailInput, cmd ) =
                    Ui.Input.update msg loginDialog.emailInput
            in
            ({ loginDialog | emailInput = emailInput }
                |> asStateIn model
            )
                ! []

        PasswordInput msg ->
            let
                ( passwordInput, cmd ) =
                    Ui.Input.update msg loginDialog.passwordInput
            in
            ({ loginDialog | passwordInput = passwordInput }
                |> asStateIn model
            )
                ! []

        Modal _ ->
            model
                ! []
