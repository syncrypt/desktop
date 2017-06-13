module LoginDialog.Update exposing (update)

import Ui.Input
import LoginDialog.Model exposing (State, Msg(..))
import Model exposing (Model)


update : Msg -> Model -> ( Model, Cmd Model.Msg )
update dialogMsg model =
    case dialogMsg of
        EmailInput msg ->
            let
                ( emailInput, cmd ) =
                    Ui.Input.update msg model.loginDialog.emailInput

                loginDialog =
                    model.loginDialog
            in
                ({ model | loginDialog = { loginDialog | emailInput = emailInput } })
                    ! []

        PasswordInput msg ->
            let
                ( passwordInput, cmd ) =
                    Ui.Input.update msg model.loginDialog.passwordInput

                loginDialog =
                    model.loginDialog
            in
                ({ model | loginDialog = { loginDialog | passwordInput = passwordInput } })
                    ! []

        Modal _ ->
            model
                ! []
