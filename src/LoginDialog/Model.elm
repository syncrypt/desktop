module LoginDialog.Model
    exposing
        ( HasLoginDialog
        , Msg(..)
        , State
        , asStateIn
        , init
        , loginFailed
        , loginSucceeded
        )

import Http
import Language exposing (Language)
import Ui.Input
import Ui.Modal


type alias State =
    { emailInput : Ui.Input.Model
    , passwordInput : Ui.Input.Model
    , loginError : Maybe String
    }


type Msg
    = Modal Ui.Modal.Msg
    | EmailInput Ui.Input.Msg
    | PasswordInput Ui.Input.Msg
    | OpenPasswordResetPage


type alias HasLoginDialog a =
    { a | loginDialog : State, language : Language }


init : State
init =
    { emailInput = Ui.Input.init ()
    , passwordInput =
        Ui.Input.init ()
            |> Ui.Input.kind "password"
    , loginError = Nothing
    }


asStateIn : HasLoginDialog a -> State -> HasLoginDialog a
asStateIn model state =
    { model | loginDialog = state }


loginFailed : Http.Error -> HasLoginDialog a -> HasLoginDialog a
loginFailed reason ({ loginDialog } as model) =
    let
        _ =
            Debug.log "Login Error:" reason
    in
    { loginDialog | loginError = Just <| errorMsg reason }
        |> asStateIn model


loginSucceeded : HasLoginDialog a -> HasLoginDialog a
loginSucceeded ({ loginDialog } as model) =
    { loginDialog | loginError = Nothing }
        |> asStateIn model


errorMsg : Http.Error -> String
errorMsg err =
    case err of
        Http.Timeout ->
            "Daemon connection timed out."

        Http.NetworkError ->
            "Network error."

        Http.BadStatus _ ->
            "Login failed."

        e ->
            let
                _ =
                    Debug.log "Unexpected login error: " e
            in
            "Unknown error while talking to Syncrypt background process."
