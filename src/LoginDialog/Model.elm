module LoginDialog.Model
    exposing
        ( HasLoginDialog
        , Msg(..)
        , State
        , asStateIn
        , init
        , loginFailed
        , loginSucceeded
        , setEmailInput
        , setPasswordInput
        )

import Http
import Language exposing (Language)
import Translation exposing (LoginDialogText(..), Text(..), translate)
import Ui.Input
import Ui.Modal
import Util exposing (andLog)


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
    { a
        | loginDialog : State
        , language : Language
    }


init : State
init =
    { emailInput =
        inputWithValue ""
    , passwordInput =
        passwordInputWithValue ""
    , loginError = Nothing
    }


inputWithValue : String -> Ui.Input.Model
inputWithValue value =
    let
        input =
            Ui.Input.init ()
    in
    { input | value = value }


passwordInputWithValue : String -> Ui.Input.Model
passwordInputWithValue value =
    value
        |> inputWithValue
        |> Ui.Input.kind "password"


asStateIn : HasLoginDialog a -> State -> HasLoginDialog a
asStateIn model state =
    { model | loginDialog = state }


loginFailed : Http.Error -> HasLoginDialog a -> HasLoginDialog a
loginFailed reason ({ loginDialog } as model) =
    { loginDialog | loginError = Just <| errorMsg reason model }
        |> asStateIn model
        |> andLog "Login Error:" reason


loginSucceeded : HasLoginDialog a -> HasLoginDialog a
loginSucceeded ({ loginDialog } as model) =
    { loginDialog | loginError = Nothing }
        |> asStateIn model


errorMsg : Http.Error -> HasLoginDialog a -> String
errorMsg err { language } =
    let
        txt =
            case err of
                Http.Timeout ->
                    DaemonConnectionTimedOut

                Http.NetworkError ->
                    NetworkError

                Http.BadStatus resp ->
                    BadStatusOrLoginFailed resp

                e ->
                    UnknownError e
    in
    translate language (LoginDialogTxt txt)


setEmailInput : String -> HasLoginDialog a -> HasLoginDialog a
setEmailInput email ({ loginDialog } as model) =
    { loginDialog | emailInput = inputWithValue email }
        |> asStateIn model


setPasswordInput : String -> HasLoginDialog a -> HasLoginDialog a
setPasswordInput password ({ loginDialog } as model) =
    { loginDialog | passwordInput = passwordInputWithValue password }
        |> asStateIn model
