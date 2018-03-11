module LoginDialog.Model
    exposing
        ( HasLoginDialog
        , Msg(..)
        , State
        , asStateIn
        , init
        )

import Language exposing (Language)
import Ui.Input
import Ui.Modal


type alias State =
    { emailInput : Ui.Input.Model
    , passwordInput : Ui.Input.Model
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
    }


asStateIn : HasLoginDialog a -> State -> HasLoginDialog a
asStateIn model state =
    { model | loginDialog = state }
