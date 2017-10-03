module LoginDialog.Model
    exposing
        ( Msg(..)
        , State
        , HasLoginDialog
        , init
        , asStateIn
        )

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


type alias HasLoginDialog a =
    { a | loginDialog : State }


init : State
init =
    { emailInput = Ui.Input.init ()
    , passwordInput = Ui.Input.init ()
    }


asStateIn : HasLoginDialog a -> State -> HasLoginDialog a
asStateIn model state =
    { model | loginDialog = state }
