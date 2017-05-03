module LoginDialog.Model exposing (Msg(..), State, init)

import Ui.Button
import Ui.Container
import Ui.Input
import Ui.Modal
import Ui.Tabs


type alias State =
    { emailInput : Ui.Input.Model
    , passwordInput : Ui.Input.Model
    }


type Msg
    = Modal Ui.Modal.Msg
    | EmailInput Ui.Input.Msg
    | PasswordInput Ui.Input.Msg


init : State
init =
    { emailInput = Ui.Input.init ()
    , passwordInput = Ui.Input.init ()
    }
