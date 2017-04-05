module VaultDialog.Model exposing (..)

import Syncrypt.Vault exposing (Vault)
import Ui.Modal
import Ui.Input


type alias State =
    { modal : Ui.Modal.Model
    , nameInput : Ui.Input.Model
    }


type Msg
    = Modal Ui.Modal.Msg
    | NameInput Ui.Input.Msg


init : State
init =
    { modal =
        Ui.Modal.init
            |> Ui.Modal.closable True
            |> Ui.Modal.backdrop False
    , nameInput =
        Ui.Input.init ()
            |> Ui.Input.showClearIcon True
    }
