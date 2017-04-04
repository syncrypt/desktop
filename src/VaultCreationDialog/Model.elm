module VaultCreationDialog.Model exposing (..)

import Ui.Modal
import Ui.Input


type alias State =
    { title : String
    , modal : Ui.Modal.Model
    , nameInput : Ui.Input.Model
    }


type Msg
    = Modal Ui.Modal.Msg
    | NameInput Ui.Input.Msg


init : State
init =
    { title = "Untitled Vault"
    , modal =
        Ui.Modal.init
            |> Ui.Modal.closable True
            |> Ui.Modal.backdrop False
    , nameInput =
        Ui.Input.init ()
            |> Ui.Input.placeholder "Vault Name"
            |> Ui.Input.showClearIcon True
    }
