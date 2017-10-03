module SettingsDialog.Model
    exposing
        ( State
        , HasSettingsDialog
        , Msg(..)
        , init
        , open
        , close
        , asStateIn
        )

import ConfirmationDialog
import Translation
import Ui.Modal
import Dialog exposing (asModalIn)


type alias State =
    { hasChangesPending : Bool
    , confirmationDialog : ConfirmationDialog.Model Msg
    , modal : Ui.Modal.Model
    }


type Msg
    = ConfirmationDialogMsg ConfirmationDialog.Msg
    | Close
    | LanguageSelection Translation.Language
    | ModalMsg Ui.Modal.Msg


type alias HasSettingsDialog a =
    { a
        | settingsDialog : State
        , language : Translation.Language
    }


init : State
init =
    { hasChangesPending = False
    , confirmationDialog = ConfirmationDialog.init ConfirmationDialogMsg
    , modal =
        Ui.Modal.init
            |> Ui.Modal.closable True
            |> Ui.Modal.backdrop True
    }


open : HasSettingsDialog a -> HasSettingsDialog a
open ({ settingsDialog } as model) =
    settingsDialog.modal
        |> Ui.Modal.open
        |> asModalIn settingsDialog
        |> asStateIn model


close : HasSettingsDialog a -> HasSettingsDialog a
close ({ settingsDialog } as model) =
    settingsDialog.modal
        |> Ui.Modal.close
        |> asModalIn settingsDialog
        |> asStateIn model


asStateIn : HasSettingsDialog a -> State -> HasSettingsDialog a
asStateIn model state =
    { model | settingsDialog = state }
