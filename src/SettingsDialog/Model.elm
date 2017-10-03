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


type alias State msg =
    { toMsg : Msg -> msg
    , hasChangesPending : Bool
    , confirmationDialog : ConfirmationDialog.Model Msg
    , modal : Ui.Modal.Model
    }


type Msg
    = ConfirmationDialogMsg ConfirmationDialog.Msg
    | Close
    | LanguageSelection Translation.Language
    | ModalMsg Ui.Modal.Msg


type alias HasSettingsDialog a msg =
    { a
        | settingsDialog : State msg
        , language : Translation.Language
    }


init : (Msg -> msg) -> State msg
init toMsg =
    { toMsg = toMsg
    , hasChangesPending = False
    , confirmationDialog = ConfirmationDialog.init ConfirmationDialogMsg
    , modal =
        Ui.Modal.init
            |> Ui.Modal.closable True
            |> Ui.Modal.backdrop True
    }


open : HasSettingsDialog a msg -> HasSettingsDialog a msg
open ({ settingsDialog } as model) =
    settingsDialog.modal
        |> Ui.Modal.open
        |> asModalIn settingsDialog
        |> asStateIn model


close : HasSettingsDialog a msg -> HasSettingsDialog a msg
close ({ settingsDialog } as model) =
    settingsDialog.modal
        |> Ui.Modal.close
        |> asModalIn settingsDialog
        |> asStateIn model


asStateIn : HasSettingsDialog a msg -> State msg -> HasSettingsDialog a msg
asStateIn model state =
    { model | settingsDialog = state }
