module SettingsDialog.Model
    exposing
        ( HasSettingsDialog
        , Msg(..)
        , State
        , asStateIn
        , close
        , init
        , open
        )

import ConfirmationDialog
import Dialog exposing (asModalIn)
import Language exposing (Language(..))
import Ui.Modal


type alias State =
    { hasChangesPending : Bool
    , confirmationDialog : ConfirmationDialog.Model Msg
    , modal : Ui.Modal.Model
    }


type Msg
    = ConfirmationDialogMsg ConfirmationDialog.Msg
    | Close
    | LanguageSelection Language
    | ModalMsg Ui.Modal.Msg


type alias HasSettingsDialog a =
    { a
        | settingsDialog : State
        , language : Language
        , isFirstLaunch : Bool
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
