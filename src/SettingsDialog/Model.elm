module SettingsDialog.Model
    exposing
        ( State
        , HasSettingsDialog
        , Msg(..)
        , init
        , open
        , close
        )

import ConfirmationDialog


type alias State msg =
    { toMsg : Msg -> msg
    , hasChangesPending : Bool
    , confirmationDialog : ConfirmationDialog.Model Msg
    }


type Msg
    = ConfirmationDialogMsg ConfirmationDialog.Msg
    | Close


type alias HasSettingsDialog a msg =
    { a | settingsDialog : State msg }


init : (Msg -> msg) -> State msg
init toMsg =
    { toMsg = toMsg
    , hasChangesPending = False
    , confirmationDialog = ConfirmationDialog.init ConfirmationDialogMsg
    }


open : HasSettingsDialog a msg -> HasSettingsDialog a msg
open ({ settingsDialog } as model) =
    { model
        | settingsDialog =
            ConfirmationDialog.open "Coming Soon"
                "Not yet implemented"
                Close
                settingsDialog
    }


close ({ settingsDialog } as model) =
    { model | settingsDialog = ConfirmationDialog.close settingsDialog }
