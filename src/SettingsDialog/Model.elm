module SettingsDialog.Model exposing (..)

import ConfirmationDialog


type alias State msg =
    { toMsg : Msg -> msg
    , hasChangesPending : Bool
    , confirmationDialog : ConfirmationDialog.Model Msg
    }


type Msg
    = ConfirmationDialogMsg ConfirmationDialog.Msg
    | Close


init : (Msg -> msg) -> State msg
init toMsg =
    { toMsg = toMsg
    , hasChangesPending = False
    , confirmationDialog = ConfirmationDialog.init ConfirmationDialogMsg
    }
