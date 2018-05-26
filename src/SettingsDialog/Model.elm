module SettingsDialog.Model
    exposing
        ( HasSettingsDialog
        , Msg(..)
        , State
        , asStateIn
        , close
        , hasChanged
        , hasNotChanged
        , init
        , open
        )

import Config exposing (Config)
import ConfirmationDialog
import Dialog exposing (asModalIn)
import Language exposing (Language(..))
import Ui.Input
import Ui.Modal


type alias State =
    { hasChangesPending : Bool
    , confirmationDialog : ConfirmationDialog.Model Msg
    , modal : Ui.Modal.Model
    , showChangePasswordForm : Bool
    , oldPasswordInput : Ui.Input.Model
    , newPasswordInput : Ui.Input.Model
    , newPasswordConfirmationInput : Ui.Input.Model
    }


type Msg
    = ConfirmationDialogMsg ConfirmationDialog.Msg
    | Close
    | LanguageSelection Language
    | ModalMsg Ui.Modal.Msg
    | ToggleChangePasswordForm
    | OpenPasswordResetPage
    | OldPasswordInputMsg Ui.Input.Msg
    | NewPasswordInputMsg Ui.Input.Msg
    | NewPasswordConfirmationInputMsg Ui.Input.Msg
    | ConfirmChangePassword


type alias HasSettingsDialog a =
    { a
        | settingsDialog : State
        , language : Language
        , isFirstLaunch : Bool
        , config : Config
        , updateAvailable : Maybe String
        , autoStartEnabled : Bool
    }


init : State
init =
    { hasChangesPending = False
    , confirmationDialog = ConfirmationDialog.init ConfirmationDialogMsg
    , modal =
        Ui.Modal.init
            |> Ui.Modal.closable True
            |> Ui.Modal.backdrop True
    , showChangePasswordForm = False
    , oldPasswordInput =
        Ui.Input.init ()
            |> Ui.Input.kind "password"
    , newPasswordInput =
        Ui.Input.init ()
            |> Ui.Input.kind "password"
    , newPasswordConfirmationInput =
        Ui.Input.init ()
            |> Ui.Input.kind "password"
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


hasChanged : HasSettingsDialog a -> HasSettingsDialog a
hasChanged ({ settingsDialog } as model) =
    { settingsDialog | hasChangesPending = True }
        |> asStateIn model


hasNotChanged : HasSettingsDialog a -> HasSettingsDialog a
hasNotChanged ({ settingsDialog } as model) =
    { settingsDialog | hasChangesPending = False }
        |> asStateIn model


asStateIn : HasSettingsDialog a -> State -> HasSettingsDialog a
asStateIn model state =
    { model | settingsDialog = state }
