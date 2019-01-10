module ReleaseNotesWizard exposing (settings, viewSettings)

import Html exposing (div, text)
import Html.Attributes exposing (class, style)
import Model
import Translation as T
import WizardDialog.Model
    exposing
        ( Button(..)
        , ButtonSettings(..)
        , State
        , ViewSettings
        , WizardSettings
        , WizardType(..)
        )


settings : Model.Model -> WizardSettings Model.Msg
settings _ =
    { address = Model.WizardDialogMsg
    , onFinishMsg = Just Model.SendFeedback
    , steps = [ "Release Notes" ]
    , wizardType = ReleaseNotesWizard
    , closable = True
    }


viewSettings : State Model.Msg -> Model.Model -> Maybe (ViewSettings Model.Msg)
viewSettings _ model =
    let
        txt t =
            T.t (T.ReleaseNotesWizardTxt t) model
    in
    Just
        { title = txt T.ReleaseNotesTitle
        , contents =
            div [ class "ReleaseNotesWizard" ]
                -- TODO: fetch release notes from somewhere
                [ text "Release Notes Here" ]
        , buttons = Visible [ CloseBtn (Just Model.CloseReleaseNotesWizard) ]
        }
