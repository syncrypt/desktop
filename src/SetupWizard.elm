module SetupWizard exposing (settings, viewSettings)

import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import Model
import WizardDialog.Model exposing (..)


settings : Model.Model -> WizardSettings Model.Msg
settings model =
    { address = Model.WizardDialogMsg
    , onFinishMsg = Just Model.SetupWizardFinished
    , steps = 2
    , wizardType = SetupWizard
    }


viewSettings : Model.Model -> State Model.Msg -> Maybe (ViewSettings Model.Msg)
viewSettings model state =
    let
        wizardContent body =
            div [ class "MainScreen-SetupWizard" ]
                body
    in
        case state.currentStep of
            1 ->
                Just
                    { title = "Welcome to Syncrypt"
                    , contents =
                        wizardContent
                            [ text "We'll guide you through a step-by-step setup process to initiate your Syncrypt account." ]
                    , buttons = Default
                    }

            2 ->
                Just
                    { title = "Account setup"
                    , contents =
                        wizardContent
                            [ text "Setup your account here" ]
                    , buttons = Default
                    }

            _ ->
                Nothing
