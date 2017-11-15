module FeedbackWizard exposing (settings, stepSettings)

import Html exposing (Html, div, text)
import Html.Attributes exposing (class, style, placeholder, rows, cols)
import Html.Events exposing (onInput)
import Model
import WizardDialog.Model exposing (..)


settings : Model.Model -> WizardSettings Model.Msg
settings model =
    { address = Model.WizardDialogMsg
    , onFinishMsg = Just Model.SendFeedback
    , steps = 1
    , wizardType = FeedbackWizard
    }


stepSettings : Model.Model -> State Model.Msg -> Maybe (StepSettings Model.Msg)
stepSettings model state =
    Just
        { title = "Send us feedback"
        , contents =
            div [ class "FeedbackWizard" ]
                [ div [ class "Label" ]
                    [ text "Your feedback, suggestions or bug report:" ]
                , div []
                    [ Html.textarea
                        [ class "FeedbackTextArea"
                        , cols 40
                        , rows 10
                        , placeholder "Type your feedback here"
                        , onInput Model.FeedbackEntered
                        ]
                        []
                    ]
                ]
        , buttons =
            Visible
                [ Cancel
                , CustomButton [ style [ ( "float", "right" ) ] ]
                    "Send Feedback"
                    Model.SendFeedback
                ]
        }
