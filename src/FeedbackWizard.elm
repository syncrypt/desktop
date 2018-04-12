module FeedbackWizard exposing (settings, viewSettings)

import Html exposing (div, text)
import Html.Attributes exposing (class, cols, placeholder, rows, style)
import Html.Events exposing (onInput)
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
    , name = "FeedbackWizard"
    , onFinishMsg = Just Model.SendFeedback
    , steps = [ "Send Feedback" ]
    , wizardType = FeedbackWizard
    , closable = True
    }


viewSettings : State Model.Msg -> Model.Model -> Maybe (ViewSettings Model.Msg)
viewSettings _ model =
    Just
        { title = T.t T.YourFeedback model
        , contents =
            div [ class "FeedbackWizard" ]
                [ div [ class "Label" ]
                    [ text <| T.t T.SendUsYourFeedbackSuggestionsOrBugReports model ]
                , div [ class "SubLabel" ]
                    [ text <| T.t T.WeWillReplyToYouViaEmail model ]
                , div []
                    [ Html.textarea
                        [ class "FeedbackTextArea"
                        , cols 40
                        , rows 10
                        , placeholder <| T.t T.TypeYourFeedbackHere model
                        , onInput Model.FeedbackEntered
                        ]
                        []
                    ]
                ]
        , buttons =
            Visible
                [ Cancel
                , CustomButton [ style [ ( "float", "right" ) ] ]
                    { label = T.t T.SendFeedback model
                    , onClick = Model.SendFeedback
                    }
                ]
        }
