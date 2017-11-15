module SetupWizard exposing (settings, viewSettings)

import Dialog exposing (labeledItem)
import Util exposing (Position(..))
import Html exposing (Html, div, text, button, input)
import Html.Attributes exposing (class, type_)
import Html.Events exposing (onClick)
import Model
import Language exposing (Language(..))
import WizardDialog.Model exposing (..)


settings : Model.Model -> WizardSettings Model.Msg
settings model =
    { address = Model.WizardDialogMsg
    , onFinishMsg = Just Model.SetupWizardFinished
    , steps = 4
    , wizardType = SetupWizard
    }


wizardContent : List (Html msg) -> Html msg
wizardContent body =
    div [ class "MainScreen-SetupWizard" ]
        body


viewSettings : Model.Model -> State Model.Msg -> Maybe (ViewSettings Model.Msg)
viewSettings model state =
    case state.currentStep of
        1 ->
            step1 model state

        2 ->
            step2 model state

        3 ->
            step3 model state

        4 ->
            step4 model state

        _ ->
            Nothing


step1 model state =
    Just
        { title = "Welcome to Syncrypt"
        , contents =
            wizardContent
                [ text "We'll guide you through a step-by-step setup process to initiate your Syncrypt account."
                , text "Please pick a language:"
                , button [ onClick <| Model.SetLanguage German ]
                    [ text "GERMAN" ]
                , button [ onClick <| Model.SetLanguage English ]
                    [ text "ENGLISH" ]
                ]
        , buttons = Default
        }


step2 model state =
    Just
        { title = "Account setup"
        , contents =
            wizardContent
                [ text "Do you already have a Syncrypt Account?"
                , button [ onClick <| state.address (ToStep 3) ]
                    [ text "Yes, login with account" ]
                , button [ onClick <| state.address (ToStep 4) ]
                    [ text "No, sign up with new account" ]
                ]
        , buttons = Default
        }


step3 model state =
    Just
        { title = "Account Login"
        , contents =
            wizardContent
                [ labeledItem [ class "InputLabel" ]
                    { side = Left
                    , onClick = Nothing
                    , label = text "Email"
                    , item =
                        div []
                            [ input [ type_ "email" ] [ text "Your Email" ] ]
                    }
                , labeledItem [ class "InputLabel" ]
                    { side = Left
                    , onClick = Nothing
                    , label = text "Password"
                    , item =
                        div []
                            [ input [ type_ "password" ] [ text "" ] ]
                    }
                ]
        , buttons = Default
        }


step4 model state =
    Just
        { title = "Account Signup"
        , contents =
            wizardContent []
        , buttons = Default
        }
