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
                            [ text "We'll guide you through a step-by-step setup process to initiate your Syncrypt account."
                            , text "Please pick a language:"
                            , button [ onClick <| Model.SetLanguage German ]
                                [ text "GERMAN" ]
                            , button [ onClick <| Model.SetLanguage English ]
                                [ text "ENGLISH" ]
                            ]
                    , buttons = Default
                    }

            2 ->
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

            3 ->
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

            4 ->
                Just
                    { title = "Account Signup"
                    , contents =
                        wizardContent []
                    , buttons = Default
                    }

            _ ->
                Nothing
