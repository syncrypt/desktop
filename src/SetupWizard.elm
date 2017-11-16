module SetupWizard exposing (settings, viewSettings)

import Dialog exposing (labeledItem)
import Html exposing (Html, button, div, input, span, text)
import Html.Attributes exposing (class, type_)
import Html.Events exposing (onClick)
import Language exposing (Language(..))
import Model
import Ui.Button
import Util exposing (Position(..))
import WizardDialog.Model exposing (..)


settings : Model.Model -> WizardSettings Model.Msg
settings model =
    { address = Model.WizardDialogMsg
    , onFinishMsg = Just Model.SetupWizardFinished
    , steps = 5
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

        5 ->
            step5 model state

        _ ->
            Nothing


step1 model state =
    Just
        { title = "Welcome to Syncrypt"
        , contents =
            wizardContent
                [ text "We'll guide you through a step-by-step setup process to initiate your Syncrypt account."
                , text "Please pick a language:"
                , div [ class "Options" ]
                    [ button []
                        "GERMAN"
                        (Model.SetLanguage German)
                    , button []
                        "ENGLISH"
                        (Model.SetLanguage English)
                    ]
                ]
        , buttons = Default
        }


step2 model state =
    Just
        { title = "Account setup"
        , contents =
            wizardContent
                [ text "Do you already have a Syncrypt Account?"
                , div [ class "Options" ]
                    [ button []
                        "Yes, login with account"
                        (state.address (ToStep 3))
                    , button []
                        "No, sign up with new account"
                        (state.address (ToStep 4))
                    ]
                ]
        , buttons = Default
        }


step3 model state =
    Just
        { title = "Account Login"
        , contents =
            wizardContent
                [ div [ class "Options" ]
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
                ]
        , buttons =
            CustomNav
                { prev = Auto
                , next = Nav <| state.address (ToStep 5)
                }
        }


step4 model state =
    Just
        { title = "Account Signup"
        , contents =
            wizardContent [ text "Here we'll have an account signup form with email address, user name, EULA acceptance checkbox etc." ]
        , buttons = Default
        }


step5 model state =
    Just
        { title = "Key Creation"
        , contents =
            wizardContent [ text "Coming soon with a nice animation next to this text." ]
        , buttons =
            CustomNav
                { prev = Nav <| state.address (ToStep 3)
                , next = Auto
                }
        }


button : List (Html.Attribute Model.Msg) -> String -> Model.Msg -> Html Model.Msg
button attrs label msg =
    span attrs
        [ Ui.Button.model label "secondary" "small"
            |> Ui.Button.view msg
        ]
