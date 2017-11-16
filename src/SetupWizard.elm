module SetupWizard exposing (settings, viewSettings)

import Dialog exposing (labeledItem)
import Html exposing (Html, button, div, input, p, span, text)
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


infoText : List String -> Html msg
infoText lines =
    let
        lineItem line =
            span []
                [ text line ]
    in
    div [ class "InfoText" ]
        (List.map lineItem lines)


step1 model state =
    Just
        { title = "Welcome to Syncrypt"
        , contents =
            wizardContent
                [ infoText
                    [ "We'll guide you through a step-by-step setup process to initiate your Syncrypt account."
                    , "Please pick a language:"
                    ]
                , div [ class "Options" ]
                    [ button []
                        { label = "GERMAN"
                        , onClick = Model.SetLanguage German
                        }
                    , button []
                        { label = "ENGLISH"
                        , onClick = Model.SetLanguage English
                        }
                    ]
                ]
        , buttons = Default
        }


step2 model state =
    Just
        { title = "Account setup"
        , contents =
            wizardContent
                [ infoText [ "Do you already have a Syncrypt Account?" ]
                , div [ class "Options" ]
                    [ button []
                        { label = "Yes, login with account"
                        , onClick = state.address (ToStep 3)
                        }
                    , button []
                        { label = "No, sign up with new account"
                        , onClick = state.address (ToStep 4)
                        }
                    ]
                ]
        , buttons = Default
        }


step3 model state =
    Just
        { title = "Account Login"
        , contents =
            wizardContent
                [ infoText [ "Login with your existing Syncrypt Account." ]
                , div [ class "Options" ]
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
                    , button [ class "ForgotPassswordButton" ]
                        { label = "Forgot Password"
                        , onClick = state.address <| ToStep 6
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


type alias ButtonSettings =
    { label : String
    , onClick : Model.Msg
    }


button : List (Html.Attribute Model.Msg) -> ButtonSettings -> Html Model.Msg
button attrs { label, onClick } =
    span attrs
        [ Ui.Button.model label "secondary" "small"
            |> Ui.Button.view onClick
        ]
