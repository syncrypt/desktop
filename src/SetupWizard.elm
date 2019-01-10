module SetupWizard exposing (settings, viewSettings)

import Data.Daemon exposing (KeyState(..))
import Dialog exposing (labeledItem)
import Html exposing (Html, div, input, p, span, text)
import Html.Attributes exposing (class, type_)
import Html.Events exposing (onClick, onInput)
import Language exposing (HasLanguage, Language(..))
import Model
import RemoteData
import Translation as T
import Util exposing (ButtonSettings, Position(..), button)
import WizardDialog.Model exposing (..)
import WizardDialog.View
    exposing
        ( infoText
        , infoTextWithHeader
        , infoTextWithHeaders
        )


steps : List (StepConfig Model.Model Model.Msg)
steps =
    [ ( "Welcome", step1 )
    , ( "Account Setup", step2 )
    , ( "Account Login", step3 )
    , ( "Account Signup", step4 )
    , ( "Key Creation", step5 )
    ]


settings : Model.Model -> WizardSettings Model.Msg
settings model =
    { address = Model.WizardDialogMsg
    , onFinishMsg = Just Model.SetupWizardFinished
    , steps = steps |> List.map Tuple.first
    , wizardType = SetupWizard
    , closable = False
    }


wizardContent : List (Html msg) -> Html msg
wizardContent body =
    div [ class "MainScreen-SetupWizard" ]
        body


viewSettings : State Model.Msg -> Model.Model -> Maybe (ViewSettings Model.Msg)
viewSettings state model =
    WizardDialog.Model.viewSettings steps state model


t : T.SetupWizardText -> HasLanguage a -> String
t text model =
    T.t (T.SetupWizardTxt text) model



-- STEPS


step1 model state =
    Just
        { title = t T.WelcomeToSyncrypt model
        , contents =
            wizardContent
                [ infoText []
                    [ t T.WelcomeHeader1 model
                    , t T.WelcomeHeader2 model
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
        , buttons = DefaultNoCancel
        }


step2 model state =
    Just
        { title = t T.AccountSetup model
        , contents =
            wizardContent
                [ infoTextWithHeader []
                    (t T.DoYouAlreadyHaveASyncryptAccount model)
                    [ t T.YouCanLoginWithAnExistingAccountOrCreateANewOne model ]
                , div [ class "Options" ]
                    [ button []
                        { label = t T.LoginWithAccount model
                        , onClick = state.address (ToStepWithName "Account Login")
                        }
                    , button []
                        { label = t T.SignUpWithNewAccount model
                        , onClick = state.address (ToStepWithName "Account Signup")
                        }
                    ]
                ]
        , buttons = DefaultNoCancel
        }


step3 : Model.Model -> State Model.Msg -> Maybe (ViewSettings Model.Msg)
step3 model state =
    Just
        { title = t T.AccountLogin model
        , contents =
            wizardContent
                [ infoTextWithHeader []
                    (t T.LoginWithYourAccount model)
                    [ t T.IfYouForgotYourPassword model
                    , t T.WeWillSendYouAPasswordResetLink model
                    ]
                , div [ class "Options" ]
                    [ labeledItem [ class "InputLabel" ]
                        { side = Left
                        , onClick = Nothing
                        , label = text "Email"
                        , item =
                            div []
                                [ input [ type_ "email", onInput Model.SetupWizardEmail ]
                                    [ text <| t T.YourEmail model ]
                                ]
                        }
                    , labeledItem [ class "InputLabel" ]
                        { side = Left
                        , onClick = Nothing
                        , label = text <| t T.Password model
                        , item =
                            div []
                                [ input [ type_ "password", onInput Model.SetupWizardPassword ]
                                    [ text "" ]
                                ]
                        }
                    , button [ class "ForgotPasswordButton" ]
                        { label = t T.ForgotPassword model
                        , onClick = Model.SendPasswordResetLink
                        }
                    , if model.setupWizard.passwordResetSent then
                        div []
                            [ text <| t T.PasswordResetLinkHasBeenSent model ]
                      else
                        text ""
                    ]
                ]
        , buttons =
            CustomNavNoCancel
                { prev = Auto
                , next = NavWithLabel (state.address (ToStepWithName "Key Creation")) (t T.Login model)
                }
        }


step4 model state =
    Just
        { title = t T.AccountSignup model
        , contents =
            wizardContent
                [ infoTextWithHeaders [ class "TermsOfService" ]
                    (t T.LegalNotice model)
                    (t T.PleaseReadAndConfirm model)
                    (List.map (flip t model)
                        [ T.TOS1
                        , T.TOS2
                        , T.TOS3
                        , T.TOS4
                        , T.TOS5
                        , T.TOS6
                        , T.TOS7
                        , T.TOS8
                        , T.TOS9
                        , T.TOS10
                        , T.TOS11
                        ]
                    )
                ]
        , buttons =
            CustomNavNoCancel
                { prev = Nav <| state.address (ToStepWithName "Account Setup")
                , next = AutoWithLabel <| t T.IAgree model
                }
        }


step5 model state =
    let
        keyStateText keyState =
            case keyState of
                Uninitialized ->
                    text <| t T.KeyNotYetInitialized model

                Initializing ->
                    text <| t T.InitializingKey model

                Initialized ->
                    text <| t T.KeySuccessfullyInitialized model

        defaultText =
            text <| t T.Updating model
    in
    Just
        { title = t T.KeyCreation model
        , contents =
            wizardContent
                [ model.stats
                    |> RemoteData.map .userKeyState
                    |> RemoteData.map keyStateText
                    |> RemoteData.withDefault defaultText
                ]
        , buttons =
            CustomNavNoCancel
                { prev = Hidden
                , next = Hidden
                }
        }
