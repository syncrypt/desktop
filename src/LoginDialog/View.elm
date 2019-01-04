module LoginDialog.View exposing (view)

import Dialog exposing (labeledItem)
import Html exposing (Html, div, span, text)
import Html.Attributes exposing (class)
import Language exposing (HasLanguage)
import LoginDialog.Model exposing (HasLoginDialog, Msg(..), State)
import Model exposing (Model)
import Translation as T
import Ui.Input
import Ui.Modal
import Util exposing (Position(..), button, onEnter)


t : T.LoginDialogText -> HasLanguage a -> String
t txt model =
    T.t (T.LoginDialogTxt txt) model


view : Model -> Html Model.Msg
view model =
    let
        modalState =
            { closable = False
            , backdrop = True
            , open = model.login == Model.LoggedOut
            }

        viewConfig =
            { address = Model.LoginDialogMsg << Modal
            , contents = contents model
            , footer = []
            , title = "Login"
            }
    in
    div [ class "LoginDialog" ]
        [ Ui.Modal.view viewConfig modalState ]


contents : HasLoginDialog a -> List (Html Model.Msg)
contents model =
    [ div [ class "Tab-Content" ]
        [ Dialog.input <| emailInput model
        , Dialog.input <| passwordInput model
        ]
    , div [ class "Errors" ] <|
        loginError model.loginDialog
    , div [ class "Buttons" ]
        [ loginButton
        , resetPasswordButton model
        ]
    ]


loginError : State -> List (Html msg)
loginError { loginError } =
    loginError
        |> Maybe.map Dialog.errorMsg
        |> Maybe.map List.singleton
        |> Maybe.withDefault []


loginButton : Html Model.Msg
loginButton =
    span [ class "Button-Login" ]
        [ button []
            { label = "Login"
            , onClick = Model.Login
            }
        ]


emailInput : HasLoginDialog a -> Html Model.Msg
emailInput model =
    let
        state =
            model.loginDialog
    in
    labeledItem []
        { side = Left
        , onClick = Just <| Model.FocusOn state.emailInput.uid
        , label = text <| t T.LoginEmail model
        , tooltipText = Nothing
        , item =
            span [ onEnter <| Model.FocusOn state.passwordInput.uid ]
                [ state.emailInput
                    |> Ui.Input.view
                    |> Html.map (Model.LoginDialogMsg << EmailInput)
                ]
        }


passwordInput : HasLoginDialog a -> Html Model.Msg
passwordInput model =
    let
        state =
            model.loginDialog
    in
    labeledItem []
        { side = Left
        , onClick = Just (Model.FocusOn state.passwordInput.uid)
        , label = text <| t T.LoginPassword model
        , tooltipText = Nothing
        , item =
            span [ onEnter Model.Login ]
                [ state.passwordInput
                    |> Ui.Input.view
                    |> Html.map (Model.LoginDialogMsg << PasswordInput)
                ]
        }


resetPasswordButton : HasLoginDialog a -> Html Model.Msg
resetPasswordButton model =
    button []
        { label = T.t (T.SettingsDialogTxt T.ResetPassword) model
        , onClick = Model.LoginDialogMsg OpenPasswordResetPage
        }
