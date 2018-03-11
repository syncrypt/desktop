module LoginDialog.View exposing (..)

import Dialog exposing (labeledItem)
import Html exposing (Html, div, form, input, label, span, text)
import Html.Attributes exposing (class, classList, for, id, style)
import LoginDialog.Model exposing (HasLoginDialog, Msg(..), State)
import Model exposing (Model)
import Translation as T
import Ui.Input
import Ui.Modal
import Util exposing (Position(..), button)


view : Model -> Html Model.Msg
view model =
    let
        state =
            { emailInput =
                Ui.Input.init ()
            , passwordInput =
                Ui.Input.init ()
                    |> Ui.Input.kind "password"
            }

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
        [ Dialog.input <| emailInput model.loginDialog
        , Dialog.input <| passwordInput model.loginDialog
        ]
    , div [ class "Buttons" ]
        [ loginButton
        , resetPasswordButton model
        ]
    ]


loginButton : Html Model.Msg
loginButton =
    span [ class "Button-Login" ]
        [ button []
            { label = "Login"
            , onClick = Model.Login
            }
        ]


emailInput : State -> Html Model.Msg
emailInput state =
    labeledItem []
        { side = Left
        , onClick = Just <| Model.FocusOn state.emailInput.uid
        , label = text "E-Mail"
        , item =
            state.emailInput
                |> Ui.Input.view
                |> Html.map (Model.LoginDialogMsg << EmailInput)
        }


passwordInput : State -> Html Model.Msg
passwordInput state =
    labeledItem []
        { side = Left
        , onClick = Just (Model.FocusOn state.passwordInput.uid)
        , label = text "Password"
        , item =
            state.passwordInput
                |> Ui.Input.view
                |> Html.map (Model.LoginDialogMsg << PasswordInput)
        }


resetPasswordButton : HasLoginDialog a -> Html Model.Msg
resetPasswordButton model =
    button []
        { label = T.t (T.SettingsDialogText T.ResetPassword) model
        , onClick = Model.LoginDialogMsg OpenPasswordResetPage
        }
