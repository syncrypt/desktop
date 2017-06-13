module LoginDialog exposing (..)

import Dialog exposing (labeledItem)
import Util exposing (Direction(..))
import Html exposing (Html, button, div, form, input, label, span, text)
import Html.Attributes exposing (class, classList, for, id, style)
import Model exposing (Model)
import Ui.Button
import Ui.Input
import Ui.Modal
import LoginDialog.Model exposing (State, Msg(..))


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
            { address = (Model.LoginDialog << Modal)
            , contents = contents state
            , footer = []
            , title = "Login"
            }
    in
        div [ class "LoginDialog" ]
            [ Ui.Modal.view viewConfig modalState ]


contents : State -> List (Html Model.Msg)
contents state =
    [ div [ class "Tab-Content" ]
        [ dialogInput <| emailInput state
        , dialogInput <| passwordInput state
        ]
    , div [ class "Buttons" ]
        [ loginButton ]
    ]


dialogInput : Html msg -> Html msg
dialogInput body =
    div [ class "Input" ]
        [ body ]


loginButton : Html Model.Msg
loginButton =
    span [ class "Button-Login" ]
        [ Ui.Button.model "Login" "primary" "small"
            |> Ui.Button.view Model.Login
        ]


emailInput : State -> Html Model.Msg
emailInput state =
    labeledItem Left
        []
        (Just (Model.FocusOn state.emailInput.uid))
        (text "E-Mail")
        (Ui.Input.view state.emailInput
            |> Html.map (Model.LoginDialog << EmailInput)
        )


passwordInput : State -> Html Model.Msg
passwordInput state =
    labeledItem Left
        []
        (Just (Model.FocusOn state.passwordInput.uid))
        (text "Password")
        (Ui.Input.view state.passwordInput
            |> Html.map (Model.LoginDialog << PasswordInput)
        )
