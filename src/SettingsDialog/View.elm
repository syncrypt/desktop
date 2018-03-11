module SettingsDialog.View exposing (view)

import Dialog exposing (labeledItem)
import Html exposing (Html, div, span, text)
import Html.Attributes exposing (class, classList)
import Language exposing (HasLanguage, Language(..))
import Model
import SettingsDialog.Model exposing (HasSettingsDialog, Msg(..))
import Translation as T
import Ui.Input
import Ui.Modal
import Util exposing (Position(..), button)


view : HasSettingsDialog a -> Html Model.Msg
view ({ settingsDialog } as model) =
    let
        viewConfig =
            { address = Model.SettingsDialogMsg << ModalMsg
            , contents = contents model
            , footer = []
            , title = T.t T.ProgramSettings model
            }
    in
    div [ class "SettingsDialog" ]
        [ Ui.Modal.view viewConfig settingsDialog.modal ]


languageButton : Language -> HasSettingsDialog a -> Html Model.Msg
languageButton forLanguage { language, settingsDialog } =
    let
        onClickMsg =
            Model.SettingsDialogMsg <| LanguageSelection forLanguage
    in
    span
        [ classList
            [ ( "Button", True )
            , ( "Active", language == forLanguage )
            ]
        ]
        [ button []
            { label = toString forLanguage
            , onClick = onClickMsg
            }
        ]


contents : HasSettingsDialog a -> List (Html Model.Msg)
contents model =
    [ div [ class "InfoLabel" ]
        [ text <| dialogText T.ChooseYourLanguage model ]
    , languageButton German model
    , languageButton English model
    , separator
    , div [ class "InfoLabel" ]
        [ text <| dialogText T.AccountOptions model ]
    , changePasswordButton model
    , resetPasswordButton model
    , separator
    , changePasswordForm model
    ]


changePasswordButton : HasSettingsDialog a -> Html Model.Msg
changePasswordButton model =
    button [ class "Button" ]
        { label = dialogText T.ChangePassword model
        , onClick = Model.SettingsDialogMsg ToggleChangePasswordForm
        }


resetPasswordButton : HasSettingsDialog a -> Html Model.Msg
resetPasswordButton model =
    button [ class "Button" ]
        { label = dialogText T.ResetPassword model
        , onClick = Model.SettingsDialogMsg OpenPasswordResetPage
        }


changePasswordForm : HasSettingsDialog a -> Html Model.Msg
changePasswordForm ({ settingsDialog } as model) =
    div
        [ classList
            [ ( "Hidden", not model.settingsDialog.showChangePasswordForm )
            , ( "ChangePasswordForm", True )
            ]
        ]
        [ passwordInput Old model
        , passwordInput New model
        , passwordInput NewConfirmation model
        , buttons model
        ]


type PasswordInputType
    = Old
    | New
    | NewConfirmation


passwordInput : PasswordInputType -> HasSettingsDialog a -> Html Model.Msg
passwordInput inputType model =
    let
        ( input, inputMsg, labelText, tooltipText ) =
            case inputType of
                Old ->
                    ( model.settingsDialog.oldPasswordInput
                    , OldPasswordInputMsg
                    , T.OldPasswordLabel
                    , T.OldPasswordTooltip
                    )

                New ->
                    ( model.settingsDialog.newPasswordInput
                    , NewPasswordInputMsg
                    , T.NewPasswordLabel
                    , T.NewPasswordTooltip
                    )

                NewConfirmation ->
                    ( model.settingsDialog.newPasswordConfirmationInput
                    , NewPasswordConfirmationInputMsg
                    , T.NewPasswordConfirmationLabel
                    , T.NewPasswordConfirmationTooltip
                    )
    in
    labeledItem [ class "InputLabel" ]
        { side = Top
        , onClick = Just (Model.FocusOn input.uid)
        , label = text <| dialogText labelText model
        , item =
            Util.tooltipItem
                { position = Right
                , length = Util.Medium
                , text = dialogText tooltipText model
                }
                [ Ui.Input.view
                    input
                    |> Html.map (Model.SettingsDialogMsg << inputMsg)
                ]
        }


buttons : HasSettingsDialog a -> Html Model.Msg
buttons model =
    div
        [ classList
            [ ( "Buttons", True )
            , ( "Hidden", not model.settingsDialog.hasChangesPending )
            ]
        ]
        [ button [ class "Button" ]
            { label = dialogText T.ChangePassword model
            , onClick = Model.SettingsDialogMsg ConfirmChangePassword
            }
        , button [ class "Button" ]
            { label = T.t T.Cancel model
            , onClick = Model.SettingsDialogMsg ToggleChangePasswordForm
            }
        ]


dialogText : T.SettingsDialogText -> HasLanguage a -> String
dialogText dialogText model =
    T.t (T.SettingsDialogText dialogText) model


separator : Html msg
separator =
    Html.hr [ class "Separator" ]
        []
