module SettingsDialog.View exposing (view)

import Dialog exposing (labeledItem)
import Html exposing (Html, div, span, text)
import Html.Attributes exposing (checked, class, classList, disabled, type_)
import Html.Events exposing (onClick)
import Language exposing (HasLanguage, Language(..))
import Model
import SettingsDialog.Model exposing (HasSettingsDialog, Msg(..))
import String
import Translation as T
import Ui.Input
import Ui.Modal
import Util exposing (Position(..), button, onEnter)


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
languageButton forLanguage { language } =
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
    let
        availableUpdate =
            case model.updateAvailable of
                Just version ->
                    div []
                        [ text <|
                            t T.UpdateIsAvailable model
                                ++ ": "
                                ++ version
                        ]

                Nothing ->
                    text ""
    in
    [ div [ class "InfoLabel" ]
        [ text <| t T.ChooseYourLanguage model ]
    , languageButton German model
    , languageButton English model
    , separator
    , div [ class "InfoLabel" ]
        [ text <| t T.KeyExportOrImport model ]
    , keyExportButton model
    , separator
    , div [ class "InfoLabel" ]
        [ text <| t T.AccountOptions model ]
    , changePasswordButton model
    , resetPasswordButton model
    , changePasswordForm model
    , separator
    , div [ class "InfoLabel" ]
        [ text <| t T.AutoStart model ]
    , autoStartCheckbox model
    , separator
    , div [ class "InfoLabel" ]
        [ text <| t T.AboutSyncryptDesktop model ]
    , div []
        [ div []
            [ span []
                [ text <| t T.Version model ++ ": " ]
            , text model.config.version
            , availableUpdate
            ]
        , div []
            [ span []
                [ text <| t T.DaemonAuthToken model ++ ": " ]
            , text model.config.apiAuthToken
            ]
        ]
    ]


keyExportButton : HasSettingsDialog a -> Html Model.Msg
keyExportButton model =
    button [ class "Button" ]
        { onClick = Model.OpenUserKeyExportDialog
        , label = "Export Key"
        }


changePasswordButton : HasSettingsDialog a -> Html Model.Msg
changePasswordButton model =
    button [ class "Button" ]
        { label = t T.ChangePassword model
        , onClick = Model.SettingsDialogMsg ToggleChangePasswordForm
        }


resetPasswordButton : HasSettingsDialog a -> Html Model.Msg
resetPasswordButton model =
    button [ class "Button" ]
        { label = t T.ResetPassword model
        , onClick = Model.SettingsDialogMsg OpenPasswordResetPage
        }


changePasswordForm : HasSettingsDialog a -> Html Model.Msg
changePasswordForm ({ settingsDialog } as model) =
    div
        [ classList
            [ ( "Hidden", not settingsDialog.showChangePasswordForm )
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
        ( input, inputMsg, labelText, tooltipText, onEnterMsg ) =
            case inputType of
                Old ->
                    ( model.settingsDialog.oldPasswordInput
                    , OldPasswordInputMsg
                    , T.OldPasswordLabel
                    , T.OldPasswordTooltip
                    , Model.FocusOn model.settingsDialog.newPasswordInput.uid
                    )

                New ->
                    ( model.settingsDialog.newPasswordInput
                    , NewPasswordInputMsg
                    , T.NewPasswordLabel
                    , T.NewPasswordTooltip
                    , Model.FocusOn model.settingsDialog.newPasswordConfirmationInput.uid
                    )

                NewConfirmation ->
                    ( model.settingsDialog.newPasswordConfirmationInput
                    , NewPasswordConfirmationInputMsg
                    , T.NewPasswordConfirmationLabel
                    , T.NewPasswordConfirmationTooltip
                    , Model.SettingsDialogMsg ConfirmChangePassword
                    )
    in
    labeledItem [ class "InputLabel" ]
        { side = Top
        , onClick = Just (Model.FocusOn input.uid)
        , label = text <| t labelText model
        , item =
            span [ onEnter onEnterMsg ]
                [ Util.tooltipItem
                    { position = Right
                    , length = Util.Medium
                    , text = t tooltipText model
                    , visible = False
                    }
                    [ Ui.Input.view
                        input
                        |> Html.map (Model.SettingsDialogMsg << inputMsg)
                    ]
                ]
        }


buttons : HasSettingsDialog a -> Html Model.Msg
buttons ({ settingsDialog } as model) =
    let
        oldPassword =
            settingsDialog.oldPasswordInput.value

        newPassword =
            settingsDialog.newPasswordInput.value

        passwordConfirmation =
            settingsDialog.newPasswordConfirmationInput.value

        isValidNewPassword =
            (not <| String.isEmpty newPassword)
                && (newPassword == passwordConfirmation)
    in
    div
        [ classList
            [ ( "Buttons", True )
            , ( "Hidden", not model.settingsDialog.hasChangesPending )
            ]
        ]
        [ case ( oldPassword, newPassword, passwordConfirmation ) of
            ( "", _, _ ) ->
                Dialog.errorMsg <| t T.YouNeedToEnterYourCurrentPassword model

            ( _, "", _ ) ->
                Dialog.errorMsg <| t T.YouNeedToEnterANewPassword model

            _ ->
                if newPassword == passwordConfirmation then
                    button
                        [ class "Button"
                        , disabled <| isValidNewPassword
                        ]
                        { label = t T.ChangePassword model
                        , onClick = Model.SettingsDialogMsg ConfirmChangePassword
                        }
                else
                    Dialog.errorMsg <| t T.PasswordConfirmationDoesNotMatch model
        , button [ class "Button" ]
            { label = T.t T.Cancel model
            , onClick = Model.SettingsDialogMsg ToggleChangePasswordForm
            }
        ]


t : T.SettingsDialogText -> HasLanguage a -> String
t txt model =
    T.t (T.SettingsDialogTxt txt) model


separator : Html msg
separator =
    Html.hr [ class "Separator" ]
        []


autoStartCheckbox : HasSettingsDialog a -> Html Model.Msg
autoStartCheckbox { autoStartEnabled } =
    Html.input
        [ type_ "checkbox"
        , onClick Model.ToggleAutoStart
        , checked autoStartEnabled
        ]
        []
