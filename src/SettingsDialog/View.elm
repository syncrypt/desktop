module SettingsDialog.View exposing (view)

import Dialog exposing (labeledItem)
import Html exposing (Html, div, span, text)
import Html.Attributes exposing (class, classList, disabled)
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
    [ div [ class "InfoLabel" ]
        [ text <| dialogText T.ChooseYourLanguage model ]
    , languageButton German model
    , languageButton English model
    , separator
    , div [ class "InfoLabel" ]
        [ text "Key export / import" ]
    , keyExportButton model
    , separator
    , div [ class "InfoLabel" ]
        [ text <| dialogText T.AccountOptions model ]
    , changePasswordButton model
    , resetPasswordButton model
    , changePasswordForm model
    , separator
    , div [ class "InfoLabel" ]
        [ text "About Syncrypt Desktop" ]
    , div []
        [ text ("Version: " ++ model.config.version) ]
    ]
        ++ (case model.updateAvailable of
                Just version ->
                    [ div [] [ text ("Update available: " ++ version) ] ]

                Nothing ->
                    []
           )


keyExportButton : HasSettingsDialog a -> Html Model.Msg
keyExportButton model =
    button [ class "Button" ]
        { onClick = Model.OpenUserKeyExportDialog
        , label = "Export Key"
        }


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
        , label = text <| dialogText labelText model
        , item =
            span [ onEnter onEnterMsg ]
                [ Util.tooltipItem
                    { position = Right
                    , length = Util.Medium
                    , text = dialogText tooltipText model
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
                Dialog.errorMsg "You need to enter your current password"

            ( _, "", _ ) ->
                Dialog.errorMsg "You need to enter a new password"

            _ ->
                if newPassword == passwordConfirmation then
                    button
                        [ class "Button"
                        , disabled <| isValidNewPassword
                        ]
                        { label = dialogText T.ChangePassword model
                        , onClick = Model.SettingsDialogMsg ConfirmChangePassword
                        }
                else
                    Dialog.errorMsg "Password confirmation doesn't match"
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
