module SettingsDialog.View exposing (view)

import ConfirmationDialog
import Html exposing (Html, button, div, form, h4, img, input, label, span, text)
import Html.Attributes exposing (class, classList, for, id, src, style)
import Html.Events exposing (onClick)
import Model
import SettingsDialog.Model exposing (HasSettingsDialog, Msg(..))
import Translation
import Ui.Button
import Ui.Modal


view : HasSettingsDialog a Model.Msg -> Html Model.Msg
view ({ settingsDialog } as model) =
    let
        viewConfig =
            { address = settingsDialog.toMsg << ModalMsg
            , contents = contents model
            , footer = []
            , title = "Program Settings"
            }

        x =
            10
    in
        (div [ class "SettingsDialog" ]
            [ Ui.Modal.view viewConfig settingsDialog.modal ]
        )


languageButton : HasSettingsDialog a msg -> Translation.Language -> Html msg
languageButton { language, settingsDialog } forLanguage =
    span
        [ classList
            [ ( "LanguageButton", True )
            , ( "Active", language == forLanguage )
            ]
        ]
        [ Ui.Button.model (toString forLanguage) "secondary" "small"
            |> Ui.Button.view (settingsDialog.toMsg (LanguageSelection forLanguage))
        ]


contents : HasSettingsDialog a msg -> List (Html msg)
contents model =
    let
        buttons =
            [ languageButton model Translation.German
            , languageButton model Translation.English
            ]
    in
        (div [ class "LanguageInfoLabel" ]
            [ text "Choose your language:" ]
        )
            :: buttons
