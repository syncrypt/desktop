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


view : HasSettingsDialog a -> Html Model.Msg
view ({ settingsDialog } as model) =
    let
        viewConfig =
            { address = Model.SettingsDialogMsg << ModalMsg
            , contents = contents model
            , footer = []
            , title = "Program Settings"
            }
    in
        div [ class "SettingsDialog" ]
            [ Ui.Modal.view viewConfig settingsDialog.modal ]


languageButton : Translation.Language -> HasSettingsDialog a -> Html Model.Msg
languageButton forLanguage { language, settingsDialog } =
    span
        [ classList
            [ ( "LanguageButton", True )
            , ( "Active", language == forLanguage )
            ]
        ]
        [ Ui.Button.model (toString forLanguage) "secondary" "small"
            |> Ui.Button.view (Model.SettingsDialogMsg (LanguageSelection forLanguage))
        ]


contents : HasSettingsDialog a -> List (Html Model.Msg)
contents model =
    [ div [ class "LanguageInfoLabel" ]
        [ text "Choose your language:" ]
    , languageButton Translation.German model
    , languageButton Translation.English model
    ]
