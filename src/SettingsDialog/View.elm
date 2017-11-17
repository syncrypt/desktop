module SettingsDialog.View exposing (view)

import ConfirmationDialog
import Html exposing (Html, div, span, text)
import Html.Attributes exposing (class, classList)
import Language exposing (Language(..))
import Model
import SettingsDialog.Model exposing (HasSettingsDialog, Msg(..))
import Ui.Modal
import Util exposing (button)


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


languageButton : Language -> HasSettingsDialog a -> Html Model.Msg
languageButton forLanguage { language, settingsDialog } =
    let
        onClickMsg =
            Model.SettingsDialogMsg <| LanguageSelection forLanguage
    in
    span
        [ classList
            [ ( "LanguageButton", True )
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
    [ div [ class "LanguageInfoLabel" ]
        [ text "Choose your language:" ]
    , languageButton German model
    , languageButton English model
    ]
