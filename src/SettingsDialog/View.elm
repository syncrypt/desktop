module SettingsDialog.View exposing (view)

import ConfirmationDialog
import Html exposing (Html, div, span, text)
import Html.Attributes exposing (class, classList)
import Model
import SettingsDialog.Model exposing (HasSettingsDialog, Msg(..))
import Language exposing (Language(..))
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
            [ Ui.Button.model (toString forLanguage) "secondary" "small"
                |> Ui.Button.view onClickMsg
            ]


contents : HasSettingsDialog a -> List (Html Model.Msg)
contents model =
    [ div [ class "LanguageInfoLabel" ]
        [ text "Choose your language:" ]
    , languageButton German model
    , languageButton English model
    ]
