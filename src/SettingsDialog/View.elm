module SettingsDialog.View exposing (view)

import ConfirmationDialog
import Html exposing (Html, div, span, text)
import Html.Attributes exposing (class, classList)
import Language exposing (Language(..))
import Model
import RemoteData exposing (RemoteData(..), WebData)
import SettingsDialog.Model exposing (HasSettingsDialog, Msg(..))
import Translation as T
import Ui.Modal
import Util exposing (button, customButton)


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
            [ ( "LanguageButton", True )
            , ( "Active", language == forLanguage )
            ]
        ]
        [ button []
            { label = toString forLanguage
            , onClick = onClickMsg
            }
        ]


daemonActionButton : String -> Bool -> Model.Msg -> Html Model.Msg
daemonActionButton title enabled msg =
    span
        [ classList
            []
        ]
        [ customButton []
            { label = title
            , onClick = msg
            , disabled = not enabled -- TODO: It looks like this has no effect.
            }
        ]


daemonContents : HasSettingsDialog a -> List (Html Model.Msg)
daemonContents model =
    let
        { stats } =
            model

        daemonIsRunning =
            case stats of
                Success s ->
                    True

                _ ->
                    False
    in
    [ div [ class "DaemonManagementTitle" ]
        [ text <|
            T.t T.DaemonManagement model
        ]
    , div []
        [ text <|
            T.t (T.DaemonStatus daemonIsRunning) model
        ]
    , daemonActionButton
        "Restart"
        daemonIsRunning
        (Model.SettingsDialogMsg <| RestartDaemon)
    , daemonActionButton
        "Shutdown"
        daemonIsRunning
        (Model.SettingsDialogMsg <| ShutdownDaemon)
    , daemonActionButton
        "Start"
        (not daemonIsRunning)
        -- TODO: We want to implement a StartDaemon signal as well
        (Model.SettingsDialogMsg <| RestartDaemon)
    ]


contents : HasSettingsDialog a -> List (Html Model.Msg)
contents model =
    [ div [ class "LanguageInfoLabel" ]
        [ text <|
            T.t T.ChooseYourLanguage model
        ]
    , languageButton German model
    , languageButton English model
    ]
        ++ daemonContents model
