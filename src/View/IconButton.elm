module View.IconButton exposing (IconButton(..), view)

import Html exposing (Html, div)
import Html.Attributes exposing (class, style)
import Language exposing (Language)
import Translation as T
import Util exposing (Position(..), TooltipLength(Auto), tooltipItem)


type IconButton
    = SettingsButton
    | LogoutButton
    | FeedbackButton
    | RefreshVaultsButton
    | DaemonLogButton
    | ImportVaultButton


view : List (Html.Attribute msg) -> Language -> IconButton -> Html msg
view attrs language buttonType =
    tooltipItem
        { position = Bottom
        , length = Auto
        , text = tooltip language buttonType
        }
        [ div
            (class "MainScreen-IconButton" :: attrs)
            [ div
                [ class "Icon"
                , style
                    [ ( "backgroundImage"
                      , "url(assets/" ++ iconName buttonType ++ "_24px.svg)"
                      )
                    ]
                ]
                []
            ]
        ]


iconName : IconButton -> String
iconName buttonType =
    case buttonType of
        LogoutButton ->
            "exit"

        SettingsButton ->
            "settings"

        FeedbackButton ->
            "feedback"

        RefreshVaultsButton ->
            "sync_white"

        DaemonLogButton ->
            "event"

        ImportVaultButton ->
            -- TODO: use custom / better icon here
            "edit"


tooltip : Language -> IconButton -> String
tooltip language buttonType =
    case buttonType of
        LogoutButton ->
            T.translate T.Logout language

        SettingsButton ->
            T.translate T.SoftwareAndAccountSettings language

        FeedbackButton ->
            T.translate T.SendUsFeedbackAndBugReports language

        RefreshVaultsButton ->
            T.translate T.RefreshVaults language

        DaemonLogButton ->
            T.translate T.ViewDaemonLog language

        ImportVaultButton ->
            T.translate T.ImportVault language
