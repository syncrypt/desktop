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


view : Language -> IconButton -> List (Html.Attribute msg) -> Html msg
view language buttonType attrs =
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
