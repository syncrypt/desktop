module DaemonLog exposing (dialogSettings, dialogViewSettings, subscriptions)

import Daemon
import Data.Daemon
import Date.Distance
import Html
    exposing
        ( Html
        , div
        , input
        , p
        , span
        , table
        , tbody
        , td
        , text
        , th
        , thead
        , tr
        )
import Html.Attributes exposing (class, classList, type_)
import Model
import Translation
import WizardDialog.Model exposing (..)


steps : List (StepConfig Model.Model Model.Msg)
steps =
    [ ( "DaemonLog", step1 ) ]


dialogSettings : Model.Model -> WizardSettings Model.Msg
dialogSettings model =
    { address = Model.WizardDialogMsg
    , onFinishMsg = Nothing
    , steps = steps |> List.map Tuple.first
    , wizardType = DaemonLogDialog
    , closable = True
    }


subscriptions : Model.Model -> Sub Model.Msg
subscriptions model =
    case model.state of
        Model.ShowingDaemonLog ->
            Daemon.subscribeDaemonLogStream Model.DaemonLogStream model

        _ ->
            Sub.none


wizardContent : List (Html msg) -> Html msg
wizardContent body =
    div [ class "MainScreen-DaemonLogDialog" ]
        body


dialogViewSettings : State Model.Msg -> Model.Model -> Maybe (ViewSettings Model.Msg)
dialogViewSettings state model =
    WizardDialog.Model.viewSettings steps state model


infoTextLine : String -> Html msg
infoTextLine line =
    span []
        [ text line ]



-- STEPS


step1 : Model.Model -> state -> Maybe (ViewSettings Model.Msg)
step1 model state =
    Just
        { title = "Daemon Log"
        , contents =
            if List.isEmpty model.daemonLogItems then
                wizardContent
                    [ text "LOADING..." ]
            else
                wizardContent <|
                    viewLogItems model
        , buttons =
            Visible
                [ CustomButton []
                    { label = "Close"
                    , onClick = Model.CloseDaemonLogDialog
                    }
                ]
        }


viewLogItems : Model.Model -> List (Html Model.Msg)
viewLogItems model =
    [ table [ class "LogItems" ]
        [ thead []
            [ tr []
                [ th [ class "LogLevel" ]
                    [ text "Level" ]
                , th [ class "CreatedAt" ]
                    [ text "Date" ]
                , th [ class "Message" ]
                    [ text "Message" ]
                ]
            ]
        , tbody []
            (model.daemonLogItems
                |> List.indexedMap (viewLogItem model)
            )
        ]
    ]


viewLogItem : Model.Model -> Int -> Data.Daemon.LogItem -> Html Model.Msg
viewLogItem model idx { level, createdAt, message } =
    tr
        [ classList
            [ ( "LogItem", True )
            , ( "LogItemLight", idx % 2 == 0 )
            , ( "LogItemDark", idx % 2 == 1 )
            ]
        ]
        [ td [ class "LogLevel" ]
            [ text <| toString level ]
        , td [ class "CreatedAt" ] <|
            case ( createdAt, model.now ) of
                ( Nothing, _ ) ->
                    []

                ( Just date, Nothing ) ->
                    [ text <| toString date ]

                ( Just date, Just now ) ->
                    [ text <| Translation.timeAgo date model ]
        , td [ class "Message" ]
            [ text message ]
        ]
