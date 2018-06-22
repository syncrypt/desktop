module DaemonLog exposing (dialogSettings, dialogViewSettings, subscriptions)

import Animation
import Daemon
import Data.Daemon
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
    , name = "DaemonLog"
    , onFinishMsg = Nothing
    , steps = steps |> List.map Tuple.first
    , wizardType = DaemonLogDialog
    , closable = True
    }


isDaemonLogOpened : Model.Model -> Bool
isDaemonLogOpened model =
    WizardDialog.Model.isWizardOpen DaemonLogDialog model


subscriptions : Model.Model -> Sub Model.Msg
subscriptions model =
    if isDaemonLogOpened model then
        Daemon.subscribeDaemonLogStream Model.DaemonLogStream model
    else
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
                    [ Animation.loadingCircle Animation.SmallCircle model ]
            else
                wizardContent <|
                    viewLogItems model
        , buttons =
            Visible [ CloseBtn <| Just Model.CloseDaemonLogDialog ]
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
