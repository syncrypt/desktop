module Tutorial
    exposing
        ( HasTutorial
        , Msg
        , State
        , Step
        , currentStep
        , init
        , isFinished
        , toNextStep
        , toPrevStep
        , update
        , view
        )

import Html exposing (Html, p, text)
import Html.Attributes exposing (class)
import Language exposing (Language)
import Translation as T
import Util exposing (button, renderIf, toList)


type Msg
    = ToNextStep
    | ToPreviousStep
    | MarkAsCompleted
    | Restart
    | Hide


type alias HasTutorial a msg =
    { a | tutorial : State msg }


type alias State msg =
    { address : Msg -> msg
    , completed : Bool
    , hidden : Bool
    , prevSteps : List (Step msg)
    , currStep : CurrentStep msg
    , nextSteps : List (Step msg)
    }


type CurrentStep msg
    = CurrentStep (Step msg)
    | TutorialFinished


type alias Step msg =
    { id : String
    , title : T.Text
    , paragraphs : List T.Text
    , onEnter : List msg
    , onExit : List msg
    }


init : (Msg -> msg) -> Step msg -> List (Step msg) -> State msg
init address firstStep remainingSteps =
    { address = address
    , completed = False
    , hidden = False
    , prevSteps = []
    , currStep = CurrentStep firstStep
    , nextSteps = remainingSteps
    }


isFinished : State msg -> Bool
isFinished state =
    state.completed || (state.currStep == TutorialFinished)


isFinalStep : State msg -> Bool
isFinalStep state =
    isFinished state || (not <| hasNextStep state)


isVisible : State msg -> Bool
isVisible state =
    not state.hidden
        && (not <| isFinished state)


hasPrevStep : State msg -> Bool
hasPrevStep state =
    not <| List.isEmpty state.prevSteps


hasNextStep : State msg -> Bool
hasNextStep state =
    not <| List.isEmpty state.nextSteps


currentStep : State msg -> Maybe (Step msg)
currentStep { currStep } =
    case currStep of
        CurrentStep step ->
            Just step

        TutorialFinished ->
            Nothing


triggerMsgs : List msg -> Cmd msg
triggerMsgs msgs =
    Cmd.batch <| List.map Util.sendMsg msgs


toNextStep : State msg -> ( State msg, Cmd msg )
toNextStep state =
    case ( state.currStep, state.nextSteps ) of
        ( TutorialFinished, _ ) ->
            ( state
            , Cmd.none
            )

        ( CurrentStep prevStep, [] ) ->
            ( { state
                | prevSteps = prevStep :: state.prevSteps
                , currStep = TutorialFinished
              }
            , triggerMsgs prevStep.onExit
            )

        ( _, currStep :: nextSteps ) ->
            let
                ( prevSteps, cmd ) =
                    case state.currStep of
                        TutorialFinished ->
                            ( state.prevSteps
                            , Cmd.none
                            )

                        CurrentStep step ->
                            ( step :: state.prevSteps
                            , triggerMsgs (step.onExit ++ currStep.onEnter)
                            )
            in
            ( { state
                | prevSteps = prevSteps
                , currStep = CurrentStep currStep
                , nextSteps = nextSteps
              }
            , cmd
            )


toPrevStep : State msg -> ( State msg, Cmd msg )
toPrevStep state =
    case state.prevSteps of
        [] ->
            ( state
            , Cmd.none
            )

        currStep :: prevSteps ->
            let
                ( nextSteps, msgs ) =
                    case state.currStep of
                        CurrentStep step ->
                            ( step :: state.nextSteps, step.onExit )

                        TutorialFinished ->
                            ( [], [] )
            in
            ( { state
                | prevSteps = prevSteps
                , currStep = CurrentStep currStep
                , nextSteps = nextSteps
              }
            , triggerMsgs <| currStep.onEnter ++ msgs
            )


toFirstStep : State msg -> State msg
toFirstStep state =
    case List.reverse state.prevSteps of
        [] ->
            state

        currStep :: nextSteps ->
            { state
                | prevSteps = []
                , currStep = CurrentStep currStep
                , nextSteps =
                    nextSteps
                        ++ (state
                                |> currentStep
                                |> Maybe.map toList
                                |> Maybe.withDefault []
                           )
                        ++ state.nextSteps
            }


markCompleted : State msg -> State msg
markCompleted state =
    { state | completed = True }


hide : State msg -> State msg
hide state =
    { state | hidden = True }


update : Msg -> State msg -> ( State msg, Cmd msg )
update msg state =
    case msg of
        ToNextStep ->
            toNextStep state

        ToPreviousStep ->
            toPrevStep state

        MarkAsCompleted ->
            ( markCompleted state
            , triggerOnExit state
            )

        Restart ->
            ( toFirstStep state
            , triggerOnExit state
            )

        Hide ->
            ( hide state
            , triggerOnExit state
            )


triggerOnExit : State msg -> Cmd msg
triggerOnExit state =
    case currentStep state of
        Just step ->
            triggerMsgs step.onExit

        _ ->
            Cmd.none


view : Language -> State msg -> Html msg
view lang state =
    case ( isVisible state, isFinished state ) of
        ( True, False ) ->
            viewCurrentStep lang state

        ( True, True ) ->
            text "Tutorial done"

        ( False, _ ) ->
            text ""


viewCurrentStep : Language -> State msg -> Html msg
viewCurrentStep lang state =
    case currentStep state of
        Just step ->
            p [ class "Tutorial" ]
                [ p [ class "Title" ]
                    [ text <| "Tutorial: " ++ T.translate lang step.title ]
                , viewParagraphs lang step
                , p [ class "Nav" ]
                    [ renderIf (hasPrevStep state) <|
                        button [ class "Button-ToPrev" ]
                            { label = T.translate lang T.Previous
                            , onClick = state.address ToPreviousStep
                            }
                    , renderIf (hasNextStep state) <|
                        button [ class "Button-ToNext" ]
                            { label = T.translate lang T.Next
                            , onClick = state.address ToNextStep
                            }
                    , renderIf (isFinalStep state) <|
                        button [ class "Button-Finish" ]
                            { label = T.translate lang T.FinishTutorial
                            , onClick = state.address MarkAsCompleted
                            }
                    , p []
                        [ renderIf (not <| isFinalStep state) <|
                            button [ class "Button-MarkCompleted" ]
                                { label = T.translate lang T.SkipTutorial
                                , onClick = state.address MarkAsCompleted
                                }
                        ]
                    ]
                ]

        Nothing ->
            text "Tutorial done"


viewParagraphs : Language -> Step msg -> Html msg
viewParagraphs lang step =
    p [ class "Paragraphs" ]
        (step.paragraphs
            |> List.map (viewParagraph lang)
        )


viewParagraph : Language -> T.Text -> Html msg
viewParagraph lang par =
    p [ class "Paragraph" ]
        [ text <| T.translate lang par ]
