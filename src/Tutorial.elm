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
import Util exposing (button, renderIf)


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
    , prevSteps : List Step
    , currStep : CurrentStep
    , nextSteps : List Step
    }


type CurrentStep
    = CurrentStep Step
    | TutorialFinished


type alias Step =
    { id : String
    , title : String
    , paragraphs : List String
    }


init : (Msg -> msg) -> Step -> List Step -> State msg
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


currentStep : State msg -> Maybe Step
currentStep { currStep } =
    case currStep of
        CurrentStep step ->
            Just step

        TutorialFinished ->
            Nothing


toNextStep : State msg -> State msg
toNextStep state =
    case ( state.currStep, state.nextSteps ) of
        ( TutorialFinished, _ ) ->
            state

        ( CurrentStep prevStep, [] ) ->
            { state
                | prevSteps = prevStep :: state.prevSteps
                , currStep = TutorialFinished
            }

        ( _, currStep :: nextSteps ) ->
            let
                prevSteps =
                    case state.currStep of
                        TutorialFinished ->
                            state.prevSteps

                        CurrentStep step ->
                            step :: state.prevSteps
            in
            { state
                | prevSteps = prevSteps
                , currStep = CurrentStep currStep
                , nextSteps = nextSteps
            }


toPrevStep : State msg -> State msg
toPrevStep state =
    case state.prevSteps of
        [] ->
            state

        currStep :: prevSteps ->
            let
                nextSteps =
                    case state.currStep of
                        CurrentStep step ->
                            step :: state.nextSteps

                        TutorialFinished ->
                            []
            in
            { state
                | prevSteps = prevSteps
                , currStep = CurrentStep currStep
                , nextSteps = nextSteps
            }


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
                                |> Maybe.map (\s -> [ s ])
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
            ( toNextStep state
            , Cmd.none
            )

        ToPreviousStep ->
            ( toPrevStep state
            , Cmd.none
            )

        MarkAsCompleted ->
            ( markCompleted state
            , Cmd.none
            )

        Restart ->
            ( toFirstStep state
            , Cmd.none
            )

        Hide ->
            ( hide state
            , Cmd.none
            )


type alias ButtonHandlers msg =
    { toPrevBtn : Msg -> msg
    , toNextBtn : Msg -> msg
    }


view : State msg -> Html msg
view state =
    case ( isVisible state, isFinished state ) of
        ( True, False ) ->
            viewCurrentStep state

        ( True, True ) ->
            text "Tutorial done"

        ( False, _ ) ->
            text ""


viewCurrentStep : State msg -> Html msg
viewCurrentStep state =
    case currentStep state of
        Just step ->
            p [ class "Tutorial" ]
                [ p [ class "Title" ]
                    [ text <| "Tutorial: " ++ step.title ]
                , viewParagraphs step
                , p [ class "Nav" ]
                    [ renderIf (hasPrevStep state) <|
                        button [ class "ToPrevBtn" ]
                            { label = "Previous"
                            , onClick = state.address ToPreviousStep
                            }
                    , renderIf (hasNextStep state) <|
                        button [ class "ToNextBtn" ]
                            { label = "Next"
                            , onClick = state.address ToNextStep
                            }
                    , renderIf (isFinalStep state) <|
                        button [ class "FinishBtn" ]
                            { label = "Finish Tutorial"
                            , onClick = state.address MarkAsCompleted
                            }
                    , p []
                        [ renderIf (not <| isFinalStep state) <|
                            button [ class "MarkCompletedBtn" ]
                                { label = "Skip Tutorial"
                                , onClick = state.address MarkAsCompleted
                                }
                        ]
                    ]
                ]

        Nothing ->
            text "Tutorial done"


viewParagraphs : Step -> Html msg
viewParagraphs step =
    p [ class "Paragraphs" ]
        (step.paragraphs
            |> List.map viewParagraph
        )


viewParagraph : String -> Html msg
viewParagraph par =
    p [ class "Paragraph" ]
        [ text par ]
