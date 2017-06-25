module WizardDialog
    exposing
        ( State
        , Msg
        , Step(..)
        , StepSettings
        , StepList
        , init
        , open
        , close
        , view
        , update
        , stepsFromList
        )

import Html exposing (Html, div, span, text)
import Html.Attributes exposing (class, classList)
import Ui.Button
import Ui.Modal
import Util


type alias StepSettings msg =
    { title : String, contents : Html msg }


type Step msg
    = Step (StepSettings msg)
    | Finished


type StepList msg
    = StepList (List (Step msg)) (Step msg) (List (Step msg))


type alias State msg =
    { modal : Ui.Modal.Model
    , view : Maybe (ViewSettings msg)
    , address : Msg -> msg
    }


type alias HasWizardDialog a m =
    { a | wizardDialog : State m }


type alias ViewSettings msg =
    { steps : StepList msg
    , onFinishMsg : msg
    }


type Msg
    = Modal Ui.Modal.Msg
    | Close
    | ToNextStep
    | ToPreviousStep
    | ToStep Int
    | FinishWizard


init : (Msg -> msg) -> State msg
init address =
    { modal =
        Ui.Modal.init
            |> Ui.Modal.closable False
            |> Ui.Modal.backdrop True
    , view = Nothing
    , address = address
    }


open : List (Step msg) -> msg -> HasWizardDialog a msg -> HasWizardDialog a msg
open steps onFinishMsg ({ wizardDialog } as model) =
    case steps of
        [] ->
            model

        step1 :: steps ->
            { model
                | wizardDialog =
                    { wizardDialog
                        | modal = Ui.Modal.open wizardDialog.modal
                        , view =
                            Just
                                { steps = stepsFromList step1 steps
                                , onFinishMsg = onFinishMsg
                                }
                    }
            }


close : HasWizardDialog a msg -> HasWizardDialog a msg
close ({ wizardDialog } as model) =
    { model
        | wizardDialog =
            { wizardDialog
                | modal = Ui.Modal.close wizardDialog.modal
                , view = Nothing
            }
    }


update : Msg -> HasWizardDialog a msg -> ( HasWizardDialog a msg, Cmd msg )
update msg ({ wizardDialog } as model) =
    case msg of
        Modal msg ->
            { model
                | wizardDialog =
                    { wizardDialog
                        | modal = Ui.Modal.update msg wizardDialog.modal
                    }
            }
                ! []

        Close ->
            (close model)
                ! []

        ToNextStep ->
            (moveToNextStep model)
                ! []

        ToPreviousStep ->
            (moveToPreviousStep model)
                ! []

        ToStep stepNum ->
            model
                ! []

        FinishWizard ->
            case wizardDialog.view of
                Just { onFinishMsg } ->
                    (close model)
                        ! [ Util.sendMsg onFinishMsg ]

                Nothing ->
                    (close model)
                        ! []


view : HasWizardDialog a msg -> Html msg
view { wizardDialog } =
    -- don't display anything unless we have messages to produce
    case wizardDialog.view of
        Nothing ->
            div [] []

        Just view ->
            case currentStep view.steps of
                Step { title } ->
                    let
                        viewConfig =
                            { address = (wizardDialog.address << Modal)
                            , contents = contents wizardDialog.address view
                            , footer = []
                            , title = title
                            }
                    in
                        Ui.Modal.view viewConfig wizardDialog.modal

                Finished ->
                    div [] []


contents : (Msg -> msg) -> ViewSettings msg -> List (Html msg)
contents address view =
    case currentStep view.steps of
        Step step ->
            [ div [ class "WizardDialog-Content" ] <|
                [ step.contents
                , wizardButtons address view
                ]
            ]

        Finished ->
            []


wizardButtons : (Msg -> msg) -> ViewSettings msg -> Html msg
wizardButtons address view =
    let
        step =
            currentStep view.steps

        prevButton =
            span [ class "WizardDialog-Button-Previous" ]
                [ Ui.Button.model "Previous" "secondary" "small"
                    |> Ui.Button.view (address ToPreviousStep)
                ]

        nextButton =
            span [ class "WizardDialog-Button-Next" ]
                [ Ui.Button.model "Next" "secondary" "small"
                    |> Ui.Button.view (address ToNextStep)
                ]

        finishButton =
            span [ class "WizardDialog-Button-Finish" ]
                [ Ui.Button.model "Finish" "secondary" "small"
                    |> Ui.Button.view (address FinishWizard)
                ]

        cancelButton =
            span [ class "WizardDialog-Button-Cancel" ]
                [ Ui.Button.model "Cancel" "secondary" "small"
                    |> Ui.Button.view (address Close)
                ]

        navigationButtons buttons =
            div [ class "WizardDialog-NavigationButtons" ]
                buttons

        buttons =
            case ( hasPreviousStep view.steps, hasNextStep view.steps ) of
                ( True, True ) ->
                    [ navigationButtons [ prevButton, nextButton ] ]

                ( True, False ) ->
                    [ navigationButtons [ prevButton ], finishButton ]

                ( False, True ) ->
                    [ navigationButtons [ nextButton ] ]

                ( False, False ) ->
                    []
    in
        div [ class "WizardDialog-Buttons" ]
            (cancelButton :: buttons)


stepsFromList : Step msg -> List (Step msg) -> StepList msg
stepsFromList firstStep steps =
    StepList [] firstStep steps


currentStep : StepList msg -> Step msg
currentStep (StepList _ current _) =
    current


hasPreviousStep : StepList msg -> Bool
hasPreviousStep steps =
    case steps of
        StepList [] _ _ ->
            False

        _ ->
            True


hasNextStep : StepList msg -> Bool
hasNextStep steps =
    case steps of
        StepList _ _ [] ->
            False

        _ ->
            True


toNextStep : StepList msg -> StepList msg
toNextStep (StepList prev current next) =
    case next of
        [] ->
            StepList (current :: prev) Finished []

        s :: rest ->
            StepList (current :: prev) s rest


toPreviousStep : StepList msg -> StepList msg
toPreviousStep (StepList prev current next) =
    case prev of
        [] ->
            StepList [] current next

        s :: rest ->
            StepList rest s (current :: next)


moveToPreviousStep : HasWizardDialog a msg -> HasWizardDialog a msg
moveToPreviousStep ({ wizardDialog } as model) =
    case wizardDialog.view of
        Nothing ->
            model

        Just view ->
            { wizardDialog
                | view = Just { view | steps = toPreviousStep view.steps }
            }
                |> asWizardIn model


moveToNextStep : HasWizardDialog a msg -> HasWizardDialog a msg
moveToNextStep ({ wizardDialog } as model) =
    case wizardDialog.view of
        Nothing ->
            model

        Just view ->
            { wizardDialog
                | view = Just { view | steps = toNextStep view.steps }
            }
                |> asWizardIn model


asWizardIn : HasWizardDialog a msg -> State msg -> HasWizardDialog a msg
asWizardIn model wizard =
    { model | wizardDialog = wizard }
