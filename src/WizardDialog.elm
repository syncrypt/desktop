module WizardDialog
    exposing
        ( State
        , Msg(..)
        , Step(..)
        , StepSettings
        , Button(..)
        , ButtonSettings(..)
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


type Button msg
    = Next
    | Prev
    | Cancel
    | Finish
    | CustomButton String msg


type ButtonSettings msg
    = Default
    | Visible (List (Button msg))


type alias StepSettings msg =
    { title : String, contents : Html msg, buttons : ButtonSettings msg }


type Step msg
    = Step (StepSettings msg)
    | Finished


type alias FinishedSteps msg =
    List (Step msg)


type alias UnfinishedSteps msg =
    List (Step msg)


type StepList msg
    = StepList (FinishedSteps msg) (Step msg) (UnfinishedSteps msg)


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
            { wizardDialog
                | modal = Ui.Modal.open wizardDialog.modal
                , view =
                    Just
                        { steps = stepsFromList step1 steps
                        , onFinishMsg = onFinishMsg
                        }
            }
                |> asWizardIn model


close : HasWizardDialog a msg -> HasWizardDialog a msg
close ({ wizardDialog } as model) =
    { wizardDialog
        | modal = Ui.Modal.close wizardDialog.modal
        , view = Nothing
    }
        |> asWizardIn model


update : Msg -> HasWizardDialog a msg -> ( HasWizardDialog a msg, Cmd msg )
update msg ({ wizardDialog } as model) =
    case msg of
        Modal msg ->
            ({ wizardDialog | modal = Ui.Modal.update msg wizardDialog.modal }
                |> asWizardIn model
            )
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
                Step stepSettings ->
                    viewDialog wizardDialog stepSettings view

                Finished ->
                    div [] []


viewDialog : State msg -> StepSettings msg -> ViewSettings msg -> Html msg
viewDialog state step view =
    let
        viewConfig =
            { address = state.address << Modal
            , contents =
                [ div [ class "Content" ]
                    [ step.contents ]
                ]
            , footer = wizardButtons state.address view step.buttons
            , title = step.title
            }
    in
        div [ class "WizardDialog" ]
            [ Ui.Modal.view viewConfig state.modal ]


button : List (Html.Attribute msg) -> String -> msg -> Html msg
button attributes title msg =
    span attributes
        [ Ui.Button.model title "secondary" "small"
            |> Ui.Button.view msg
        ]


wizardButtons : (Msg -> msg) -> ViewSettings msg -> ButtonSettings msg -> List (Html msg)
wizardButtons address view buttonSettings =
    let
        step : Step msg
        step =
            currentStep view.steps

        prevButton =
            button [ class "Button-Previous" ] "Previous" (address ToPreviousStep)

        nextButton =
            button [ class "Button-Next" ] "Next" (address ToNextStep)

        finishButton =
            button [ class "Button-Finish" ] "Finish" (address FinishWizard)

        cancelButton =
            button [ class "Button-Cancel" ] "Cancel" (address Close)

        navigationButtons buttons =
            div [ class "NavigationButtons" ]
                buttons

        buttons : List (Html msg)
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

        toHtml : Button msg -> Html msg
        toHtml btn =
            case btn of
                Prev ->
                    prevButton

                Next ->
                    nextButton

                Cancel ->
                    cancelButton

                Finish ->
                    finishButton

                CustomButton title msg ->
                    button [ class "Custom-Button" ] title msg
    in
        case buttonSettings of
            Default ->
                cancelButton :: buttons

            Visible buttons ->
                List.map toHtml buttons


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
