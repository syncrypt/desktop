module WizardDialog
    exposing
        ( open
        , close
        , view
        , update
        )

import Html exposing (Html, div, span, text)
import Html.Attributes exposing (class, classList, style)
import Ui.Button
import Ui.Modal
import Util
import Model
import WizardDialog.Model exposing (..)
import SetupWizard
import FeedbackWizard


open : WizardSettings msg -> HasWizardDialog a msg -> ( HasWizardDialog a msg, Cmd msg )
open settings ({ wizardDialog } as model) =
    case ( wizardDialog, settings.steps ) of
        ( _, 0 ) ->
            model
                ! []

        ( Nothing, _ ) ->
            (settings
                |> init
                |> Just
                |> asWizardIn model
            )
                ! [ Util.delayMsg 150 (settings.address Show) ]

        ( Just state, _ ) ->
            ({ state | modal = Ui.Modal.open state.modal }
                |> Just
                |> asWizardIn model
            )
                ! []


close : HasWizardDialog a msg -> HasWizardDialog a msg
close ({ wizardDialog } as model) =
    { model | wizardDialog = Nothing }


show : HasWizardDialog a msg -> HasWizardDialog a msg
show ({ wizardDialog } as model) =
    wizardDialog
        |> Maybe.map
            (\wizard ->
                { wizard
                    | modal = Ui.Modal.open wizard.modal
                    , currentStep = 1
                }
            )
        |> asWizardIn model


hide : HasWizardDialog a msg -> HasWizardDialog a msg
hide ({ wizardDialog } as model) =
    wizardDialog
        |> Maybe.map
            (\wizard ->
                { wizard
                    | modal = Ui.Modal.close wizard.modal
                    , currentStep = 1
                }
            )
        |> asWizardIn model


update : Msg -> HasWizardDialog a msg -> ( HasWizardDialog a msg, Cmd msg )
update msg ({ wizardDialog } as model) =
    case msg of
        Modal msg ->
            case wizardDialog of
                Nothing ->
                    model
                        ! []

                Just state ->
                    ({ state | modal = Ui.Modal.update msg state.modal }
                        |> Just
                        |> asWizardIn model
                    )
                        ! [ Util.delayMsg 150 (state.address Close) ]

        Show ->
            (show model)
                ! []

        Hide ->
            (hide model)
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
            case wizardDialog of
                Just { onFinishMsg } ->
                    case onFinishMsg of
                        Just msg ->
                            (close model)
                                ! [ Util.sendMsg msg ]

                        _ ->
                            (close model)
                                ! []

                _ ->
                    (close model)
                        ! []


view : Model.Model -> Html Model.Msg
view ({ wizardDialog } as model) =
    -- don't display anything unless we have messages to produce
    case wizardDialog of
        Nothing ->
            div [] []

        Just state ->
            case stepSettings model state of
                Nothing ->
                    div [] []

                Just settings ->
                    viewDialog state settings


viewDialog : State Model.Msg -> StepSettings Model.Msg -> Html Model.Msg
viewDialog state step =
    let
        viewConfig =
            { address = state.address << Modal
            , contents =
                [ div [ class "Content" ]
                    [ step.contents ]
                ]
            , footer = wizardButtons state step.buttons
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


wizardButtons :
    State msg
    -> ButtonSettings msg
    -> List (Html msg)
wizardButtons state buttonSettings =
    let
        prevButton =
            button [ class "Button-Previous" ]
                "Previous"
                (state.address ToPreviousStep)

        nextButton =
            button [ class "Button-Next" ]
                "Next"
                (state.address ToNextStep)

        finishButton =
            button [ class "Button-Finish" ]
                "Finish"
                (state.address FinishWizard)

        cancelButton =
            button [ class "Button-Cancel" ]
                "Cancel"
                (state.address Close)

        navigationButtons buttons =
            div [ class "NavigationButtons" ]
                buttons

        buttons : List (Html msg)
        buttons =
            case ( hasPreviousStep state, hasNextStep state ) of
                ( True, True ) ->
                    [ navigationButtons [ prevButton, nextButton ] ]

                ( True, False ) ->
                    [ finishButton, navigationButtons [ prevButton ] ]

                ( False, True ) ->
                    [ navigationButtons [ nextButton ] ]

                ( False, False ) ->
                    [ finishButton ]

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

                CustomButton attrs title msg ->
                    button (attrs ++ [ class "Custom-Button" ]) title msg
    in
        case buttonSettings of
            Default ->
                cancelButton :: buttons

            Visible buttons ->
                List.map toHtml buttons


hasPreviousStep : State msg -> Bool
hasPreviousStep { currentStep } =
    currentStep > 1


hasNextStep : State msg -> Bool
hasNextStep { currentStep, steps } =
    currentStep < steps


toNextStep : State msg -> State msg
toNextStep state =
    if hasNextStep state then
        { state | currentStep = state.currentStep + 1 }
    else
        state


toPreviousStep : State msg -> State msg
toPreviousStep state =
    if hasPreviousStep state then
        { state | currentStep = state.currentStep - 1 }
    else
        state


moveToPreviousStep : HasWizardDialog a msg -> HasWizardDialog a msg
moveToPreviousStep ({ wizardDialog } as model) =
    wizardDialog
        |> Maybe.map toPreviousStep
        |> asWizardIn model


moveToNextStep : HasWizardDialog a msg -> HasWizardDialog a msg
moveToNextStep ({ wizardDialog } as model) =
    wizardDialog
        |> Maybe.map toNextStep
        |> asWizardIn model


asWizardIn :
    HasWizardDialog a msg
    -> Maybe (State msg)
    -> HasWizardDialog a msg
asWizardIn model maybeWizard =
    { model | wizardDialog = maybeWizard }


stepSettings :
    Model.Model
    -> State Model.Msg
    -> Maybe (StepSettings Model.Msg)
stepSettings model state =
    case state.wizardType of
        SetupWizard ->
            SetupWizard.stepSettings model state

        FeedbackWizard ->
            FeedbackWizard.stepSettings model state
