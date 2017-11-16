module WizardDialog
    exposing
        ( buttonToStep
        , cancelButton
        , close
        , finishButton
        , navigationButtons
        , nextButton
        , open
        , prevButton
        , update
        , view
        )

import FeedbackWizard
import Html exposing (Html, div, span, text)
import Html.Attributes exposing (class, classList, style)
import Model
import SetupWizard
import Ui.Button
import Ui.Modal
import Util
import WizardDialog.Model exposing (..)


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


hideAndClose : HasWizardDialog a msg -> ( HasWizardDialog a msg, Cmd msg )
hideAndClose ({ wizardDialog } as model) =
    case model.wizardDialog of
        Nothing ->
            model
                ! []

        Just state ->
            hide model
                ! [ Util.delayMsg 250 (state.address Close) ]


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
                        ! [ Util.delayMsg 150 (state.address HideAndClose) ]

        Show ->
            show model
                ! []

        Hide ->
            hide model
                ! []

        Close ->
            close model
                ! []

        HideAndClose ->
            model
                |> hideAndClose

        ToNextStep ->
            moveToNextStep model
                ! []

        ToPreviousStep ->
            moveToPreviousStep model
                ! []

        ToStep stepNum ->
            moveToStep stepNum model
                ! []

        FinishWizard ->
            let
                cmds =
                    wizardDialog
                        |> Maybe.map
                            (\{ onFinishMsg } ->
                                onFinishMsg
                                    |> Maybe.map (\msg -> [ Util.sendMsg msg ])
                                    |> Maybe.withDefault []
                            )
                        |> Maybe.withDefault []
            in
            close model
                ! cmds


view : Model.Model -> Html Model.Msg
view ({ wizardDialog } as model) =
    let
        empty =
            text ""

        maybeViewDialog state =
            state
                |> viewSettings model
                |> Maybe.map (viewDialog state)
                |> Maybe.withDefault empty
    in
    wizardDialog
        |> Maybe.map maybeViewDialog
        |> Maybe.withDefault empty


viewDialog : State Model.Msg -> ViewSettings Model.Msg -> Html Model.Msg
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
        buttons : List (Html msg)
        buttons =
            case ( hasPreviousStep state, hasNextStep state ) of
                ( True, True ) ->
                    [ navigationButtons [ prevButton state, nextButton state ] ]

                ( True, False ) ->
                    [ finishButton state, navigationButtons [ prevButton state ] ]

                ( False, True ) ->
                    [ navigationButtons [ nextButton state ] ]

                ( False, False ) ->
                    [ finishButton state ]

        toHtml : Button msg -> Html msg
        toHtml btn =
            case btn of
                Previous ->
                    prevButton state

                Next ->
                    nextButton state

                Cancel ->
                    cancelButton state

                Finish ->
                    finishButton state

                CustomButton attrs title msg ->
                    button (attrs ++ [ class "Custom-Button" ]) title msg
    in
    case buttonSettings of
        Default ->
            cancelButton state :: buttons

        Visible buttons ->
            List.map toHtml buttons


viewSettings : Model.Model -> State Model.Msg -> Maybe (ViewSettings Model.Msg)
viewSettings model state =
    case state.wizardType of
        SetupWizard ->
            SetupWizard.viewSettings model state

        FeedbackWizard ->
            FeedbackWizard.viewSettings model state


prevButton state =
    button [ class "Button-Previous" ]
        "Previous"
        (state.address ToPreviousStep)


nextButton state =
    button [ class "Button-Next" ]
        "Next"
        (state.address ToNextStep)


finishButton state =
    button [ class "Button-Finish" ]
        "Finish"
        (state.address FinishWizard)


cancelButton state =
    button [ class "Button-Cancel" ]
        "Cancel"
        (state.address HideAndClose)


buttonToStep : State msg -> List (Html.Attribute msg) -> String -> Int -> Button msg
buttonToStep state attrs label step =
    CustomButton attrs label (state.address <| ToStep step)


navigationButtons buttons =
    div [ class "NavigationButtons" ]
        buttons
