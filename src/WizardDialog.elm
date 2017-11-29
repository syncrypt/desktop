module WizardDialog
    exposing
        ( cancelButton
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
import Language exposing (Language)
import Model
import SetupWizard
import Translation as T
import Ui.Modal
import Util exposing (button, customButton)
import WizardDialog.Model exposing (..)


open : WizardSettings msg -> HasWizardDialog a msg -> ( HasWizardDialog a msg, Cmd msg )
open settings ({ wizardDialog } as model) =
    case ( wizardDialog, List.isEmpty settings.steps ) of
        -- no steps, abort
        ( _, True ) ->
            model
                ! []

        -- no wizard window open yet, create & open it
        ( Nothing, _ ) ->
            (settings
                |> init
                |> Just
                |> asWizardIn model
            )
                ! [ Util.delayMsg 150 (settings.address Show) ]

        -- wizard window already there (potentially hidden), open it
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

        ToStepWithName stepName ->
            moveToStepWithName stepName model
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
view ({ wizardDialog, language } as model) =
    let
        empty =
            text ""

        maybeViewDialog state =
            model
                |> viewSettings state
                |> Maybe.map (viewDialog language state)
                |> Maybe.withDefault empty
    in
    wizardDialog
        |> Maybe.map maybeViewDialog
        |> Maybe.withDefault empty


viewDialog : Language -> State Model.Msg -> ViewSettings Model.Msg -> Html Model.Msg
viewDialog language state step =
    let
        viewConfig =
            { address = state.address << Modal
            , contents =
                [ div [ class "Content" ]
                    [ step.contents ]
                ]
            , footer = wizardButtons language state step.buttons
            , title = step.title
            }
    in
    div [ class "WizardDialog" ]
        [ Ui.Modal.view viewConfig state.modal ]


wizardButtons :
    Language
    -> State msg
    -> ButtonSettings msg
    -> List (Html msg)
wizardButtons language state buttonSettings =
    let
        commonButtons : Maybe (NavButtons msg) -> List (Html msg)
        commonButtons navButtons =
            let
                ( prevBtn, nextBtn ) =
                    case navButtons of
                        Just customNavButtons ->
                            customNavigationButtons customNavButtons language state

                        Nothing ->
                            ( prevButton language state, nextButton language state )
            in
            case ( hasPreviousStep state, hasNextStep state ) of
                ( True, True ) ->
                    [ navigationButtons [ prevBtn, nextBtn ] ]

                ( True, False ) ->
                    [ finishButton language state
                    , navigationButtons [ prevBtn ]
                    ]

                ( False, True ) ->
                    [ navigationButtons [ nextBtn ] ]

                ( False, False ) ->
                    [ finishButton language state ]

        toHtml : Button msg -> Html msg
        toHtml btn =
            case btn of
                Previous ->
                    prevButton language state

                Next ->
                    nextButton language state

                Cancel ->
                    cancelButton language state

                Finish ->
                    finishButton language state

                CustomButton attrs { label, onClick } ->
                    button (attrs ++ [ class "Custom-Button" ])
                        { label = label
                        , onClick = onClick
                        }
    in
    case buttonSettings of
        Default ->
            cancelButton language state
                :: commonButtons Nothing

        Visible buttons ->
            List.map toHtml buttons

        CustomNav navButtons ->
            cancelButton language state
                :: commonButtons (Just navButtons)


type NavButtonDirection
    = NavPrev
    | NavNext


customNavigationButtons : NavButtons msg -> Language -> State msg -> ( Html msg, Html msg )
customNavigationButtons { prev, next } language state =
    let
        toHtml navButton navButtonDirection =
            case ( navButton, navButtonDirection ) of
                ( Auto, NavPrev ) ->
                    prevButton language state

                ( Auto, NavNext ) ->
                    nextButton language state

                ( AutoWithLabel label, NavPrev ) ->
                    customPrevButton (Label label)
                        (state.address ToPreviousStep)
                        language
                        state

                ( AutoWithLabel label, NavNext ) ->
                    customNextButton (Label label)
                        (state.address ToNextStep)
                        language
                        state

                ( Nav msg, NavPrev ) ->
                    customPrevButton DefaultLabel msg language state

                ( Nav msg, NavNext ) ->
                    customNextButton DefaultLabel msg language state

                ( NavWithLabel msg label, NavPrev ) ->
                    customPrevButton (Label label) msg language state

                ( NavWithLabel msg label, NavNext ) ->
                    customNextButton (Label label) msg language state

                ( Hidden, _ ) ->
                    text ""
    in
    ( toHtml prev NavPrev
    , toHtml next NavNext
    )


viewSettings : State Model.Msg -> Model.Model -> Maybe (ViewSettings Model.Msg)
viewSettings state model =
    case state.wizardType of
        SetupWizard ->
            SetupWizard.viewSettings state model

        FeedbackWizard ->
            FeedbackWizard.viewSettings state model


prevButton : Language -> State msg -> Html msg
prevButton language state =
    customPrevButton DefaultLabel (state.address ToPreviousStep) language state


nextButton : Language -> State msg -> Html msg
nextButton language state =
    customNextButton DefaultLabel (state.address ToNextStep) language state


type CustomButtonLabel
    = DefaultLabel
    | Label String


customButtonLabel : CustomButtonLabel -> String -> String
customButtonLabel label default =
    case label of
        DefaultLabel ->
            default

        Label l ->
            l


customPrevButton : CustomButtonLabel -> msg -> Language -> State msg -> Html msg
customPrevButton label msg language state =
    button [ class "Button-Previous" ]
        { label = customButtonLabel label (T.translate T.Previous language)
        , onClick = msg
        }


customNextButton : CustomButtonLabel -> msg -> Language -> State msg -> Html msg
customNextButton label msg language state =
    button [ class "Button-Next" ]
        { label = customButtonLabel label (T.translate T.Next language)
        , onClick = msg
        }


finishButton : Language -> State msg -> Html msg
finishButton language state =
    button [ class "Button-Finish" ]
        { label = T.translate T.Finish language
        , onClick = state.address FinishWizard
        }


cancelButton : Language -> State msg -> Html msg
cancelButton language state =
    customButton [ class "Button-Cancel" ]
        { disabled = False -- TODO: CHANGE TO: not state.closable
        , label = T.translate T.Cancel language
        , onClick = state.address HideAndClose
        }


navigationButtons : List (Html msg) -> Html msg
navigationButtons buttons =
    div [ class "NavigationButtons" ]
        buttons
