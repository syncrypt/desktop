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
import Model
import SetupWizard
import Ui.Button
import Ui.Modal
import Util
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


type alias CustomButtonSettings msg =
    { label : String, onClick : msg, disabled : Bool }


customButton : List (Html.Attribute msg) -> CustomButtonSettings msg -> Html msg
customButton attributes { label, onClick, disabled } =
    let
        model =
            Ui.Button.model label "secondary" "small"
    in
    span attributes
        [ { model | disabled = disabled }
            |> Ui.Button.view onClick
        ]


wizardButtons :
    State msg
    -> ButtonSettings msg
    -> List (Html msg)
wizardButtons state buttonSettings =
    let
        commonButtons : Maybe (NavButtons msg) -> List (Html msg)
        commonButtons navButtons =
            let
                ( prevBtn, nextBtn ) =
                    case navButtons of
                        Just customNavButtons ->
                            customNavigationButtons customNavButtons state

                        Nothing ->
                            ( prevButton state, nextButton state )
            in
            case ( hasPreviousStep state, hasNextStep state ) of
                ( True, True ) ->
                    [ navigationButtons [ prevBtn, nextBtn ] ]

                ( True, False ) ->
                    [ finishButton state, navigationButtons [ prevBtn ] ]

                ( False, True ) ->
                    [ navigationButtons [ nextBtn ] ]

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

                CustomButton attrs { label, onClick } ->
                    button (attrs ++ [ class "Custom-Button" ]) label onClick
    in
    case buttonSettings of
        Default ->
            cancelButton state :: commonButtons Nothing

        Visible buttons ->
            List.map toHtml buttons

        CustomNav navButtons ->
            cancelButton state :: commonButtons (Just navButtons)


type NavButtonDirection
    = NavPrev
    | NavNext


customNavigationButtons : NavButtons msg -> State msg -> ( Html msg, Html msg )
customNavigationButtons { prev, next } state =
    let
        toHtml navButton navButtonDirection =
            case ( navButton, navButtonDirection ) of
                ( Auto, NavPrev ) ->
                    prevButton state

                ( Auto, NavNext ) ->
                    nextButton state

                ( AutoWithLabel label, NavPrev ) ->
                    customPrevButton (Label label) (state.address ToPreviousStep) state

                ( AutoWithLabel label, NavNext ) ->
                    customNextButton (Label label) (state.address ToNextStep) state

                ( Nav msg, NavPrev ) ->
                    customPrevButton DefaultLabel msg state

                ( Nav msg, NavNext ) ->
                    customNextButton DefaultLabel msg state

                ( NavWithLabel msg label, NavPrev ) ->
                    customPrevButton (Label label) msg state

                ( NavWithLabel msg label, NavNext ) ->
                    customNextButton (Label label) msg state

                ( Hidden, _ ) ->
                    text ""
    in
    ( toHtml prev NavPrev
    , toHtml next NavNext
    )


viewSettings : Model.Model -> State Model.Msg -> Maybe (ViewSettings Model.Msg)
viewSettings model state =
    case state.wizardType of
        SetupWizard ->
            SetupWizard.viewSettings model state

        FeedbackWizard ->
            FeedbackWizard.viewSettings model state


prevButton : State msg -> Html msg
prevButton state =
    customPrevButton DefaultLabel (state.address ToPreviousStep) state


nextButton : State msg -> Html msg
nextButton state =
    customNextButton DefaultLabel (state.address ToNextStep) state


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


customPrevButton : CustomButtonLabel -> msg -> State msg -> Html msg
customPrevButton label msg state =
    button [ class "Button-Previous" ]
        (customButtonLabel label "Previous")
        msg


customNextButton : CustomButtonLabel -> msg -> State msg -> Html msg
customNextButton label msg state =
    button [ class "Button-Next" ]
        (customButtonLabel label "Next")
        msg


finishButton : State msg -> Html msg
finishButton state =
    button [ class "Button-Finish" ]
        "Finish"
        (state.address FinishWizard)


cancelButton : State msg -> Html msg
cancelButton state =
    customButton [ class "Button-Cancel" ]
        { disabled = False -- TODO: CHANGE TO: not state.closable
        , label = "Cancel"
        , onClick = state.address HideAndClose
        }


navigationButtons : List (Html msg) -> Html msg
navigationButtons buttons =
    div [ class "NavigationButtons" ]
        buttons
