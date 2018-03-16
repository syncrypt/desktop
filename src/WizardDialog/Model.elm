module WizardDialog.Model
    exposing
        ( Button(..)
        , ButtonSettings(..)
        , CustomNavButton(..)
        , HasWizardDialog
        , Msg(..)
        , NavButtons
        , State
        , StepConfig
        , ViewSettings
        , WizardSettings
        , WizardType(..)
        , asWizardIn
        , buttonToStep
        , currentStep
        , hasNextStep
        , hasPreviousStep
        , init
        , moveToNextStep
        , moveToPreviousStep
        , moveToStep
        , moveToStepWithName
        , toNextStep
        , toPreviousStep
        , toStep
        , toStepWithName
        , viewSettings
        )

import Html exposing (Html)
import Language exposing (Language)
import Ui.Modal
import Util


type alias Step =
    Int


type WizardType
    = SetupWizard
    | FeedbackWizard


type alias WizardSettings msg =
    { address : Msg -> msg
    , wizardType : WizardType
    , onFinishMsg : Maybe msg
    , steps : List String
    , closable : Bool
    }


type Button msg
    = Previous
    | Next
    | Cancel
    | Finish
    | CloseBtn (Maybe msg)
    | CustomButton (List (Html.Attribute msg)) { label : String, onClick : msg }


type CustomNavButton msg
    = Nav msg
    | NavWithLabel msg String
    | Auto
    | AutoWithLabel String
    | Hidden


type alias NavButtons msg =
    { prev : CustomNavButton msg
    , next : CustomNavButton msg
    }


type ButtonSettings msg
    = Default
    | DefaultNoCancel
    | Visible (List (Button msg))
    | CustomNav (NavButtons msg)
    | CustomNavNoCancel (NavButtons msg)


type alias ViewSettings msg =
    { title : String
    , contents : Html msg
    , buttons : ButtonSettings msg
    }


type alias State msg =
    { modal : Ui.Modal.Model
    , address : Msg -> msg
    , wizardType : WizardType
    , steps : List String
    , currentStep : Int
    , onFinishMsg : Maybe msg
    , closable : Bool
    }


type alias HasWizardDialog a m =
    { a
        | wizardDialog : Maybe (State m)
        , language : Language
    }


type Msg
    = Modal Ui.Modal.Msg
    | Show
    | Hide
    | Close
    | HideAndClose
    | ToNextStep
    | ToPreviousStep
    | ToStep Step
    | ToStepWithName String
    | FinishWizard


type alias StepConfig model msg =
    ( String, model -> State msg -> Maybe (ViewSettings msg) )


init : WizardSettings msg -> State msg
init { wizardType, address, steps, onFinishMsg, closable } =
    { modal =
        Ui.Modal.init
            |> Ui.Modal.closable closable
            |> Ui.Modal.backdrop True
    , address = address
    , wizardType = wizardType
    , steps = steps
    , currentStep = 1
    , onFinishMsg = onFinishMsg
    , closable = closable
    }


stepCount : State msg -> Int
stepCount { steps } =
    List.length steps


hasPreviousStep : State msg -> Bool
hasPreviousStep { currentStep } =
    currentStep > 1


hasNextStep : State msg -> Bool
hasNextStep state =
    state.currentStep < stepCount state


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


toStep : Int -> State msg -> State msg
toStep step state =
    if step > 0 && step <= stepCount state then
        { state | currentStep = step }
    else
        state


undefinedStep : Step
undefinedStep =
    -1


toStepWithName : String -> State msg -> State msg
toStepWithName name state =
    let
        step =
            state.steps
                |> Util.findIndex ((==) name)
                |> Maybe.map ((+) 1)
                |> Maybe.withDefault undefinedStep
    in
    toStep step state


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


moveToStep : Step -> HasWizardDialog a msg -> HasWizardDialog a msg
moveToStep step ({ wizardDialog } as model) =
    wizardDialog
        |> Maybe.map (toStep step)
        |> asWizardIn model


moveToStepWithName : String -> HasWizardDialog a msg -> HasWizardDialog a msg
moveToStepWithName stepName ({ wizardDialog } as model) =
    wizardDialog
        |> Maybe.map (toStepWithName stepName)
        |> asWizardIn model


asWizardIn : HasWizardDialog a msg -> Maybe (State msg) -> HasWizardDialog a msg
asWizardIn model maybeWizard =
    { model | wizardDialog = maybeWizard }


buttonToStep : List (Html.Attribute msg) -> String -> Step -> State msg -> Button msg
buttonToStep attrs label step state =
    CustomButton attrs
        { label = label
        , onClick = state.address <| ToStep step
        }


currentStep : List (StepConfig model msg) -> State msg -> Maybe (StepConfig model msg)
currentStep steps { currentStep } =
    steps
        |> List.drop (currentStep - 1)
        |> List.head


viewSettings : List (StepConfig model msg) -> State msg -> model -> Maybe (ViewSettings msg)
viewSettings steps state model =
    state
        |> currentStep steps
        |> Maybe.map (\( _, f ) -> f model state)
        |> Maybe.withDefault Nothing
