module WizardDialog.Model
    exposing
        ( Button(..)
        , ButtonSettings(..)
        , CustomNavButton(..)
        , HasWizardDialog
        , Msg(..)
        , NavButtons
        , State
        , ViewSettings
        , WizardSettings
        , WizardType(..)
        , asWizardIn
        , buttonToStep
        , hasNextStep
        , hasPreviousStep
        , init
        , moveToNextStep
        , moveToPreviousStep
        , moveToStep
        , toNextStep
        , toPreviousStep
        , toStep
        )

import Html exposing (Html)
import Ui.Modal


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
    | Visible (List (Button msg))
    | CustomNav (NavButtons msg)


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
    { a | wizardDialog : Maybe (State m) }


type Msg
    = Modal Ui.Modal.Msg
    | Show
    | Hide
    | Close
    | HideAndClose
    | ToNextStep
    | ToPreviousStep
    | ToStep Int
    | FinishWizard


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


moveToStep : Int -> HasWizardDialog a msg -> HasWizardDialog a msg
moveToStep step ({ wizardDialog } as model) =
    wizardDialog
        |> Maybe.map (toStep step)
        |> asWizardIn model


asWizardIn : HasWizardDialog a msg -> Maybe (State msg) -> HasWizardDialog a msg
asWizardIn model maybeWizard =
    { model | wizardDialog = maybeWizard }


buttonToStep : List (Html.Attribute msg) -> String -> Int -> State msg -> Button msg
buttonToStep attrs label step state =
    CustomButton attrs
        { label = label
        , onClick = state.address <| ToStep step
        }
