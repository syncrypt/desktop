module WizardDialog.Model
    exposing
        ( State
        , Msg(..)
        , Step(..)
        , StepSettings
        , Button(..)
        , ButtonSettings(..)
        , HasWizardDialog
        , WizardSettings
        , WizardType(..)
        , init
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
    , steps : Int
    }


type Button msg
    = Next
    | Prev
    | Cancel
    | Finish
    | CustomButton (List (Html.Attribute msg)) String msg


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


type alias State msg =
    { modal : Ui.Modal.Model
    , address : Msg -> msg
    , wizardType : WizardType
    , steps : Int
    , currentStep : Int
    , onFinishMsg : Maybe msg
    }


type alias HasWizardDialog a m =
    { a | wizardDialog : Maybe (State m) }


type Msg
    = Modal Ui.Modal.Msg
    | Show
    | Hide
    | Close
    | ToNextStep
    | ToPreviousStep
    | ToStep Int
    | FinishWizard


init : WizardSettings msg -> State msg
init { wizardType, address, steps, onFinishMsg } =
    { modal =
        Ui.Modal.init
            |> Ui.Modal.closable True
            |> Ui.Modal.backdrop True
    , address = address
    , wizardType = wizardType
    , steps = steps
    , currentStep = 1
    , onFinishMsg = onFinishMsg
    }
