module WizardDialog.Model
    exposing
        ( Button(..)
        , ButtonSettings(..)
        , HasWizardDialog
        , Msg(..)
        , State
        , ViewSettings
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
    = Previous
    | Next
    | Cancel
    | Finish
    | CustomButton (List (Html.Attribute msg)) String msg


type ButtonSettings msg
    = Default
    | Visible (List (Button msg))


type alias ViewSettings msg =
    { title : String
    , contents : Html msg
    , buttons : ButtonSettings msg
    }


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
    | HideAndClose
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
