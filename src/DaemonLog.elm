module DaemonLog exposing (dialogSettings, dialogViewSettings)

import Dialog exposing (labeledItem)
import Html exposing (Html, div, input, p, span, text)
import Html.Attributes exposing (class, type_)
import Html.Events exposing (onClick, onInput)
import Language exposing (Language(..))
import Model
import Util exposing (ButtonSettings, Position(..), button)
import WizardDialog.Model exposing (..)


steps : List (StepConfig Model.Model Model.Msg)
steps =
    [ ( "DaemonLog", step1 ) ]


dialogSettings : Model.Model -> WizardSettings Model.Msg
dialogSettings model =
    { address = Model.WizardDialogMsg
    , onFinishMsg = Nothing
    , steps = steps |> List.map Tuple.first
    , wizardType = DaemonLogDialog
    , closable = True
    }


wizardContent : List (Html msg) -> Html msg
wizardContent body =
    div [ class "MainScreen-DaemonLogDialog" ]
        body


dialogViewSettings : State Model.Msg -> Model.Model -> Maybe (ViewSettings Model.Msg)
dialogViewSettings state model =
    WizardDialog.Model.viewSettings steps state model


infoTextLine : String -> Html msg
infoTextLine line =
    span []
        [ text line ]



-- STEPS


step1 model state =
    Just
        { title = "Daemon Log"
        , contents =
            wizardContent
                [ text "TODO: Daemon log with filter, search & sorting goes here" ]
        , buttons = Visible [ CustomButton [] { label = "Close", onClick = Model.CloseDaemonLogDialog } ]
        }
