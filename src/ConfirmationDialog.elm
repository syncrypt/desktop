module ConfirmationDialog
    exposing
        ( Model
        , Msg(..)
        , close
        , init
        , open
        , update
        , view
        )

import Html exposing (Html, div, span, text)
import Html.Attributes exposing (class)
import Language exposing (Language)
import Translation as T
import Ui.Modal
import Util exposing (button)


type alias ViewSettings msg =
    { title : String
    , question : String
    , confirmMsg : msg
    }


type alias Model msg =
    { modal : Ui.Modal.Model
    , view : Maybe (ViewSettings msg)
    , address : Msg -> msg
    }


type alias HasConfirmationDialog a m =
    { a | confirmationDialog : Model m }


type Msg
    = Modal Ui.Modal.Msg
    | Close


init : (Msg -> msg) -> Model msg
init address =
    { modal =
        Ui.Modal.init
            |> Ui.Modal.closable False
            |> Ui.Modal.backdrop True
    , view = Nothing
    , address = address
    }


open :
    String
    -> String
    -> msg
    -> HasConfirmationDialog a msg
    -> HasConfirmationDialog a msg
open title question confirmMsg ({ confirmationDialog } as model) =
    { model
        | confirmationDialog =
            { confirmationDialog
                | modal = Ui.Modal.open confirmationDialog.modal
                , view =
                    Just
                        { title = title
                        , question = question
                        , confirmMsg = confirmMsg
                        }
            }
    }


close : HasConfirmationDialog a msg -> HasConfirmationDialog a msg
close ({ confirmationDialog } as model) =
    { model
        | confirmationDialog =
            { confirmationDialog
                | modal = Ui.Modal.close confirmationDialog.modal
                , view = Nothing
            }
    }


update : Msg -> HasConfirmationDialog a msg -> HasConfirmationDialog a msg
update msg ({ confirmationDialog } as model) =
    case msg of
        Modal modelMsg ->
            { model
                | confirmationDialog =
                    { confirmationDialog
                        | modal = Ui.Modal.update modelMsg confirmationDialog.modal
                    }
            }

        Close ->
            model
                |> close


view : Language -> HasConfirmationDialog a msg -> Html msg
view language { confirmationDialog } =
    -- don't display anything unless we have messages to produce
    case confirmationDialog.view of
        Nothing ->
            text ""

        Just dialogView ->
            let
                viewConfig =
                    { address = confirmationDialog.address << Modal
                    , contents = contents confirmationDialog.address language dialogView
                    , footer = []
                    , title = dialogView.title
                    }
            in
            Ui.Modal.view viewConfig confirmationDialog.modal


contents : (Msg -> msg) -> Language -> ViewSettings msg -> List (Html msg)
contents address language { question, confirmMsg } =
    [ div [ class "ConfirmationDialog-Content" ]
        [ text question
        , div [ class "ConfirmationDialog-Buttons" ]
            [ span [ class "ConfirmationDialog-Button-Cancel" ]
                [ button []
                    { label = T.translate language T.Cancel
                    , onClick = address Close
                    }
                ]
            , span [ class "ConfirmationDialog-Button-Confirm" ]
                [ button []
                    { label = T.translate language T.Confirm
                    , onClick = confirmMsg
                    }
                ]
            ]
        ]
    ]
