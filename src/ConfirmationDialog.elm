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

import Html exposing (Html, div, form, span, text)
import Html.Attributes exposing (class)
import Http
import Ui.Button
import Ui.Modal


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
        Modal msg ->
            { model
                | confirmationDialog =
                    { confirmationDialog
                        | modal = Ui.Modal.update msg confirmationDialog.modal
                    }
            }

        Close ->
            model
                |> close


view : HasConfirmationDialog a msg -> Html msg
view { confirmationDialog } =
    -- don't display anything unless we have messages to produce
    case confirmationDialog.view of
        Nothing ->
            text ""

        Just view ->
            let
                viewConfig =
                    { address = confirmationDialog.address << Modal
                    , contents = contents confirmationDialog.address view
                    , footer = []
                    , title = view.title
                    }
            in
            Ui.Modal.view viewConfig confirmationDialog.modal


contents : (Msg -> msg) -> ViewSettings msg -> List (Html msg)
contents address { question, confirmMsg } =
    [ div [ class "ConfirmationDialog-Content" ]
        [ text question
        , div [ class "ConfirmationDialog-Buttons" ]
            [ span [ class "ConfirmationDialog-Button-Cancel" ]
                [ Ui.Button.model "Cancel" "secondary" "small"
                    |> Ui.Button.view (address Close)
                ]
            , span [ class "ConfirmationDialog-Button-Confirm" ]
                [ Ui.Button.model "Confirm" "danger" "small"
                    |> Ui.Button.view confirmMsg
                ]
            ]
        ]
    ]
