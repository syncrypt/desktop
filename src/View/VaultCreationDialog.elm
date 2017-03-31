module View.VaultCreationDialog exposing (..)

import Html exposing (Html, button, div, form, input, label, span, text)
import Html.Attributes exposing (class, for, type_, width)
import Html.CssHelpers
import Html.Events exposing (onClick)
import Ui.Modal as Modal
import View.Css.VaultCreationDialog exposing (..)
import View.Css.Util


{ id, class, classList } =
    View.Css.Util.namespacedHelpers View.Css.VaultCreationDialog.namespace


type alias State =
    { title : String
    , modal : Modal.Model
    }


type alias Msg =
    Modal.Msg


init : State
init =
    { title = "Untitled Vault"
    , modal =
        Modal.init
            |> Modal.closable True
            |> Modal.backdrop False
    }


open : State -> State
open model =
    { model | modal = Modal.open model.modal }


close : State -> State
close model =
    { model | modal = Modal.close model.modal }


update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    let
        newState =
            Modal.update msg state.modal
    in
        { state | modal = newState } ! []


view : State -> Html Msg
view state =
    let
        viewConfig =
            { contents = contents
            , footer = []
            , title = "Create New Vault"
            , address = \msg -> msg
            }
    in
        Modal.view viewConfig state.modal


contents : List (Html Msg)
contents =
    [ div [ class [ Content ] ]
        [ form []
            [ textField "Name"
            ]
        ]
    ]


textField : String -> Html msg
textField description =
    div [ class [ FormInput ] ]
        [ label []
            [ span [ class [ FormLabel ] ]
                [ text description ]
            , input [ type_ "text" ]
                []
            ]
        ]
