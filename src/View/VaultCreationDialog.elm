module View.VaultCreationDialog exposing (..)

import Ui.Modal as Modal
import Html exposing (Html)


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
            |> Modal.backdrop True
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
            { contents = []
            , footer = []
            , title = "Create New Vault"
            , address = \msg -> msg
            }
    in
        Modal.view viewConfig state.modal
