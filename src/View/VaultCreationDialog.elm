module View.VaultCreationDialog exposing (..)

import Html exposing (Html, button, div, form, input, label, span, text)
import Html.Attributes exposing (class, for, type_, value, width)
import Ui.Modal
import Ui.Input
import View.Css.Util
import View.Css.VaultCreationDialog exposing (..)


{ id, class, classList } =
    View.Css.Util.namespacedHelpers View.Css.VaultCreationDialog.namespace


type alias State =
    { title : String
    , modal : Ui.Modal.Model
    , nameInput : Ui.Input.Model
    }


type Msg
    = Modal Ui.Modal.Msg
    | NameInput Ui.Input.Msg


init : State
init =
    { title = "Untitled Vault"
    , modal =
        Ui.Modal.init
            |> Ui.Modal.closable True
            |> Ui.Modal.backdrop False
    , nameInput =
        Ui.Input.init ()
            |> Ui.Input.placeholder "Vault Name"
            |> Ui.Input.showClearIcon True
    }


open : State -> State
open model =
    { model | modal = Ui.Modal.open model.modal }


close : State -> State
close model =
    { model | modal = Ui.Modal.close model.modal }


update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case msg of
        Modal msg ->
            { state | modal = Ui.Modal.update msg state.modal }
                ! []

        NameInput msg ->
            let
                ( nameInput, cmd ) =
                    Ui.Input.update msg state.nameInput
            in
                { state | nameInput = nameInput }
                    ! [ Cmd.map NameInput cmd ]


view : State -> Html Msg
view state =
    let
        viewConfig =
            { address = Modal
            , contents = contents state
            , footer = []
            , title = "Create New Vault"
            }
    in
        Ui.Modal.view viewConfig state.modal


contents : State -> List (Html Msg)
contents state =
    [ div [ class [ Content ] ]
        [ nameInput state
        ]
    ]


nameInput : State -> Html Msg
nameInput { nameInput } =
    Ui.Input.view nameInput
        |> Html.map NameInput
