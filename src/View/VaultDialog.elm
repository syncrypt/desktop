module View.VaultDialog exposing (..)

import Html exposing (Html, button, div, h1, hr, node, span, text)
import Html.Events exposing (onClick)
import View.Css.Util
import View.Css.VaultDialog exposing (..)
import Ui.Modal
import Ui.Input
import Syncrypt.Vault exposing (Vault)


{ id, class, classList } =
    View.Css.Util.namespacedHelpers View.Css.VaultDialog.namespace


type alias State =
    { vault : Vault
    , modal : Ui.Modal.Model
    , nameInput : Ui.Input.Model
    }


type Msg
    = Modal Ui.Modal.Msg
    | NameInput Ui.Input.Msg


init : Vault -> State
init vault =
    { vault = vault
    , modal =
        Ui.Modal.init
            |> Ui.Modal.closable True
            |> Ui.Modal.backdrop False
    , nameInput =
        Ui.Input.init ()
            |> Ui.Input.placeholder (Maybe.withDefault "" vault.name)
            |> Ui.Input.showClearIcon True
    }


open : Vault -> State -> State
open vault state =
    let
        -- ignore cmd for now
        ( nameInput, _ ) =
            state.nameInput
                |> Ui.Input.setValue (Maybe.withDefault "" vault.name)
    in
        { state
            | vault = vault
            , modal = Ui.Modal.open state.modal
            , nameInput = nameInput
        }


close : Maybe State -> Maybe State
close state =
    case state of
        Nothing ->
            Nothing

        Just state ->
            Just { state | modal = Ui.Modal.close state.modal }


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
            , title = "Vault Details for " ++ state.vault.id
            }
    in
        Ui.Modal.view viewConfig state.modal


contents : State -> List (Html Msg)
contents state =
    [ div [ class [ Content ] ]
        [ text <| "Modal content for vault: " ++ state.vault.id
        , nameInput state
        ]
    ]


nameInput : State -> Html Msg
nameInput { nameInput } =
    Ui.Input.view nameInput
        |> Html.map NameInput
