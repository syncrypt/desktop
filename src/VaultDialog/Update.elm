module VaultDialog.Update exposing (..)

import Model exposing (Model, vaultWithId)
import Ui.Modal
import Ui.Input
import Ui.Tabs
import VaultDialog.Ports
import VaultDialog.Model
    exposing
        ( FolderItem
        , Msg(..)
        , State
        , isIgnored
        , addFolder
        , toggleIgnorePath
        , expandFolder
        , collapseFolder
        )
import Path exposing (folderName)
import Dialog exposing (asModalIn)
import Platform.Cmd exposing (map)
import Dict
import Syncrypt.Vault exposing (VaultId, Vault, nameOrId)


open : Model -> ( Model, Cmd Model.Msg )
open model =
    case model.state of
        Model.CreatingNewVault ->
            openNew model

        Model.ShowingVaultDetails vault ->
            openForVault vault model

        _ ->
            let
                _ =
                    Debug.log "Invalid state for VaultDialog.open: " model.state
            in
                model
                    ! []


openNew : Model -> ( Model, Cmd Model.Msg )
openNew model =
    let
        state =
            dialogState "" model
    in
        (state.modal
            |> Ui.Modal.open
            |> asModalIn state
            |> asStateIn "" model
        )
            ! []


openForVault : Vault -> Model -> ( Model, Cmd Model.Msg )
openForVault vault model =
    let
        ( isNewlyCreated, ( state, cmd ) ) =
            case Dict.get vault.id model.vaultDialogs of
                Nothing ->
                    let
                        ( state, cmd ) =
                            VaultDialog.Model.initForVault vault
                                |> setNameInputValue (nameOrId vault)
                    in
                        ( True
                        , ( state, Cmd.map (Model.VaultDialog vault.id) cmd )
                        )

                Just s ->
                    ( False
                    , ( s, Cmd.none )
                    )

        path =
            Maybe.withDefault [] state.localFolderPath

        commands =
            if isNewlyCreated then
                [ cmd
                , VaultDialog.Ports.getFileList ( vault.id, path )
                ]
            else
                [ cmd ]
    in
        (state.modal
            |> Ui.Modal.open
            |> asModalIn state
            |> asStateIn vault.id model
        )
            ! commands


cancel : VaultId -> Model -> ( Model, Cmd Model.Msg )
cancel vaultId model =
    let
        state =
            dialogState vaultId model
    in
        { model | vaultDialogs = Dict.remove vaultId model.vaultDialogs }
            ! []


close : VaultId -> Model -> ( Model, Cmd Model.Msg )
close vaultId model =
    let
        state =
            dialogState vaultId model
    in
        (state.modal
            |> Ui.Modal.close
            |> asModalIn state
            |> asStateIn vaultId model
        )
            ! []


dialogState : VaultId -> Model -> State
dialogState vaultId model =
    case Dict.get vaultId model.vaultDialogs of
        Just state ->
            state

        Nothing ->
            VaultDialog.Model.initForVault (vaultWithId vaultId model)


update : VaultDialog.Model.Msg -> VaultId -> Model -> ( Model, Cmd Model.Msg )
update msg vaultId ({ vaultDialogs } as model) =
    let
        state =
            dialogState vaultId model
    in
        case msg of
            Modal msg ->
                (state.modal
                    |> Ui.Modal.update msg
                    |> asModalIn state
                    |> asStateIn vaultId model
                )
                    ! []

            ConfirmationModal msg ->
                ({ state
                    | deleteConfirmationModal =
                        Ui.Modal.update msg state.deleteConfirmationModal
                 }
                    |> asStateIn vaultId model
                )
                    ! []

            NameInput msg ->
                let
                    ( nameInput, cmd ) =
                        Ui.Input.update msg state.nameInput
                in
                    ({ state | nameInput = nameInput }
                        |> asStateIn vaultId model
                    )
                        ! [ cmd |> map (NameInput >> Model.VaultDialog vaultId) ]

            Tabs msg ->
                let
                    ( tabs, cmd ) =
                        Ui.Tabs.update msg state.tabs
                in
                    ({ state | tabs = tabs }
                        |> asStateIn vaultId model
                    )
                        ! [ cmd |> map (Tabs >> Model.VaultDialog vaultId) ]

            FileCheckBox path _ ->
                (state
                    |> toggleIgnorePath path
                    |> asStateIn vaultId model
                )
                    ! []

            NestedFileList rootPath f ->
                if Just rootPath /= state.localFolderPath then
                    model
                        ! []
                else
                    (state
                        |> addFolder f
                        |> asStateIn vaultId model
                    )
                        ! []

            ToggleIgnorePath path ->
                (state
                    |> toggleIgnorePath path
                    |> asStateIn vaultId model
                )
                    ! []

            OpenFolderDialog vaultId ->
                model
                    ! [ VaultDialog.Ports.openFolder vaultId ]

            SelectedFolder path ->
                let
                    ( nameInput, nameInputCmd ) =
                        Ui.Input.setValue (folderName path) state.nameInput
                in
                    ({ state
                        | localFolderPath = Just path
                        , localFolderItems = Dict.empty
                        , nameInput = nameInput
                     }
                        |> asStateIn vaultId model
                    )
                        ! [ VaultDialog.Ports.getFileList ( vaultId, path )
                          , Cmd.map (NameInput >> Model.VaultDialog vaultId) nameInputCmd
                          ]

            CollapseFolder path ->
                (state
                    |> collapseFolder path
                    |> asStateIn vaultId model
                )
                    ! []

            ExpandFolder path ->
                (state
                    |> expandFolder path
                    |> asStateIn vaultId model
                )
                    ! []

            AskDeleteVault ->
                ({ state
                    | deleteConfirmationModal = Ui.Modal.open state.deleteConfirmationModal
                 }
                    |> asStateIn vaultId model
                )
                    ! []

            CancelDeleteVault ->
                ({ state
                    | deleteConfirmationModal = Ui.Modal.close state.deleteConfirmationModal
                 }
                    |> asStateIn vaultId model
                )
                    ! []


asStateIn : VaultId -> Model -> State -> Model
asStateIn vaultId model state =
    { model
        | vaultDialogs =
            Dict.insert vaultId state model.vaultDialogs
    }


setNameInputValue : String -> State -> ( State, Cmd Msg )
setNameInputValue value state =
    let
        ( nameInput, cmd ) =
            state.nameInput
                |> Ui.Input.setValue value
    in
        ( { state | nameInput = nameInput }
        , Cmd.map NameInput cmd
        )
