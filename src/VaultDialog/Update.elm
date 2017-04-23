module VaultDialog.Update exposing (..)

import Model exposing (Model, vaultWithId)
import Ui.Modal
import Ui.Input
import VaultDialog.Ports
import VaultDialog.Model
    exposing
        ( FolderItem
        , Msg(..)
        , State
        , isIgnored
        , addFolder
        , toggleIgnorePath
        , folderName
        )
import Dialog exposing (asModalIn)
import Platform.Cmd exposing (map)
import Dict
import Syncrypt.Vault exposing (VaultId, Vault)


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
        state =
            case Dict.get vault.id model.vaultDialogs of
                Nothing ->
                    VaultDialog.Model.initForVault vault

                Just state ->
                    state

        modal =
            state.modal |> Ui.Modal.open
    in
        (modal
            |> asModalIn state
            |> asStateIn vault.id model
        )
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

            NameInput msg ->
                let
                    ( nameInput, cmd ) =
                        Ui.Input.update msg state.nameInput
                in
                    ({ state | nameInput = nameInput }
                        |> asStateIn vaultId model
                    )
                        ! [ cmd |> map (NameInput >> Model.VaultDialog vaultId) ]

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

            OpenFolderDialog ->
                model
                    ! [ VaultDialog.Ports.openFolder () ]

            SelectedFolder path ->
                let
                    ( nameInput, nameInputMsg ) =
                        Ui.Input.setValue (folderName path) state.nameInput
                in
                    ({ state
                        | localFolderPath = Just path
                        , localFolderItems = Dict.empty
                        , nameInput = nameInput
                     }
                        |> asStateIn vaultId model
                    )
                        ! [ VaultDialog.Ports.getFileList path
                          , Cmd.map (NameInput >> Model.VaultDialog vaultId) nameInputMsg
                          ]


asStateIn : VaultId -> Model -> State -> Model
asStateIn vaultId model state =
    { model
        | vaultDialogs =
            Dict.insert vaultId state model.vaultDialogs
    }
