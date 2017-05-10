module VaultDialog.Update exposing (..)

import ConfirmationDialog
import Daemon
import Dialog exposing (asModalIn)
import Dict
import Http
import Model exposing (Model, vaultWithId)
import Path exposing (folderName)
import Platform.Cmd exposing (map)
import Ports
import Syncrypt.Vault exposing (Vault, VaultId, nameOrId)
import Task
import Ui.Input
import Ui.Modal
import Ui.Tabs
import VaultDialog.Model
    exposing
        ( FolderItem
        , Msg(..)
        , State
        , addFolder
        , collapseFolder
        , expandFolder
        , isIgnored
        , toggleIgnorePath
        , toggleUserKey
        )
import VaultDialog.Ports


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
                , model
                    |> fetchUsers vault.id Daemon.attempt
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


saveVaultChanges : VaultId -> State -> Model -> ( Model, Cmd Model.Msg )
saveVaultChanges vaultId state model =
    let
        addUserCmds =
            (state.usersToAdd
                |> Dict.toList
                |> List.map
                    (\( email, keys ) ->
                        model.config
                            |> Daemon.addVaultUser vaultId email keys
                            |> Daemon.attempt (Model.VaultUserAdded vaultId email)
                    )
            )

        updateMetadataCmd =
            model.config
                |> Daemon.updateVaultMetadata vaultId { name = state.nameInput.value }
                |> Daemon.attempt (Model.VaultMetadataUpdated vaultId)

        ( newModel, modalCmd ) =
            cancel vaultId model
    in
        model
            ! (modalCmd :: updateMetadataCmd :: addUserCmds)


update : VaultDialog.Model.Msg -> VaultId -> Model -> ( Model, Cmd Model.Msg )
update msg vaultId ({ vaultDialogs } as model) =
    let
        dialogMsg msg =
            (Model.VaultDialog vaultId) << msg

        dialogCmd msg ( model, cmd ) =
            ( model, cmd |> map (dialogMsg msg) )

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

            ConfirmationDialog msg ->
                (state
                    |> ConfirmationDialog.update msg
                    |> asStateIn vaultId model
                )
                    ! []

            NameInput msg ->
                let
                    ( nameInput, cmd ) =
                        Ui.Input.update msg state.nameInput
                            |> dialogCmd NameInput
                in
                    ({ state | nameInput = nameInput }
                        |> asStateIn vaultId model
                    )
                        ! [ cmd ]

            UserInput msg ->
                let
                    ( userInput, cmd ) =
                        Ui.Input.update msg state.userInput
                            |> dialogCmd UserInput
                in
                    ({ state | userInput = userInput }
                        |> asStateIn vaultId model
                    )
                        ! [ cmd ]

            Tabs msg ->
                let
                    ( tabs, cmd ) =
                        Ui.Tabs.update msg state.tabs
                            |> dialogCmd Tabs
                in
                    ({ state | tabs = tabs }
                        |> asStateIn vaultId model
                    )
                        ! [ cmd ]

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
                    ! [ VaultDialog.Ports.openFolderDialog vaultId ]

            OpenFolder folderPath ->
                model
                    ! [ Ports.openVaultFolder folderPath ]

            SelectedFolder path ->
                let
                    ( nameInput, nameInputCmd ) =
                        Ui.Input.setValue (folderName path) state.nameInput
                            |> dialogCmd NameInput
                in
                    ({ state
                        | localFolderPath = Just path
                        , localFolderItems = Dict.empty
                        , nameInput = nameInput
                     }
                        |> asStateIn vaultId model
                    )
                        ! [ VaultDialog.Ports.getFileList ( vaultId, path )
                          , nameInputCmd
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
                let
                    title =
                        "Delete Vault?"

                    question =
                        "Do you really want to delete this vault from the server?"

                    confirmMsg =
                        ConfirmedVaultDeletion
                in
                    (state
                        |> ConfirmationDialog.open title question confirmMsg
                        |> asStateIn vaultId model
                    )
                        ! []

            ConfirmedVaultDeletion ->
                model
                    ! [ model.config
                            |> Daemon.deleteVault state.id
                            |> Daemon.attempt Model.DeletedVault
                      ]

            FetchedUsers (Ok users) ->
                ({ state | users = users }
                    |> asStateIn vaultId model
                )
                    ! []

            FetchedUsers (Err reason) ->
                ( model
                , model
                    |> fetchUsers vaultId (Daemon.attemptDelayed 1000)
                )

            UserKeyCheckbox email userKey _ ->
                (state
                    |> toggleUserKey email userKey
                    |> asStateIn vaultId model
                )
                    ! []

            ToggleUserKey email key ->
                (state
                    |> toggleUserKey email key
                    |> asStateIn vaultId model
                )
                    ! []

            AddUserWithKeys email keys ->
                let
                    usersToAdd =
                        Dict.insert email keys state.usersToAdd
                in
                    ({ state | usersToAdd = usersToAdd }
                        |> asStateIn vaultId model
                    )
                        ! []

            ConfirmAddUser ->
                let
                    ( input, cmd ) =
                        Ui.Input.setValue "" state.userInput
                            |> dialogCmd UserInput
                in
                    ({ state | userInput = input }
                        |> asStateIn vaultId model
                    )
                        ! [ cmd ]

            SearchUserKeys email ->
                ( model
                , model
                    |> searchFingerprints email vaultId Daemon.attempt
                )

            FoundUserKeys email (Ok keys) ->
                ({ state
                    | userKeys =
                        Dict.insert email keys state.userKeys
                 }
                    |> asStateIn vaultId model
                )
                    ! []

            FoundUserKeys email (Err reason) ->
                let
                    _ =
                        Debug.log "No fingerprints found for email: " email
                in
                    model
                        ! []

            SetUserInput email ->
                let
                    ( userInput, cmd ) =
                        Ui.Input.setValue email state.userInput
                            |> dialogCmd UserInput
                in
                    ({ state | userInput = userInput }
                        |> asStateIn vaultId model
                    )
                        ! [ cmd
                          , searchFingerprints email vaultId Daemon.attempt model
                          ]


searchFingerprints email vaultId attemptFunc model =
    model.config
        |> Daemon.getUserKeys email
        |> attemptFunc (Model.VaultDialog vaultId << FoundUserKeys email)


fetchUsers vaultId attemptFunc model =
    model.config
        |> Daemon.getVaultUsers vaultId
        |> attemptFunc (Model.VaultDialog vaultId << FetchedUsers)


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
