module VaultDialog.Update exposing (..)

import ConfirmationDialog
import Daemon
import Dialog exposing (asModalIn)
import Dict
import Model exposing (Model, vaultWithId)
import Path exposing (folderName)
import Platform.Cmd exposing (map)
import Ports
import RemoteData exposing (RemoteData(..))
import Set
import Syncrypt.Vault exposing (FlyingVault, Vault, VaultId, nameOrId)
import Ui.Input
import Ui.Modal
import Ui.Tabs
import Util exposing (andAlso)
import VaultDialog.Model exposing (CloneStatus(..), FolderItem, Msg(..), RequiresConfirmation(..), State, addFolder, collapseFolder, expandFolder, hasChanged, isIgnored, toggleIgnorePath, toggleUserKey)
import VaultDialog.Ports


open : Model -> ( Model, Cmd Model.Msg )
open model =
    case model.state of
        Model.CreatingNewVault ->
            openNew model

        Model.ShowingVaultDetails vault ->
            openForVault vault model

        Model.ShowingFlyingVaultDetails flyingVault ->
            openForFlyingVault flyingVault model

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


openForFlyingVault : FlyingVault -> Model -> ( Model, Cmd Model.Msg )
openForFlyingVault flyingVault model =
    let
        path =
            []

        ( state, cmd ) =
            VaultDialog.Model.initForFlyingVault flyingVault
                |> setNameInputValue (nameOrId flyingVault)
    in
        (state.modal
            |> Ui.Modal.open
            |> asModalIn state
            |> asStateIn flyingVault.id model
        )
            ! [ Cmd.map (Model.VaultDialog flyingVault.id) cmd ]


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
                    |> fetchUsers vault.id
                , model
                    |> getVaultFingerprints vault.id
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
                    )
            )

        updateMetadataCmd =
            Daemon.updateVaultMetadata
                vaultId
                { name = state.nameInput.value, icon = state.icon }
                model

        ( newModel, modalCmd ) =
            cancel vaultId model

        updateCmd =
            Daemon.getVaults model
    in
        newModel
            ! (updateCmd :: modalCmd :: updateMetadataCmd :: addUserCmds)


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

            NameChanged ->
                (state
                    |> hasChanged
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
                    |> hasChanged
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
                    |> hasChanged
                    |> toggleIgnorePath path
                    |> asStateIn vaultId model
                )
                    ! []

            OpenIconDialog ->
                model
                    ! [ VaultDialog.Ports.openIconFileDialog state.id ]

            SelectedIcon iconUrl ->
                ({ state | icon = Just iconUrl }
                    |> hasChanged
                    |> asStateIn state.id model
                )
                    ! []

            OpenExportDialog ->
                model
                    ! [ VaultDialog.Ports.openExportFileDialog state.id ]

            SelectedExportFile filePath ->
                model
                    ! [ Daemon.exportVault state.id filePath model ]

            OpenFolderDialog ->
                model
                    ! [ VaultDialog.Ports.openFolderDialog state.id ]

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
                        |> hasChanged
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

            Confirm DeleteVault ->
                let
                    title =
                        "Delete vault?"

                    question =
                        "Do you really want to delete this vault from the server?"

                    confirmMsg =
                        Confirmed DeleteVault
                in
                    (state
                        |> ConfirmationDialog.open title question confirmMsg
                        |> asStateIn vaultId model
                    )
                        ! []

            Confirm RemoveVault ->
                let
                    title =
                        "Remove vault from sync?"

                    question =
                        "This vault will stop being synchronized to this computer. Any local file changes won't be uploaded and new files added to the vault won't be downloaded to this computer."

                    confirmMsg =
                        Confirmed RemoveVault
                in
                    (state
                        |> ConfirmationDialog.open title question confirmMsg
                        |> asStateIn vaultId model
                    )
                        ! []

            Confirm AddUser ->
                -- no dialog shown for this, just a button
                model
                    ! []

            Confirmed DeleteVault ->
                model
                    ! [ model.config
                            |> Daemon.deleteVault vaultId
                            |> Cmd.map Model.DeletedVault
                      ]

            Confirmed RemoveVault ->
                model
                    ! [ Daemon.removeVault vaultId model ]

            Confirmed AddUser ->
                let
                    ( input, cmd ) =
                        Ui.Input.setValue "" state.userInput
                            |> dialogCmd UserInput
                in
                    ({ state | userInput = input }
                        |> hasChanged
                        |> asStateIn vaultId model
                    )
                        ! [ cmd ]

            FetchedUsers users ->
                ({ state | users = users }
                    |> asStateIn vaultId model
                )
                    ! []

            UserKeyCheckbox email userKey _ ->
                (state
                    |> toggleUserKey email userKey
                    |> asStateIn vaultId model
                )
                    ! []

            ToggleUserKey email key ->
                (state
                    |> hasChanged
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
                        |> hasChanged
                        |> asStateIn vaultId model
                    )
                        ! []

            SearchUserKeys email ->
                let
                    newState =
                        case Dict.get email state.userKeys of
                            Just (Success _) ->
                                state

                            _ ->
                                { state
                                    | userKeys =
                                        state.userKeys
                                            |> Dict.insert email Loading
                                }
                in
                    (newState
                        |> asStateIn vaultId model
                    )
                        ! [ model
                                |> searchFingerprints email vaultId
                          ]

            FoundUserKeys email keys ->
                ({ state | userKeys = Dict.insert email keys state.userKeys }
                    |> asStateIn vaultId model
                )
                    ! []

            GetVaultFingerprints ->
                model
                    ! [ getVaultFingerprints state.id model ]

            FoundVaultFingerprints data ->
                ({ state | vaultFingerprints = RemoteData.map Set.fromList data }
                    |> asStateIn state.id model
                )
                    |> Model.retryOnFailure data (Model.VaultDialog vaultId GetVaultFingerprints)

            SetUserInput email ->
                let
                    ( userInput, cmd ) =
                        Ui.Input.setValue email state.userInput
                            |> dialogCmd UserInput
                in
                    ({ state
                        | userInput = userInput
                        , userKeys =
                            Dict.update email
                                (Just << Maybe.withDefault Loading)
                                state.userKeys
                     }
                        |> asStateIn vaultId model
                    )
                        ! [ cmd
                          , searchFingerprints email vaultId model
                          ]


getVaultFingerprints vaultId model =
    model.config
        |> Daemon.getVaultFingerprints vaultId
        |> Cmd.map (Model.VaultDialog vaultId << FoundVaultFingerprints)


searchFingerprints email vaultId model =
    model.config
        |> Daemon.getUserKeys email
        |> Cmd.map (Model.VaultDialog vaultId << FoundUserKeys email)


fetchUsers vaultId model =
    model.config
        |> Daemon.getVaultUsers vaultId
        |> Cmd.map (Model.VaultDialog vaultId << FetchedUsers)


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


isOwner : VaultId -> Model -> Bool
isOwner vaultId model =
    let
        state =
            dialogState vaultId model
    in
        case model.login of
            Model.Unknown ->
                False

            Model.LoggedOut ->
                state.cloneStatus == New

            Model.LoggedIn { email } ->
                case List.head <| RemoteData.withDefault [] state.users of
                    Nothing ->
                        state.cloneStatus == New

                    Just owner ->
                        email == owner.email
