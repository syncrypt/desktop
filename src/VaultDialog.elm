module VaultDialog exposing (..)

import Animation exposing (..)
import ConfirmationDialog
import Date
import Date.Distance
import Dialog exposing (labeledItem)
import Dict
import Html exposing (Html, button, div, form, h4, img, input, label, span, text)
import Html.Attributes exposing (class, classList, for, id, src, style)
import Html.Events exposing (onClick)
import Model exposing (Model)
import Path exposing (Path)
import RemoteData exposing (RemoteData(..))
import Syncrypt.User exposing (Email, User, UserKey)
import Syncrypt.Vault exposing (Vault, VaultId)
import Ui.Button
import Ui.Checkbox
import Ui.Container
import Ui.Input
import Ui.Modal
import Ui.Tabs
import Util
    exposing
        ( Direction(..)
        , TooltipLength(..)
        , onAnyKeyDown
        , onEnter
        , tooltipItem
        )
import VaultDialog.Model
    exposing
        ( CloneStatus(..)
        , FileName
        , FolderItem
        , Msg(..)
        , RequiresConfirmation(..)
        , State
        , folderIsEmpty
        , isExpanded
        , isIgnored
        , isUserKeyAlreadyAdded
        , isUserKeySelected
        , keysToAdd
        , sortedFolders
        , userInputEmail
        , userKeys
        )
import VaultDialog.Ports
import VaultDialog.Update exposing (dialogState, isOwner)


subscriptions : Model -> Sub Model.Msg
subscriptions _ =
    let
        fileListMsg ( vaultId, rootPath, folderItem ) =
            Model.VaultDialog vaultId (NestedFileList rootPath folderItem)

        selectedFolderMsg ( vaultId, path ) =
            Model.VaultDialog vaultId (SelectedFolder path)

        selectedIconMsg ( vaultId, path ) =
            Model.VaultDialog vaultId (SelectedIcon path)
    in
        Sub.batch
            [ VaultDialog.Ports.fileList fileListMsg
            , VaultDialog.Ports.selectedFolder selectedFolderMsg
            , VaultDialog.Ports.selectedIconFile selectedIconMsg
            ]


viewAll : Model -> List (Html Model.Msg)
viewAll ({ vaultDialogs } as model) =
    vaultDialogs
        |> Dict.keys
        |> List.map (\vaultId -> view vaultId model)


view : VaultId -> Model -> Html Model.Msg
view vaultId model =
    let
        state =
            dialogState vaultId model

        viewConfig =
            { address = (Model.VaultDialog vaultId << Modal)
            , contents = contents vaultId model
            , footer = []
            , title =
                case ( vaultId, state.cloneStatus ) of
                    ( _, New ) ->
                        "Create New Vault"

                    ( id, Cloned ) ->
                        "Vault " ++ id

                    ( id, NotCloned ) ->
                        "Vault (not synchronized) " ++ id
            }
    in
        div [ class "VaultDialog" ]
            [ Ui.Modal.view viewConfig state.modal
            ]


contents : VaultId -> Model -> List (Html Model.Msg)
contents vaultId model =
    let
        state =
            dialogState vaultId model

        tabsViewConfig =
            { address = (Model.VaultDialog vaultId << Tabs)
            , contents = tabContents vaultId state model
            }

        tabs =
            Ui.Tabs.view tabsViewConfig state.tabs

        confirmationDialog =
            ConfirmationDialog.view state
                |> Html.map (Model.VaultDialog state.id)

        buttons =
            div [ class "VaultDialog-Buttons" ]
                [ removeButton vaultId state
                , deleteButton vaultId state model
                , saveButton vaultId state
                , cancelButton vaultId state
                ]
    in
        [ tabs
        , confirmationDialog
        , buttons
        ]


tabContents : VaultId -> State -> Model -> List ( String, Html Model.Msg )
tabContents vaultId state model =
    let
        msg =
            Model.VaultDialog vaultId

        -- converter from Html Msg -> Html Model.Msg
        rootMsg =
            Html.map msg

        searchKeys =
            Model.VaultDialog vaultId <|
                SearchUserKeys (userInputEmail state)

        filesTab =
            ( "Name & Files"
            , div [ class "VaultDialog-Tab-Content" ]
                [ dialogInput "Icon"
                    [ rootMsg <| iconInput state model ]
                , dialogInput "Name"
                    [ nameInput msg state ]
                , dialogInput "Folder"
                    [ rootMsg <| openFolderButton vaultId state model ]
                , dialogInput "FileSelection"
                    [ rootMsg <| fileSelectionContainer state ]
                ]
            )

        usersTab =
            ( "Users"
            , div [ class "VaultDialog-Tab-Content" ]
                [ div
                    [ classList [ ( "Hidden", not (isOwner vaultId model) ) ] ]
                    [ div
                        [ class "VaultDialog-Add-User", onEnter searchKeys ]
                        [ dialogInput "User"
                            [ userInput vaultId state ]
                        ]
                    , div [ class "VaultDialog-UserKey-Selection" ]
                        [ rootMsg <| userKeySelection state model
                        , rootMsg <| confirmUserKeysButton state
                        ]
                    ]
                , rootMsg <| userList state model
                , rootMsg <| pendingUserList state
                ]
            )

        cryptoTab =
            ( "Cryptography"
            , div [ class "VaultDialog-Tab-Keys" ]
                [ text "TODO: add section with information on vault keys & other crypto stuff" ]
            )
    in
        if isOwner vaultId model then
            [ filesTab, usersTab, cryptoTab ]
        else
            [ filesTab, cryptoTab ]


dialogInput : String -> List (Html msg) -> Html msg
dialogInput inputClassSuffix body =
    div
        [ class "VaultDialog-Input"
        , class ("VaultDialog-Input-" ++ inputClassSuffix)
        ]
        body


cancelButton : VaultId -> State -> Html Model.Msg
cancelButton vaultId state =
    span
        [ classList
            [ ( "VaultDialog-Button-Cancel", True )
            , ( "Hidden", not state.hasChangesPending )
            ]
        ]
        [ Ui.Button.model "Cancel Changes" "secondary" "small"
            |> Ui.Button.view (Model.CloseVaultDetails vaultId)
        ]


deleteButton : VaultId -> State -> Model -> Html Model.Msg
deleteButton vaultId state model =
    span
        [ classList
            [ ( "Hidden", state.cloneStatus == New || not (isOwner vaultId model) )
            , ( "VaultDialog-Button-Delete", True )
            ]
        ]
        [ Ui.Button.model "Delete from Server" "danger" "small"
            |> Ui.Button.view (Model.VaultDialog vaultId (Confirm DeleteVault))
        ]


removeButton : VaultId -> State -> Html Model.Msg
removeButton vaultId state =
    span
        [ classList
            [ ( "Hidden", state.cloneStatus /= Cloned )
            , ( "VaultDialog-Button-Remove", True )
            ]
        ]
        [ Ui.Button.model "Stop syncing" "warning" "small"
            |> Ui.Button.view (Model.VaultDialog vaultId (Confirm RemoveVault))
        ]


saveButton : VaultId -> State -> Html Model.Msg
saveButton vaultId state =
    let
        ( label, msg ) =
            case ( state.cloneStatus, state.hasChangesPending ) of
                ( New, True ) ->
                    ( "Create", Model.SaveVaultDetails vaultId )

                ( Cloned, True ) ->
                    ( "Save", Model.SaveVaultDetails vaultId )

                ( NotCloned, True ) ->
                    ( "Sync vault to folder", Model.CloneVault vaultId )

                _ ->
                    ( "Close", Model.CloseVaultDetails vaultId )
    in
        span [ class "VaultDialog-Button-Save" ]
            [ Ui.Button.model label "primary" "small"
                |> Ui.Button.view msg
            ]


confirmUserKeysButton : State -> Html Msg
confirmUserKeysButton state =
    let
        email =
            userInputEmail state
    in
        span [ class "VaultDialog-Button-Confirm-UserKeys" ] <|
            if List.isEmpty (keysToAdd email state) then
                []
            else
                [ Ui.Button.model "Invite with selected keys" "primary" "small"
                    |> Ui.Button.view (Confirmed AddUser)
                ]


openFolderButton : VaultId -> State -> Model -> Html Msg
openFolderButton vaultId state model =
    let
        pathString path =
            path
                |> Path.toString model.config.pathSeparator

        ( folderPath, msg, tooltipMsg ) =
            case ( state.cloneStatus, state.localFolderPath ) of
                ( NotCloned, Nothing ) ->
                    ( "Select Folder to clone vault to"
                    , OpenFolderDialog
                    , "By clicking here, you select a folder to use for this vault to download its files to."
                    )

                ( _, Nothing ) ->
                    ( "Select Folder"
                    , OpenFolderDialog
                    , "Select a new folder for this vault."
                    )

                ( New, Just path ) ->
                    ( pathString path
                    , OpenFolderDialog
                    , "This new vault will synchronize files in this folder."
                    )

                ( _, Just path ) ->
                    let
                        ps =
                            pathString path
                    in
                        ( ps
                        , OpenFolder ps
                        , "This vault is synchronizing files from and to this folder."
                        )
    in
        span [ class "VaultDialog-Button-Folder" ]
            [ labeledItem Left
                [ class "VaultDialog-InputLabel" ]
                Nothing
                (text "Folder")
                (tooltipItem Bottom
                    Auto
                    tooltipMsg
                    [ Ui.Button.model folderPath "primary" "medium"
                        |> Ui.Button.view msg
                    ]
                )
            ]


nameInput : (Msg -> Model.Msg) -> State -> Html Model.Msg
nameInput msg state =
    span [ onAnyKeyDown (msg NameChanged) ]
        [ labeledItem Left
            [ class "VaultDialog-InputLabel" ]
            (Just (Model.FocusOn state.nameInput.uid))
            (text "Name")
            (tooltipItem Bottom
                Auto
                "The name of the vault. Chosen by the owner."
                [ Ui.Input.view state.nameInput
                    |> Html.map (msg << NameInput)
                ]
            )
        ]


iconInput : State -> Model -> Html Msg
iconInput state model =
    let
        iconPath =
            case state.icon of
                Nothing ->
                    ""

                Just path ->
                    path

        icon =
            img
                ((src iconPath)
                    :: (if isOwner state.id model then
                            [ class "VaultDialog-Icon Pointer-Cursor", onClick OpenIconDialog ]
                        else
                            [ class "VaultDialog-Icon" ]
                       )
                )
                []
    in
        tooltipItem Right
            Auto
            "Vault icon that can be seen by any invited user"
            [ icon ]


userInput : VaultId -> State -> Html Model.Msg
userInput vaultId state =
    labeledItem Left
        [ class "VaultDialog-InputLabel" ]
        (Just (Model.FocusOn state.userInput.uid))
        (text "Invite User")
        (tooltipItem Bottom
            Auto
            "Search for a user's email address to add them to this vault"
            [ Ui.Input.view
                state.userInput
                |> Html.map (Model.VaultDialog vaultId << UserInput)
            ]
        )


fileSelectionContainer : State -> Html Msg
fileSelectionContainer state =
    let
        settings =
            { direction = "column"
            , compact = False
            , align = "start"
            }

        hasFiles =
            not <| Dict.isEmpty state.localFolderItems

        body =
            if hasFiles then
                [ labeledItem Left
                    [ class "VaultDialog-InputLabel" ]
                    Nothing
                    (text "Files")
                    (Ui.Container.view settings [] (renderFolders state))
                ]
            else
                []
    in
        div [ class "VaultDialog-FileSelection" ]
            body


renderFolders : State -> List (Html Msg)
renderFolders state =
    case sortedFolders state of
        ( _, rootFileNames ) :: folders ->
            let
                rootFiles =
                    List.map (renderFile state []) rootFileNames

                rootFolders =
                    List.map (renderFolder state) folders
            in
                [ tooltipItem Top
                    XLarge
                    "This shows all local files in your vault. Toggle individual files or whole subdirectories from automated synchronization if you don't want all files to be uploaded & synchronized automatically."
                    [ div []
                        (List.foldr (::)
                            []
                            (rootFiles ++ rootFolders)
                        )
                    ]
                ]

        [] ->
            []


renderFolder : State -> FolderItem -> Html Msg
renderFolder state ( path, files ) =
    if isExpanded path state then
        div [ class "VaultDialog-FolderItem" ] <|
            (inFolderPath path
                [ span []
                    [ fileCheckbox path state
                    , folderCollapseToggle path state
                    ]
                , div (hiddenIfIgnored path state [])
                    [ div [ class "VaultDialog-FolderItem-Nested" ]
                        (List.map (renderFile state path) files)
                    ]
                ]
            )
    else
        div [ class "VaultDialog-FolderItem-Collapsed" ]
            (inFolderPath path
                [ span []
                    [ fileCheckbox path state
                    , folderCollapseToggle path state
                    ]
                ]
            )


renderFile : State -> Path -> FileName -> Html Msg
renderFile state folderPath path =
    let
        filePath =
            folderPath ++ [ path ]
    in
        div [ class "VaultDialog-File" ] <|
            [ fileCheckbox filePath state
            ]


hiddenIfIgnored : Path -> State -> List (Html.Attribute msg) -> List (Html.Attribute msg)
hiddenIfIgnored path state attributes =
    if isIgnored path state then
        (class "VaultDialog-FolderItem-Hidden") :: attributes
    else
        attributes


inFolderPath : Path -> List (Html Msg) -> List (Html Msg)
inFolderPath path contents =
    case path of
        [] ->
            []

        [ p ] ->
            contents

        x :: rest ->
            [ div [ class "VaultDialog-FolderItem-Nested" ]
                (inFolderPath rest contents)
            ]


folderCollapseToggle : Path -> State -> Html Msg
folderCollapseToggle path state =
    if isExpanded path state then
        button
            [ onClick (CollapseFolder path)
            , class "VaultDialog-FolderItem-Collapse-Toggle"
            ]
            [ text "-" ]
    else
        button
            [ onClick (ExpandFolder path)
            , classList
                [ ( "VaultDialog-FolderItem-Collapse-Toggle", True )
                , ( "Hidden", isIgnored path state || folderIsEmpty path state )
                ]
            ]
            [ text "+" ]


fileCheckbox : Path -> State -> Html Msg
fileCheckbox path state =
    let
        fileCheckboxSettings =
            { disabled = False
            , readonly = False
            , value = not (isIgnored path state)
            , uid = Path.name path
            }

        checkbox =
            Ui.Checkbox.view fileCheckboxSettings
                |> Html.map (FileCheckBox path)

        checkboxWithLabel =
            labeledItem Right
                []
                (Just (ToggleIgnorePath path))
                (text (Path.folderName path))
                checkbox
    in
        span [ class "VaultDialog-Checkbox" ]
            [ checkboxWithLabel ]


userKeySelection : State -> Model -> Html Msg
userKeySelection state model =
    let
        email =
            userInputEmail state

        keys =
            userKeys email state

        isHidden =
            keys == NotAsked
    in
        div
            [ class "VaultDialog-UserKeys"
            ]
        <|
            (loadingSpinnerIf <| keys == Loading)
                :: List.map (\key -> userKeyCheckbox email key state model)
                    (RemoteData.withDefault [] keys)


userKeyCheckbox : String -> UserKey -> State -> Model -> Html Msg
userKeyCheckbox email userKey state model =
    let
        isDisabled =
            isUserKeyAlreadyAdded userKey state

        checkboxViewSettings =
            { disabled = isDisabled
            , readonly = False
            , value = isUserKeySelected email userKey state
            , uid = userKey.fingerprint
            }

        labelMsg =
            if isDisabled then
                Nothing
            else
                Just (ToggleUserKey email userKey)

        checkbox =
            Ui.Checkbox.view checkboxViewSettings
                |> Html.map (UserKeyCheckbox email userKey)

        checkboxWithLabel =
            labeledItem Right
                []
                labelMsg
                (text (userKey.fingerprint ++ " - " ++ userKey.description))
                checkbox
    in
        div [ class "VaultDialog-SelectKey", animation 0.5 Highlight ]
            [ span [ class "VaultDialog-Checkbox" ] [ checkboxWithLabel ]
            , keyCreatedTimestamp userKey model
            ]


userList : State -> Model -> Html Msg
userList state model =
    div [ class "Vault-Dialog-UserList" ] <|
        (h4 [] [ text "Vault Users:" ])
            :: case state.users of
                Success users ->
                    List.map (\u -> userItem u state model) users

                Loading ->
                    [ loadingSpinner ]

                _ ->
                    []


pendingUserList : State -> Html Msg
pendingUserList state =
    let
        hasPendingKeys email keys =
            keys
                |> List.isEmpty
                |> not

        pendingUsers =
            state.usersToAdd
                |> Dict.filter hasPendingKeys
                |> Dict.toList
    in
        div [ class "VaultDialog-PendingUserList" ] <|
            if List.isEmpty pendingUsers then
                []
            else
                (h4 [] [ text "Pending Users:" ])
                    :: (List.map (\( email, keys ) -> pendingUserItem email keys) pendingUsers)


userItem : User -> State -> Model -> Html Msg
userItem user state model =
    div
        [ classList
            [ ( "VaultDialog-User", True )
            , ( "Normal-Cursor", not (isOwner state.id model) )
            ]
        , onClick (SetUserInput user.email)
        ]
        [ span [ class "VaultDialog-User-Name" ]
            [ text <| user.firstName ++ " " ++ user.lastName ]
        , span [ class "VaultDialog-User-Email" ]
            [ text <| " ( " ++ user.email ++ " )" ]
        , userAddedTimestamp user model
        ]


pendingUserItem : Email -> List UserKey -> Html Msg
pendingUserItem email keys =
    div [ class "VaultDialog-User", onClick (SetUserInput email) ]
        [ span [ class "VaultDialog-User-Email" ]
            [ text email ]
        , span [ class "VaultDialog-UserKeyFingerprints" ]
            [ text
                (keys
                    |> List.map (\key -> key.fingerprint)
                    |> String.join ", "
                )
            ]
        ]


userAddedTimestamp : User -> Model -> Html msg
userAddedTimestamp user model =
    span [ class "VaultDialog-UserAddedTime" ] <|
        case ( user.accessGrantedAt, model.now ) of
            ( Nothing, _ ) ->
                [ text "Vault Owner" ]

            ( Just date, Nothing ) ->
                [ text <| toString date ]

            ( Just date, Just now ) ->
                [ text <| "Invited " ++ (Date.Distance.inWords date (Date.fromTime now)) ++ " ago" ]


keyCreatedTimestamp : UserKey -> Model -> Html msg
keyCreatedTimestamp key model =
    span [ class "VaultDialog-UserKeyCreatedTime" ] <|
        case ( key.createdAt, model.now ) of
            ( Nothing, _ ) ->
                []

            ( Just date, Nothing ) ->
                [ text <| toString date ]

            ( Just date, Just now ) ->
                [ text <| "Created " ++ (Date.Distance.inWords date (Date.fromTime now)) ++ " ago" ]
