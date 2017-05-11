module VaultDialog exposing (..)

import ConfirmationDialog
import Date.Distance
import Dialog exposing (labeledLeft, labeledRight)
import Dict
import Html exposing (Html, button, div, form, h4, input, label, span, text)
import Html.Attributes exposing (class, classList, for, id, style)
import Html.Events exposing (onClick)
import Model exposing (Model)
import Path exposing (Path)
import Syncrypt.User exposing (Email, User, UserKey)
import Syncrypt.Vault exposing (Vault, VaultId)
import Ui.Button
import Ui.Checkbox
import Ui.Container
import Ui.Input
import Ui.Modal
import Ui.Tabs
import Util exposing (onAnyKeyDown, onEnter)
import VaultDialog.Model
    exposing
        ( FileName
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
import VaultDialog.Update exposing (dialogState)


subscriptions : Model -> Sub Model.Msg
subscriptions _ =
    let
        fileListMsg ( vaultId, rootPath, folderItem ) =
            Model.VaultDialog vaultId (NestedFileList rootPath folderItem)

        selectedFolderMsg ( vaultId, path ) =
            Model.VaultDialog vaultId (SelectedFolder path)
    in
        Sub.batch
            [ VaultDialog.Ports.fileList fileListMsg
            , VaultDialog.Ports.selectedFolder selectedFolderMsg
            ]


viewAll : Model -> List (Html Model.Msg)
viewAll ({ vaultDialogs } as model) =
    vaultDialogs
        |> Dict.keys
        |> List.map (\vaultId -> view vaultId model)


view : VaultId -> Model -> Html Model.Msg
view vaultId model =
    let
        viewConfig =
            { address = (Model.VaultDialog vaultId << Modal)
            , contents = contents vaultId model
            , footer = []
            , title =
                case vaultId of
                    "" ->
                        "Create New Vault"

                    id ->
                        "Vault " ++ id
            }

        state =
            dialogState vaultId model
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
    in
        [ Ui.Tabs.view tabsViewConfig state.tabs
        , ConfirmationDialog.view state
            |> Html.map (Model.VaultDialog state.id)
        , div [ class "VaultDialog-Buttons" ]
            [ deleteButton vaultId state
            , removeButton vaultId state
            , saveButton vaultId state
            , cancelButton vaultId state
            ]
        ]


tabContents : VaultId -> State -> Model -> List ( String, Html Model.Msg )
tabContents vaultId state model =
    let
        -- converter from Html Msg -> Html Model.Msg
        msg =
            Html.map (Model.VaultDialog vaultId)

        searchKeys =
            Model.VaultDialog vaultId <|
                SearchUserKeys (userInputEmail state)
    in
        [ ( "Basic"
          , div [ class "VaultDialog-Tab-Content" ]
                [ dialogInput <| nameInput vaultId state
                , dialogInput <| msg <| openFolderButton vaultId state model
                , dialogInput <| msg <| fileSelectionContainer state
                ]
          )
        , ( "Users"
          , div [ class "VaultDialog-Tab-Content" ]
                [ div []
                    [ div
                        [ class "VaultDialog-Add-User", onEnter searchKeys ]
                        [ dialogInput <| userInput vaultId state
                        ]
                    , div [ class "VaultDialog-UserKey-Selection" ]
                        [ msg <| userKeySelection state model
                        , msg <| confirmUserKeysButton state
                        ]
                    ]
                , h4 []
                    [ text "Vault Users:" ]
                , msg <| userList state model
                , msg <| pendingUserList state
                ]
          )
        ]


dialogInput : Html msg -> Html msg
dialogInput body =
    div [ class "VaultDialog-Input" ]
        [ body ]


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


deleteButton : VaultId -> State -> Html Model.Msg
deleteButton vaultId state =
    span
        [ classList
            [ ( "Hidden", state.isNew )
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
            [ ( "Hidden", state.isNew )
            , ( "VaultDialog-Button-Remove", True )
            ]
        ]
        [ Ui.Button.model "Stop Syncing" "warning" "small"
            |> Ui.Button.view (Model.VaultDialog vaultId (Confirm RemoveVault))
        ]


saveButton : VaultId -> State -> Html Model.Msg
saveButton vaultId state =
    let
        ( label, msg ) =
            case ( state.isNew, state.hasChangesPending ) of
                ( True, True ) ->
                    ( "Create", Model.SaveVaultDetails vaultId )

                ( False, True ) ->
                    ( "Save", Model.SaveVaultDetails vaultId )

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

        ( folderPath, msg ) =
            case ( state.isNew, state.localFolderPath ) of
                ( _, Nothing ) ->
                    ( "Select Folder"
                    , OpenFolderDialog vaultId
                    )

                ( True, Just path ) ->
                    ( pathString path
                    , OpenFolderDialog vaultId
                    )

                ( False, Just path ) ->
                    let
                        ps =
                            pathString path
                    in
                        ( ps
                        , OpenFolder ps
                        )
    in
        span [ class "VaultDialog-Button-Folder" ]
            [ Ui.Button.model folderPath "primary" "medium"
                |> Ui.Button.view msg
            ]


nameInput : VaultId -> State -> Html Model.Msg
nameInput vaultId state =
    span [ onAnyKeyDown (Model.VaultDialog vaultId NameChanged) ]
        [ Ui.Input.view state.nameInput
            |> Html.map (Model.VaultDialog vaultId << NameInput)
            |> labeledLeft [ class "VaultDialog-InputLabel" ]
                (Just (Model.FocusOn state.nameInput.uid))
                "Name"
        ]


userInput : VaultId -> State -> Html Model.Msg
userInput vaultId state =
    Ui.Input.view state.userInput
        |> Html.map (Model.VaultDialog vaultId << UserInput)
        |> labeledLeft [ class "VaultDialog-InputLabel" ]
            (Just (Model.FocusOn state.userInput.uid))
            "Invite User"


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
                [ Ui.Container.view settings [] (renderFolders state)
                    |> labeledLeft [ class "VaultDialog-InputLabel" ]
                        Nothing
                        "Files"
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
                [ div []
                    (List.foldr (::)
                        []
                        (rootFiles ++ rootFolders)
                    )
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
            checkbox
                |> labeledRight []
                    (Just (ToggleIgnorePath path))
                    (Path.folderName path)
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
            List.isEmpty keys
    in
        div
            [ classList
                [ ( "VaultDialog-UserKeys", not isHidden )
                , ( "VaultDialog-UserKeys-Hidden", isHidden )
                ]
            ]
        <|
            (h4 [] [ text "Select keys" ])
                :: List.map (\key -> userKeyCheckbox email key state model) keys


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
            checkbox
                |> labeledRight []
                    labelMsg
                    (userKey.fingerprint ++ " - " ++ userKey.description)
    in
        div []
            [ span [ class "VaultDialog-Checkbox" ] [ checkboxWithLabel ]
            , keyCreatedTimestamp userKey model
            ]


userList : State -> Model -> Html Msg
userList state model =
    div [ class "VaultDialog-UserList" ]
        (List.map (\u -> userItem u model) state.users)


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


userItem : User -> Model -> Html Msg
userItem user model =
    div [ class "VaultDialog-User" ]
        [ span [ class "VaultDialog-User-Name" ]
            [ text <| user.firstName ++ " " ++ user.lastName ]
        , span [ class "VaultDialog-User-Email", onClick (SetUserInput user.email) ]
            [ text <| " ( " ++ user.email ++ " )" ]
        , userAddedTimestamp user model
        ]


pendingUserItem : Email -> List UserKey -> Html Msg
pendingUserItem email keys =
    div [ class "VaultDialog-PendingUser" ]
        [ span [ class "VaultDialog-User-Email", onClick (SetUserInput email) ]
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
                []

            ( Just date, Nothing ) ->
                [ text <| toString date ]

            ( Just date, Just now ) ->
                [ text <| "Invited " ++ (Date.Distance.inWords date now) ++ " ago" ]


keyCreatedTimestamp : UserKey -> Model -> Html msg
keyCreatedTimestamp key model =
    span [ class "VaultDialog-UserKeyCreatedTime" ] <|
        case ( key.createdAt, model.now ) of
            ( Nothing, _ ) ->
                []

            ( Just date, Nothing ) ->
                [ text <| toString date ]

            ( Just date, Just now ) ->
                [ text <| "Created " ++ (Date.Distance.inWords date now) ++ " ago" ]
