module VaultDialog exposing (..)

import Animation exposing (..)
import ConfirmationDialog
import Daemon
import Date exposing (Date)
import Date.Distance
import Dialog exposing (labeledItem)
import Dict
import Html
    exposing
        ( Html
        , button
        , div
        , form
        , h4
        , img
        , input
        , label
        , span
        , table
        , td
        , text
        , th
        , tr
        )
import Html.Attributes exposing (class, classList, for, id, src, style)
import Html.Events exposing (onClick)
import Model exposing (Model)
import Path exposing (Path)
import RemoteData exposing (RemoteData(..))
import Syncrypt.User as User exposing (Email, User, UserKey)
import Syncrypt.Vault
    exposing
        ( Event(..)
        , HistoryItem
        , LogLevel(..)
        , Vault
        , VaultId
        )
import Translation exposing (Text(..), t, timeAgo)
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
        , dateParts
        , fullDateString
        , monthNumber
        , onAnyKeyDown
        , onEnter
        , padNumber
        , shortDateString
        , tooltipItem
        )
import VaultDialog.Model
    exposing
        ( CloneStatus(..)
        , EventFilter(..)
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
subscriptions model =
    let
        fileListMsg ( vaultId, rootPath, folderItem ) =
            Model.VaultDialogMsg vaultId (NestedFileList rootPath folderItem)

        selectedFolderMsg ( vaultId, path ) =
            Model.VaultDialogMsg vaultId (SelectedFolder path)

        selectedExportFileMsg ( vaultId, path ) =
            Model.VaultDialogMsg vaultId (SelectedExportFile path)

        logStream =
            case model.state of
                Model.ShowingVaultDetails vault ->
                    model.config
                        |> Daemon.subscribeVaultLogStream vault.id (Model.VaultDialogMsg vault.id << VaultLogStream)

                _ ->
                    Sub.none
    in
        Sub.batch
            [ VaultDialog.Ports.fileList fileListMsg
            , VaultDialog.Ports.selectedFolder selectedFolderMsg
            , VaultDialog.Ports.selectedExportFile selectedExportFileMsg
            , logStream
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
            { address = (Model.VaultDialogMsg vaultId << ModalMsg)
            , contents = contents vaultId model
            , footer =
                [ span [ class "Buttons" ]
                    [ saveButton vaultId state
                    , cancelButton vaultId state
                    ]
                ]
            , title =
                case state.cloneStatus of
                    New ->
                        t CreateNewVault model

                    Cloned ->
                        state.title

                    NotCloned ->
                        t (VaultNotSynced vaultId) model
            }
    in
        div [ class "VaultDialog" ]
            [ Ui.Modal.view viewConfig state.modal ]


contents : VaultId -> Model -> List (Html Model.Msg)
contents vaultId model =
    let
        state =
            dialogState vaultId model

        tabsViewConfig =
            { address = (Model.VaultDialogMsg vaultId << TabsMsg)
            , contents = tabContents vaultId state model
            }

        tabs =
            Ui.Tabs.view tabsViewConfig state.tabs

        confirmationDialog =
            ConfirmationDialog.view state
                |> Html.map (Model.VaultDialogMsg state.id)
    in
        [ tabs
        , confirmationDialog
        ]


tabContents : VaultId -> State -> Model -> List ( String, Html Model.Msg )
tabContents vaultId state model =
    let
        msg =
            Model.VaultDialogMsg vaultId

        -- converter from Html Msg -> Html Model.Msg
        rootMsg =
            Html.map msg

        searchKeys =
            Model.VaultDialogMsg vaultId <|
                SearchUserKeys (userInputEmail state)

        filesTab =
            ( t NameAndFilesTab model
            , div []
                [ dialogInput "Name"
                    [ nameInput msg state ]
                , dialogInput "Folder"
                    [ rootMsg <| openFolderButton vaultId state model ]
                , dialogInput "FileSelection"
                    [ rootMsg <| fileSelectionContainer state ]
                ]
            )

        usersTab =
            let
                ownsVault =
                    isOwner vaultId model

                infoText =
                    if ownsVault then
                        "Add users to this vault to securely share access to files and collaborate on folders and files with as many people as you like."
                    else
                        "These users have access to this vault (including you). Anyone with access can add, edit and read files in this vault."
            in
                ( t UsersTab model
                , div []
                    [ tabInfoText infoText
                    , div
                        [ classList [ ( "Hidden", not ownsVault ) ] ]
                        [ div
                            [ class "Add-User", onEnter searchKeys ]
                            [ dialogInput "User"
                                [ userInput vaultId state ]
                            ]
                        , div [ class "UserKey-Selection" ]
                            [ rootMsg <| userKeySelection state model
                            , rootMsg <| confirmUserKeysButton state
                            ]
                        ]
                    , rootMsg <| userList state model
                    , rootMsg <| pendingUserList state
                    ]
                )

        cryptoTab =
            let
                vault =
                    Model.vaultWithId vaultId model

                cryptoInfoItem label tooltip value =
                    div [ class "CryptoInfoItem" ]
                        [ labeledItem Left
                            [ class "InputLabel CryptoInfoItem-Label" ]
                            Nothing
                            (text label)
                            (tooltipItem Bottom
                                Auto
                                tooltip
                                [ text value ]
                            )
                        ]
            in
                ( t CryptoTab model
                , div []
                    [ tabInfoText "Here you can see detailed information on this vault's cryptographic settings, used algorithms and keys."
                    , cryptoInfoItem "Vault ID"
                        "Syncrypt Vault ID"
                        (String.toUpper vault.id)
                    , cryptoInfoItem "File Revisions"
                        "Total number of file revisions in this vault."
                        (toString vault.revisionCount)
                    , cryptoInfoItem "Last modified"
                        "Date & time of last update to this vault."
                        (vault.modificationDate
                            |> Maybe.map (\date -> timeAgo date model)
                            |> Maybe.withDefault "No changes so far."
                        )
                    , cryptoInfoItem "Key Algorithm"
                        "Asymmetric key algorithm used for vault key"
                        (String.toUpper vault.crypto.keyAlgorithm)
                    , cryptoInfoItem "Vault Key Fingerprint"
                        "Vault public key fingerprint"
                        (vault.crypto.fingerprint
                            |> Maybe.map String.toUpper
                            |> Maybe.withDefault "N/A"
                        )
                    , cryptoInfoItem "Transfer Algorithm"
                        "Algorithm used for encrypting data transfer"
                        (String.toUpper vault.crypto.transferAlgorithm)
                    , cryptoInfoItem "Hash Algorithm"
                        "Algorithm used for hashing file contents & names"
                        (String.toUpper vault.crypto.hashAlgorithm)
                    , cryptoInfoItem "AES Key Length"
                        "Length of symmetric file encryption keys in this vault"
                        (toString vault.crypto.aesKeyLength)
                    , cryptoInfoItem "RSA Key Length"
                        "Length of vault private key"
                        (toString vault.crypto.rsaKeyLength)
                    , separator
                    ]
                )

        logTab =
            ( t LogTab model
            , div [] <|
                [ div [ class "EventFilters" ] <|
                    [ Dialog.labeledItem Left
                        []
                        Nothing
                        (text "Filters")
                        (span []
                            (eventFilterButtons vaultId state)
                        )
                    ]
                , div [ class "EventTableHeader" ]
                    [ table [ class "EventTable" ] <|
                        [ th
                            [ class "Default-Cursor"
                            , onClick (Model.VaultDialogMsg vaultId ToggleEventSortOrder)
                            ]
                            [ text "Time" ]
                          -- , th []
                          --     [ text "User" ]
                        , th []
                            [ text "Operation" ]
                        , th []
                            [ text "Path / Message" ]
                        ]
                    ]
                , div [ class "EventTableContent" ]
                    [ table [ class "EventTable" ] <|
                        (case VaultDialog.Model.events state of
                            [] ->
                                [ loadingSpinner ]

                            events ->
                                (List.map (viewEvent model.now) events)
                        )
                    ]
                ]
            )

        adminTab =
            ( t AdminTab model
            , div [ class "Admin-Buttons" ]
                [ infoText "Stop synchronizing this vault on this computer. Will stop all local changes from being uploaded and any remote changes being downloaded to this computer."
                , removeButton vaultId state
                , separator
                , infoText "Delete this vault with its files from the Syncrypt cloud."
                , deleteButton vaultId state model
                , separator
                , infoText "You can export your vault here to backup the vault's configuration, private metadata and encryption key. This allows you to re-download the vault in case of a disk failure or theft of the computer you're currently uploading files to this vault from."
                , exportButton <| Model.vaultWithId vaultId model
                ]
            )
    in
        [ filesTab, usersTab, cryptoTab, logTab, adminTab ]


eventFilterButtons : VaultId -> State -> List (Html Model.Msg)
eventFilterButtons vaultId state =
    let
        button attributes label msg =
            span attributes
                [ Ui.Button.model label "primary" "small"
                    |> Ui.Button.view msg
                ]

        rootMsg msg =
            Model.VaultDialogMsg vaultId msg

        filterMsg filter =
            rootMsg <| FilterEventsBy filter

        filterButton title filter =
            button
                [ classList
                    [ ( "Filter-Active"
                      , VaultDialog.Model.isFilterEnabled filter state
                      )
                    ]
                ]
                title
                (filterMsg <| filter)

        logLevelButtons =
            [ filterButton "Debug" <| Level Debug
            , filterButton "Info" <| Level Info
            , filterButton "Warning" <| Level Warning
            , filterButton "Error" <| Level Error
            ]

        logLevelButton =
            button [] "Log Level" (rootMsg <| ToggleViewLogLevelFilters)

        buttons =
            [ filterButton "History" <| IsHistoryItem
            , filterButton "Log" <| IsLogItem
            , logLevelButton
            , span [ class "LogLevelButtons" ] <|
                if state.viewLogLevelFilters then
                    logLevelButtons
                else
                    []
            ]
    in
        buttons


tabInfoText : String -> Html msg
tabInfoText infoText =
    div []
        [ div [ class "TabInfoText" ] [ text infoText ]
        , separator
        ]


viewEvent : Maybe Date -> Event -> Html msg
viewEvent now event =
    case event of
        Log item ->
            viewLogItem now item

        History item ->
            viewHistoryItem now item


type alias HasCreatedAt event =
    { event | createdAt : Maybe Date.Date }


eventDateString : Maybe Date -> HasCreatedAt a -> String
eventDateString now { createdAt } =
    case ( now, createdAt ) of
        ( Nothing, Just date ) ->
            fullDateString date

        ( Just nowDate, Just date ) ->
            let
                ( y1, m1, d1, _, _, _ ) =
                    dateParts nowDate

                ( y2, m2, d2, _, _, _ ) =
                    dateParts date
            in
                if y1 == y2 && m1 == m2 && d1 == d2 then
                    shortDateString date
                else
                    fullDateString date

        _ ->
            ""


viewLogItem : Maybe Date -> Syncrypt.Vault.LogItem -> Html msg
viewLogItem now item =
    tr [ class "HistoryItem" ]
        [ td []
            [ text <| eventDateString now item ]
          -- , td []
          --     []
        , td []
            [ text <| toString item.level ]
        , td []
            [ text item.message ]
        ]


viewHistoryItem : Maybe Date -> HistoryItem -> Html msg
viewHistoryItem now item =
    tr [ class "HistoryItem" ]
        [ td []
            [ text <| eventDateString now item ]
          -- , td []
          --     [ text item.email ]
        , td []
            [ text item.operation ]
        , td []
            [ text <| Util.shortenString 50 item.path ]
        ]


infoText : String -> Html msg
infoText infoText =
    div [ class "InfoText" ]
        [ text infoText ]


separator : Html msg
separator =
    Html.hr [ class "Separator" ] []


dialogInput : String -> List (Html msg) -> Html msg
dialogInput inputClassSuffix body =
    div
        [ class "Input"
        , class ("Input-" ++ inputClassSuffix)
        ]
        body


exportButton : Vault -> Html Model.Msg
exportButton vault =
    span
        [ class "Button-Export" ]
        [ Ui.Button.model "Export vault key bundle" "secondary" "small"
            |> Ui.Button.view (Model.VaultDialogMsg vault.id OpenExportDialog)
        ]


cancelButton : VaultId -> State -> Html Model.Msg
cancelButton vaultId state =
    span
        [ classList [ ( "Hidden", not state.hasChangesPending ) ] ]
        [ Ui.Button.model "Cancel Changes" "secondary" "small"
            |> Ui.Button.view (Model.CloseVaultDetails vaultId)
        ]


deleteButton : VaultId -> State -> Model -> Html Model.Msg
deleteButton vaultId state model =
    span
        [ classList
            [ ( "Hidden"
              , state.cloneStatus == New || not (isOwner vaultId model)
              )
            ]
        ]
        [ Ui.Button.model "Delete from Server" "danger" "small"
            |> Ui.Button.view (Model.VaultDialogMsg vaultId (Confirm DeleteVault))
        ]


removeButton : VaultId -> State -> Html Model.Msg
removeButton vaultId state =
    span
        [ classList [ ( "Hidden", state.cloneStatus /= Cloned ) ] ]
        [ Ui.Button.model "Stop syncing" "warning" "small"
            |> Ui.Button.view (Model.VaultDialogMsg vaultId (Confirm RemoveVault))
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
        Ui.Button.model label "primary" "small"
            |> Ui.Button.view msg


confirmUserKeysButton : State -> Html Msg
confirmUserKeysButton state =
    let
        email =
            userInputEmail state
    in
        if List.isEmpty (keysToAdd email state) then
            span [] []
        else
            Ui.Button.model "Invite with selected keys" "primary" "small"
                |> Ui.Button.view (Confirmed AddUser)


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
        span [ class "Button-Folder" ]
            [ labeledItem Left
                [ class "InputLabel" ]
                Nothing
                (text "Folder")
                (tooltipItem Bottom
                    Auto
                    tooltipMsg
                    [ Ui.Button.model folderPath "primary" "small"
                        |> Ui.Button.view msg
                    ]
                )
            ]


nameInput : (Msg -> Model.Msg) -> State -> Html Model.Msg
nameInput msg state =
    span [ onAnyKeyDown (msg NameChanged) ]
        [ labeledItem Left
            [ class "InputLabel" ]
            (Just (Model.FocusOn state.nameInput.uid))
            (text "Name")
            (tooltipItem Bottom
                Auto
                "The name of the vault. Chosen by the owner."
                [ Ui.Input.view state.nameInput
                    |> Html.map (msg << NameInputMsg)
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
                            [ class "Icon", onClick OpenIconDialog ]
                        else
                            [ class "Icon" ]
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
        [ class "InputLabel" ]
        (Just (Model.FocusOn state.userInput.uid))
        (text "Invite User")
        (tooltipItem Bottom
            Auto
            "Search for a user's email address to add them to this vault"
            [ Ui.Input.view
                state.userInput
                |> Html.map (Model.VaultDialogMsg vaultId << UserInputMsg)
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
                    [ class "InputLabel" ]
                    Nothing
                    (text "Files")
                    (Ui.Container.view settings [] (renderFolders state))
                ]
            else
                []
    in
        div [ class "FileSelection" ]
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
                    (rootFiles ++ rootFolders)
                ]

        [] ->
            []


renderFolder : State -> FolderItem -> Html Msg
renderFolder state ( path, files ) =
    if isExpanded path state then
        div [ class "FolderItem" ] <|
            (inFolderPath path
                [ span []
                    [ fileCheckbox path state
                    , folderCollapseToggle path state
                    ]
                , div (hiddenIfIgnored path state [])
                    [ div [ class "FolderItem-Nested" ]
                        (List.map (renderFile state path) files)
                    ]
                ]
            )
    else
        div [ class "FolderItem-Collapsed" ]
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
        div [ class "File" ]
            [ fileCheckbox filePath state ]


hiddenIfIgnored : Path -> State -> List (Html.Attribute msg) -> List (Html.Attribute msg)
hiddenIfIgnored path state attributes =
    if isIgnored path state then
        (class "FolderItem-Hidden") :: attributes
    else
        attributes


inFolderPath : Path -> List (Html Msg) -> List (Html Msg)
inFolderPath path contents =
    case path of
        [] ->
            []

        [ p ] ->
            contents

        _ :: rest ->
            [ div [ class "FolderItem-Nested" ]
                (inFolderPath rest contents)
            ]


folderCollapseToggle : Path -> State -> Html Msg
folderCollapseToggle path state =
    if isExpanded path state then
        button
            [ onClick (CollapseFolder path)
            , class "FolderItem-Collapse-Toggle"
            ]
            [ text "-" ]
    else
        button
            [ onClick (ExpandFolder path)
            , classList
                [ ( "FolderItem-Collapse-Toggle", True )
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
                |> Html.map (FileCheckBoxMsg path)

        checkboxWithLabel =
            labeledItem Right
                []
                (Just (ToggleIgnorePath path))
                (text (Path.folderName path))
                checkbox
    in
        span [ class "Checkbox" ]
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
        div [ class "UserKeys" ] <|
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
        div [ class "SelectKey", animation 0.5 Highlight ]
            [ span [ class "Checkbox" ] [ checkboxWithLabel ]
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
        div [ class "PendingUserList" ] <|
            if List.isEmpty pendingUsers then
                []
            else
                (h4 [] [ text "Pending Users:" ])
                    :: (List.map (\( email, keys ) -> pendingUserItem email keys) pendingUsers)


userItem : User -> State -> Model -> Html Msg
userItem user state model =
    div
        [ classList
            [ ( "User", True )
            , ( "Normal-Cursor", not (isOwner state.id model) )
            ]
        , onClick (SetUserInput user.email)
        ]
        [ span [ class "UserName" ]
            [ text <| User.fullName user ]
        , span [ class "UserEmail" ]
            [ text <| " ( " ++ user.email ++ " )" ]
        , userAddedTimestamp user model
        ]


pendingUserItem : Email -> List UserKey -> Html Msg
pendingUserItem email keys =
    div [ class "User", onClick (SetUserInput email) ]
        [ span [ class "UserEmail" ]
            [ text email ]
        , span [ class "UserKeyFingerprints" ]
            [ text
                (keys
                    |> List.map .fingerprint
                    |> String.join ", "
                )
            ]
        ]


userAddedTimestamp : User -> Model -> Html msg
userAddedTimestamp user model =
    span [ class "UserAddedTime" ]
        [ text
            (user.accessGrantedAt
                |> Maybe.map (\date -> "Invited " ++ (timeAgo date model))
                |> Maybe.withDefault "Vault Owner"
            )
        ]


keyCreatedTimestamp : UserKey -> Model -> Html msg
keyCreatedTimestamp key model =
    span [ class "UserKeyCreatedTime" ] <|
        case ( key.createdAt, model.now ) of
            ( Nothing, _ ) ->
                []

            ( Just date, Nothing ) ->
                [ text <| toString date ]

            ( Just date, Just now ) ->
                [ text <| "Created " ++ (Date.Distance.inWords date now) ++ " ago" ]
