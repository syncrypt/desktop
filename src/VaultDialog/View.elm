module VaultDialog.View exposing (..)

import Animation exposing (..)
import ConfirmationDialog
import Daemon
import Data.User as User exposing (Email, User, UserKey)
import Data.Vault
    exposing
        ( Event(..)
        , HistoryItem
        , LogLevel(..)
        , Vault
        , VaultId
        )
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
import Language exposing (HasLanguage)
import Model exposing (Model)
import Path exposing (Path)
import RemoteData exposing (RemoteData(..))
import Translation
    exposing
        ( FolderButtonType(..)
        , Text(..)
        , VaultDialogText(..)
        , t
        , timeAgo
        )
import Ui.Button
import Ui.Checkbox
import Ui.Container
import Ui.Input
import Ui.Modal
import Ui.Tabs
import Util
    exposing
        ( Position(..)
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
        , hasFiles
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
                    let
                        msg =
                            Model.VaultDialogMsg vault.id << VaultLogStream
                    in
                        model.config
                            |> Daemon.subscribeVaultLogStream vault.id msg

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

        basicTabs =
            if isOwner vaultId model then
                [ filesTab msg vaultId state model
                , usersTab msg vaultId state model
                ]
            else
                [ filesTab msg vaultId state model ]

        clonedTabs =
            [ cryptoTab vaultId state model
            , logTab vaultId state model
            , adminTab vaultId state model
            ]

        notClonedTabs =
            [ cryptoTab vaultId state model
            , adminTab vaultId state model
            ]

        tabs =
            case state.cloneStatus of
                New ->
                    basicTabs

                Cloned ->
                    basicTabs ++ clonedTabs

                NotCloned ->
                    basicTabs ++ notClonedTabs
    in
        tabs


usersTab : (Msg -> Model.Msg) -> VaultId -> State -> Model -> ( String, Html Model.Msg )
usersTab toRootMsg vaultId state model =
    let
        -- converter from Html Msg -> Html Model.Msg
        rootMsg =
            Html.map toRootMsg

        searchKeys =
            Model.VaultDialogMsg vaultId <|
                SearchUserKeys (userInputEmail state)

        ownsVault =
            isOwner vaultId model

        infoText =
            t (VaultDialogText (UsersTabInfoText ownsVault)) model
    in
        ( t (VaultDialogText UsersTab) model
        , div []
            [ tabInfoText infoText
            , div
                [ classList [ ( "Hidden", not ownsVault ) ] ]
                [ div
                    [ class "Add-User", onEnter searchKeys ]
                    [ dialogInput "User"
                        [ userInput vaultId state model ]
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


cryptoTab : VaultId -> State -> Model -> ( String, Html Model.Msg )
cryptoTab vaultId state model =
    let
        vault =
            Model.vaultWithId vaultId model

        cryptoInfoItem label tooltip value =
            div [ class "CryptoInfoItem" ]
                [ labeledItem Left
                    [ class "InputLabel CryptoInfoItem-Label" ]
                    Nothing
                    (text label)
                    (tooltipItem
                        { position = Bottom
                        , length = Auto
                        , text = tooltip
                        }
                        [ text value ]
                    )
                ]

        vt vaultDialogText =
            t (VaultDialogText vaultDialogText) model
    in
        ( t (VaultDialogText CryptoTab) model
        , div []
            [ tabInfoText (vt CryptoTabInfoText)
            , cryptoInfoItem (vt VaultIdLabel)
                (vt VaultIdTooltip)
                (String.toUpper vault.id)
            , cryptoInfoItem (vt FileRevisionsLabel)
                (vt TotalNumberOfFileRevisionsTooltip)
                (toString vault.revisionCount)
            , cryptoInfoItem (vt LastModifiedLabel)
                (vt LastModifiedTooltip)
                (vault.modificationDate
                    |> Maybe.map (\date -> timeAgo date model)
                    |> Maybe.withDefault (vt NoChangesSoFar)
                )
            , cryptoInfoItem (vt KeyAlgorithmLabel)
                (vt KeyAlgorithmTooltip)
                (String.toUpper vault.crypto.keyAlgorithm)
            , cryptoInfoItem (vt KeyFingerprintLabel)
                (vt KeyFingerprintTooltip)
                (vault.crypto.fingerprint
                    |> Maybe.map String.toUpper
                    |> Maybe.withDefault "N/A"
                )
            , cryptoInfoItem (vt TransferAlgorithmLabel)
                (vt TransferAlgorithmTooltip)
                (String.toUpper vault.crypto.transferAlgorithm)
            , cryptoInfoItem (vt HashAlgorithmLabel)
                (vt HashAlgorithmTooltip)
                (String.toUpper vault.crypto.hashAlgorithm)
            , cryptoInfoItem (vt AESKeyLengthLabel)
                (vt AESKeyLengthTooltip)
                (toString vault.crypto.aesKeyLength)
            , cryptoInfoItem (vt RSAKeyLengthLabel)
                (vt RSAKeyLengthTooltip)
                (toString vault.crypto.rsaKeyLength)
            , separator
            ]
        )


filesTab : (Msg -> Model.Msg) -> VaultId -> State -> Model -> ( String, Html Model.Msg )
filesTab toRootMsg vaultId state model =
    ( t (VaultDialogText NameAndFilesTab) model
    , div []
        [ dialogInput "Name"
            [ nameInput toRootMsg state model ]
        , dialogInput "Folder"
            [ Html.map toRootMsg <| openFolderButton vaultId state model ]
        , dialogInput "FileSelection"
            [ Html.map toRootMsg <| fileSelectionContainer state model ]
        ]
    )


logTab : VaultId -> State -> Model -> ( String, Html Model.Msg )
logTab vaultId state model =
    ( t (VaultDialogText LogTab) model
    , div [] <|
        [ div [ class "EventFilters" ] <|
            [ Dialog.labeledItem Left
                [ class "InputLabel" ]
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


adminTab : VaultId -> State -> Model -> ( String, Html Model.Msg )
adminTab vaultId state model =
    let
        adminActions =
            if state.cloneStatus == New || not (isOwner vaultId model) then
                []
            else
                [ infoText (t (VaultDialogText VaultDeleteButtonInfo) model)
                , deleteButton vaultId state model
                ]
    in
        ( t (VaultDialogText AdminTab) model
        , div [ class "Admin-Buttons" ] <|
            [ infoText (t (VaultDialogText VaultRemoveButtonInfo) model)
            , removeButton vaultId state
            , separator
            , infoText (t (VaultDialogText VaultExportButtonInfo) model)
            , exportButton <| Model.vaultWithId vaultId model
            , separator
            ]
                ++ adminActions
        )


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


viewLogItem : Maybe Date -> Data.Vault.LogItem -> Html msg
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
    span []
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
                    ( t (VaultDialogText <| FolderButtonLabel CloneIntoFolder)
                        model
                    , OpenFolderDialog
                    , t (VaultDialogText <| FolderButtonTooltip CloneIntoFolder)
                        model
                    )

                ( _, Nothing ) ->
                    ( t (VaultDialogText <| FolderButtonLabel SelectFolder)
                        model
                    , OpenFolderDialog
                    , t (VaultDialogText <| FolderButtonTooltip SelectFolder)
                        model
                    )

                ( New, Just path ) ->
                    let
                        ps =
                            pathString path
                    in
                        ( t
                            (VaultDialogText <|
                                FolderButtonLabel <|
                                    FolderSelectedForSync ps
                            )
                            model
                        , OpenFolderDialog
                        , t
                            (VaultDialogText <|
                                FolderButtonTooltip <|
                                    FolderSelectedForSync ps
                            )
                            model
                        )

                ( _, Just path ) ->
                    let
                        ps =
                            pathString path
                    in
                        ( t
                            (VaultDialogText <|
                                FolderButtonLabel <|
                                    SyncedFolder ps
                            )
                            model
                        , OpenFolder ps
                        , t
                            (VaultDialogText <|
                                FolderButtonTooltip <|
                                    SyncedFolder ps
                            )
                            model
                        )
    in
        span [ class "Button-Folder" ]
            [ labeledItem Left
                [ class "InputLabel" ]
                Nothing
                (text "Folder")
                (tooltipItem
                    { position = Bottom
                    , length = Auto
                    , text = tooltipMsg
                    }
                    [ Ui.Button.model folderPath "primary" "small"
                        |> Ui.Button.view msg
                    ]
                )
            ]


nameInput : (Msg -> Model.Msg) -> State -> HasLanguage a -> Html Model.Msg
nameInput msg state model =
    span [ onAnyKeyDown (msg NameChanged) ]
        [ labeledItem Left
            [ class "InputLabel" ]
            (Just (Model.FocusOn state.nameInput.uid))
            (text "Name")
            (tooltipItem
                { position = Bottom
                , length = Auto
                , text = t (VaultDialogText VaultNameTooltip) model
                }
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
        tooltipItem
            { position = Right
            , length = Auto
            , text = "Vault icon that can be seen by any invited user"
            }
            [ icon ]


userInput : VaultId -> State -> HasLanguage a -> Html Model.Msg
userInput vaultId state model =
    labeledItem Top
        [ class "InputLabel" ]
        (Just (Model.FocusOn state.userInput.uid))
        (text <| t (VaultDialogText UserInputLabel) model)
        (tooltipItem
            { position = Bottom
            , length = Auto
            , text = t (VaultDialogText UserInputTooltip) model
            }
            [ Ui.Input.view
                state.userInput
                |> Html.map (Model.VaultDialogMsg vaultId << UserInputMsg)
            ]
        )


fileSelectionContainer : State -> HasLanguage a -> Html Msg
fileSelectionContainer state model =
    let
        settings =
            { direction = "column"
            , compact = False
            , align = "start"
            }

        body =
            if hasFiles state then
                [ labeledItem Left
                    [ class "InputLabel" ]
                    Nothing
                    (text <| t (VaultDialogText FilesLabel) model)
                    (Ui.Container.view settings
                        []
                        [ (tooltipItem
                            { position = Top
                            , length = XLarge
                            , text =
                                t (VaultDialogText FileSelectionTooltip) model
                            }
                            (renderFolders state)
                          )
                        ]
                    )
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
                (rootFiles ++ rootFolders)

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


hiddenIfIgnored :
    Path
    -> State
    -> List (Html.Attribute msg)
    -> List (Html.Attribute msg)
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
            [ text "v" ]
    else
        button
            [ onClick (ExpandFolder path)
            , classList
                [ ( "FolderItem-Collapse-Toggle", True )
                , ( "Hidden", isIgnored path state || folderIsEmpty path state )
                ]
            ]
            [ text ">" ]


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
    div [ class "UserList" ] <|
        (h4 [] [ text <| t (VaultDialogText VaultUsersLabel) model ])
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

        pendingUserItems =
            pendingUsers
                |> List.map (\( email, keys ) -> pendingUserItem email keys)
    in
        div [ class "PendingUserList" ] <|
            if List.isEmpty pendingUsers then
                []
            else
                (h4 [] [ text "Pending Users:" ]) :: pendingUserItems


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
                [ text <|
                    "Created "
                        ++ Date.Distance.inWords date now
                        ++ " ago"
                ]
