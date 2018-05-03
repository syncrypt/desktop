port module Ports
    exposing
        ( addEmailToCompletionList
        , focusOn
        , getEmailCompletionList
        , openPasswordResetInBrowser
        , openUserKeyExportFileDialog
        , openVaultFolder
        , openVaultImportFolderDialog
        , openVaultKeyImportFileDialog
        , quitAndInstall
        , selectedUserKeyExportFile
        , selectedVaultImportFolder
        , selectedVaultKeyImportFile
        , updateAvailable
        , updateEmailCompletionList
        )

import Path exposing (Path)


port focusOn : String -> Cmd msg


port openVaultFolder : String -> Cmd msg


port addEmailToCompletionList : String -> Cmd msg


port updateEmailCompletionList : () -> Cmd msg


port getEmailCompletionList : (List String -> msg) -> Sub msg


port openPasswordResetInBrowser : () -> Cmd msg


port openUserKeyExportFileDialog : String -> Cmd msg


port selectedUserKeyExportFile : (String -> msg) -> Sub msg


port quitAndInstall : () -> Cmd msg


port updateAvailable : (String -> msg) -> Sub msg


port openVaultKeyImportFileDialog : () -> Cmd msg


port selectedVaultKeyImportFile : (Path -> msg) -> Sub msg


port openVaultImportFolderDialog : () -> Cmd msg


port selectedVaultImportFolder : (Path -> msg) -> Sub msg
