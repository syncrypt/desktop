port module Ports
    exposing
        ( addEmailToCompletionList
        , focusOn
        , getEmailCompletionList
        , openPasswordResetInBrowser
        , openUserKeyExportFileDialog
        , openVaultFolder
        , selectedUserKeyExportFile
        , updateEmailCompletionList
        )


port focusOn : String -> Cmd msg


port openVaultFolder : String -> Cmd msg


port addEmailToCompletionList : String -> Cmd msg


port updateEmailCompletionList : () -> Cmd msg


port getEmailCompletionList : (List String -> msg) -> Sub msg


port openPasswordResetInBrowser : () -> Cmd msg


port openUserKeyExportFileDialog : String -> Cmd msg


port selectedUserKeyExportFile : (String -> msg) -> Sub msg
