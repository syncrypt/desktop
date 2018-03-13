port module Ports
    exposing
        ( addEmailToCompletionList
        , focusOn
        , getEmailCompletionList
        , openPasswordResetInBrowser
        , openVaultFolder
        , updateEmailCompletionList
        )


port focusOn : String -> Cmd msg


port openVaultFolder : String -> Cmd msg


port addEmailToCompletionList : String -> Cmd msg


port updateEmailCompletionList : () -> Cmd msg


port getEmailCompletionList : (List String -> msg) -> Sub msg


port openPasswordResetInBrowser : () -> Cmd msg
