port module Ports
    exposing
        ( addEmailToCompletionList
        , focusOn
        , getEmailCompletionList
        , log
        , openPasswordResetInBrowser
        , openUserKeyExportFileDialog
        , openVaultFolder
        , quitAndInstall
        , selectedUserKeyExportFile
        , updateAvailable
        , updateEmailCompletionList
        )

import Json.Encode as Json
import Util exposing (LogLevel)


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


port logWithData : Json.Value -> Cmd msg


log : LogLevel -> String -> Maybe logData -> Cmd msg
log level infoText data =
    logWithData <|
        Json.object
            [ ( "level", Json.string <| toString level )
            , ( "text", Json.string infoText )
            , ( "data"
              , data
                    |> Maybe.map (toString >> Json.string)
                    |> Maybe.withDefault Json.null
              )
            ]
