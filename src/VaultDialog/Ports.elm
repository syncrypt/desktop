port module VaultDialog.Ports exposing (..)

import VaultDialog.Model exposing (FolderItem)
import Path exposing (Path)
import Data.Vault exposing (VaultId)


port openFolderDialog : VaultId -> Cmd msg


port selectedFolder : (( VaultId, Path ) -> msg) -> Sub msg


port getFileList : ( VaultId, Path ) -> Cmd msg


port fileList : (( VaultId, Path, FolderItem ) -> msg) -> Sub msg


port openIconFileDialog : String -> Cmd msg


port selectedIconFile : (( String, String ) -> msg) -> Sub msg


port openExportFileDialog : String -> Cmd msg


port selectedExportFile : (( String, String ) -> msg) -> Sub msg
