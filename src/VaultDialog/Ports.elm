port module VaultDialog.Ports exposing (..)

import VaultDialog.Model exposing (FolderItem)
import Path exposing (Path)
import Syncrypt.Vault exposing (VaultId)


port openFolder : VaultId -> Cmd msg


port selectedFolder : (( VaultId, Path ) -> msg) -> Sub msg


port getFileList : ( VaultId, Path ) -> Cmd msg


port fileList : (( VaultId, Path, FolderItem ) -> msg) -> Sub msg
