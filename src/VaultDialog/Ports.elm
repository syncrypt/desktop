port module VaultDialog.Ports exposing (..)

import VaultDialog.Model exposing (FolderItem, Path)


port openFolder : () -> Cmd msg


port selectedFolder : (Path -> msg) -> Sub msg


port getFileList : Path -> Cmd msg


port fileList : (( Path, FolderItem ) -> msg) -> Sub msg
