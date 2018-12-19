module Tooltips exposing (copied)

import Data.Vault exposing (VaultId)
import Html
import Html.Events exposing (onClick)
import Model
import Tooltip exposing (Tooltip)
import Translation as T
import Util exposing (Position(..), TooltipLength(..))


copied : { id : String, position : Position } -> Tooltip T.Text
copied opts =
    Tooltip.init ("copied." ++ opts.id)
        { text = T.CopiedToClipboard
        , visibleTime = 750
        , position = opts.position
        , length = Auto
        }
