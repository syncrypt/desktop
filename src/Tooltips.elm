module Tooltips exposing (copied)

import Tooltip exposing (Tooltip, TooltipLength(..))
import Translation as T
import Util exposing (Position(..))


copied : { id : String, position : Position } -> Tooltip T.Text
copied opts =
    Tooltip.init ("copied." ++ opts.id)
        { text = T.CopiedToClipboard
        , visibleTime = 750
        , position = opts.position
        , length = Auto
        }
