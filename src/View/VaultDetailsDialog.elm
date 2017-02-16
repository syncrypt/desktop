module View.VaultDetailsDialog exposing (..)

import Html exposing (Html, button, div, h1, node, text)
import Html.Attributes exposing (..)
import Html.CssHelpers
import Html.Events exposing (onClick)
import Model exposing (..)
import Syncrypt.Vault exposing (Vault)


-- import View.Css.VaultDetailsDialog exposing (..)


view : vault -> Model -> Html msg
view vault model =
    text "ok"
