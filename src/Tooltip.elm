module Tooltip
    exposing
        ( HasTooltips
        , Tooltip
        , Tooltips
        , add
        , copied
        , emptyTooltips
        , isActive
        , remove
        , removeIn
        , view
        , viewIfActive
        , viewWithId
        )

import Dict exposing (Dict)
import Html exposing (Html)
import Language exposing (HasLanguage, Language)
import Time exposing (Time)
import Translation as T
import Util exposing (Position(..), TooltipLength(..))


type alias Tooltip =
    { id : String
    , text : T.Text
    , active : Bool
    , visibleTime : Time
    , position : Position
    , length : TooltipLength
    }


type alias Tooltips =
    Dict String Tooltip


type alias HasTooltips a =
    { a
        | tooltips : Tooltips
        , language : Language
    }


emptyTooltips : Tooltips
emptyTooltips =
    Dict.empty


removeIn : Time -> String -> (String -> msg) -> Cmd msg
removeIn time id fn =
    Util.delayMsg time (fn id)


add : Tooltip -> HasTooltips a -> HasTooltips a
add tip model =
    { model | tooltips = Dict.insert tip.id tip model.tooltips }


remove : String -> HasTooltips a -> HasTooltips a
remove id model =
    { model | tooltips = Dict.remove id model.tooltips }


isActive : String -> HasTooltips a -> Bool
isActive id { tooltips } =
    tooltips
        |> Dict.get id
        |> Maybe.map .active
        |> Maybe.withDefault False


view : Tooltip -> HasTooltips a -> List (Html msg) -> Html msg
view { id, text, active, position, length } model body =
    Util.tooltipItem
        { position = position
        , length = length
        , text = T.t text model
        }
        body


viewIfActive : Tooltip -> HasTooltips a -> List (Html msg) -> Html msg
viewIfActive tip model body =
    viewWithId tip.id model body


viewWithId : String -> HasTooltips a -> List (Html msg) -> Html msg
viewWithId id model body =
    case Dict.get id model.tooltips of
        Nothing ->
            Html.span [] body

        Just tip ->
            view tip model body


copied : { id : String, position : Position } -> Tooltip
copied { id, position } =
    { id = "copied." ++ id
    , text = T.CopiedToClipboard
    , active = True
    , visibleTime = 750
    , position = position
    , length = Auto
    }
