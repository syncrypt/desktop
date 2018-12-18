module Tooltip
    exposing
        ( HasTooltips
        , ID
        , Tooltip
        , TooltipOptions
        , Tooltips
        , add
        , copied
        , emptyTooltips
        , id
        , isActive
        , length
        , position
        , remove
        , removeIn
        , removeInSchedule
        , text
        , view
        , viewIfActive
        , viewWithId
        , visibleTime
        )

import Dict exposing (Dict)
import Html exposing (Html)
import Language exposing (HasLanguage, Language)
import Time exposing (Time)
import Translation as T
import Util exposing (Position(..), TooltipLength(..))


type ID
    = ID String


type Tooltip
    = Tooltip
        { id : ID
        , text : T.Text
        , active : Bool
        , visibleTime : Time
        , position : Position
        , length : TooltipLength
        }


type alias TooltipOptions =
    { text : T.Text
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


initID : String -> ID
initID idString =
    -- add random UUID?
    ID idString


init : String -> TooltipOptions -> Tooltip
init idString { text, visibleTime, position, length } =
    Tooltip
        { id = initID idString
        , active = True
        , text = text
        , visibleTime = visibleTime
        , position = position
        , length = length
        }


id : Tooltip -> ID
id (Tooltip tip) =
    tip.id


text : Tooltip -> T.Text
text (Tooltip { text }) =
    text


visibleTime : Tooltip -> Time
visibleTime (Tooltip { visibleTime }) =
    visibleTime


position : Tooltip -> Position
position (Tooltip { position }) =
    position


length : Tooltip -> TooltipLength
length (Tooltip { length }) =
    length


emptyTooltips : Tooltips
emptyTooltips =
    Dict.empty


removeInSchedule : Tooltip -> (ID -> msg) -> Cmd msg
removeInSchedule ((Tooltip { visibleTime, id }) as tip) fn =
    removeIn visibleTime id fn


removeIn : Time -> ID -> (ID -> msg) -> Cmd msg
removeIn time id fn =
    Util.delayMsg time (fn id)


add : Tooltip -> HasTooltips a -> HasTooltips a
add tip model =
    { model | tooltips = Dict.insert (idStr tip) tip model.tooltips }


idStr : Tooltip -> String
idStr (Tooltip { id }) =
    case id of
        ID idString ->
            idString


remove : ID -> HasTooltips a -> HasTooltips a
remove (ID id) model =
    { model | tooltips = Dict.remove id model.tooltips }


isActive : ID -> HasTooltips a -> Bool
isActive (ID id) { tooltips } =
    tooltips
        |> Dict.get id
        |> Maybe.map (\(Tooltip { active }) -> active)
        |> Maybe.withDefault False


view : Tooltip -> HasTooltips a -> List (Html msg) -> Html msg
view (Tooltip { text, active, position, length }) model body =
    Util.tooltipItem
        { position = position
        , length = length
        , text = T.t text model
        }
        body


viewIfActive : Tooltip -> HasTooltips a -> List (Html msg) -> Html msg
viewIfActive (Tooltip { id }) model body =
    viewWithId id model body


viewWithId : ID -> HasTooltips a -> List (Html msg) -> Html msg
viewWithId (ID id) model body =
    let
        bodyOnly =
            Html.span [] body
    in
    case Dict.get id model.tooltips of
        Nothing ->
            bodyOnly

        Just ((Tooltip { active }) as tip) ->
            if active then
                view tip model body
            else
                bodyOnly


copied : { id : String, position : Position } -> Tooltip
copied opts =
    init ("copied." ++ opts.id)
        { text = T.CopiedToClipboard
        , visibleTime = 750
        , position = opts.position
        , length = Auto
        }
