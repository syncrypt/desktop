module Tooltip
    exposing
        ( HasTooltips
        , ID
        , Tooltip
        , TooltipLength(..)
        , TooltipOptions
        , Tooltips
        , activate
        , add
        , deactivate
        , emptyTooltips
        , id
        , init
        , isActive
        , item
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
import Html.Attributes exposing (attribute, class, style)
import Language exposing (HasLanguage, Language)
import Time exposing (Time)
import Util exposing (Position(..))


type ID
    = ID String


type Tooltip text
    = Tooltip
        { id : ID
        , text : text
        , active : Bool
        , visibleTime : Time
        , position : Position
        , length : TooltipLength
        }


type alias TooltipOptions text =
    { text : text
    , visibleTime : Time
    , position : Position
    , length : TooltipLength
    }


type alias Tooltips t =
    Dict String (Tooltip t)


type alias HasTooltips a t =
    { a
        | tooltips : Tooltips t
        , language : Language
    }


initID : String -> ID
initID idString =
    -- add random UUID?
    ID idString


init : String -> TooltipOptions t -> Tooltip t
init idString { text, visibleTime, position, length } =
    Tooltip
        { id = initID idString
        , active = True
        , text = text
        , visibleTime = visibleTime
        , position = position
        , length = length
        }


id : Tooltip t -> ID
id (Tooltip tip) =
    tip.id


text : Tooltip t -> t
text (Tooltip { text }) =
    text


visibleTime : Tooltip t -> Time
visibleTime (Tooltip { visibleTime }) =
    visibleTime


position : Tooltip t -> Position
position (Tooltip { position }) =
    position


length : Tooltip t -> TooltipLength
length (Tooltip { length }) =
    length


emptyTooltips : Tooltips t
emptyTooltips =
    Dict.empty


removeInSchedule : Tooltip t -> (ID -> msg) -> Cmd msg
removeInSchedule ((Tooltip { visibleTime, id }) as tip) fn =
    removeIn visibleTime id fn


removeIn : Time -> ID -> (ID -> msg) -> Cmd msg
removeIn time id fn =
    if time /= Util.forever then
        Util.delayMsg time (fn id)
    else
        Cmd.none


add : Tooltip t -> HasTooltips a t -> HasTooltips a t
add tip model =
    { model | tooltips = Dict.insert (idStr tip) tip model.tooltips }


idStr : Tooltip t -> String
idStr (Tooltip { id }) =
    case id of
        ID idString ->
            idString


remove : ID -> HasTooltips a t -> HasTooltips a t
remove (ID id) model =
    { model | tooltips = Dict.remove id model.tooltips }


setActive : Bool -> Tooltip t -> Tooltip t
setActive active (Tooltip tip) =
    Tooltip { tip | active = active }


updateTooltips : ID -> (Tooltip t -> Tooltip t) -> HasTooltips a t -> HasTooltips a t
updateTooltips (ID id) f model =
    { model | tooltips = Dict.update id (Maybe.map f) model.tooltips }


activate : ID -> HasTooltips a t -> HasTooltips a t
activate id model =
    model
        |> updateTooltips id (setActive True)


deactivate : ID -> HasTooltips a t -> HasTooltips a t
deactivate id model =
    model
        |> updateTooltips id (setActive False)


isActive : ID -> HasTooltips a t -> Bool
isActive (ID id) { tooltips } =
    tooltips
        |> Dict.get id
        |> Maybe.map (\(Tooltip { active }) -> active)
        |> Maybe.withDefault False


type alias Translator t =
    Language -> t -> String


view : Tooltip t -> Translator t -> HasTooltips a t -> List (Html msg) -> Html msg
view (Tooltip { text, active, position, length }) t model body =
    item
        { position = position
        , length = length
        , text = t model.language text
        , visible = active
        }
        body


viewIfActive : Tooltip t -> Translator t -> HasTooltips a t -> List (Html msg) -> Html msg
viewIfActive (Tooltip { id }) t model body =
    viewWithId id t model body


viewWithId : ID -> Translator t -> HasTooltips a t -> List (Html msg) -> Html msg
viewWithId (ID id) t model body =
    let
        bodyOnly =
            Html.span [] body
    in
    case Dict.get id model.tooltips of
        Nothing ->
            bodyOnly

        Just ((Tooltip { active }) as tip) ->
            if active then
                view tip t model body
            else
                bodyOnly


type TooltipLength
    = Small
    | Medium
    | Large
    | XLarge
    | Fit
    | Auto


type alias TooltipConfig =
    { position : Position
    , length : TooltipLength
    , text : String
    , visible : Bool
    }


item : TooltipConfig -> List (Html msg) -> Html msg
item { position, length, text, visible } body =
    let
        commonAttrs =
            [ attribute "data-balloon" text
            , attribute "data-balloon-pos" (tooltipPositionString position)
            , class "Tooltip"
            ]

        baseAttrs =
            if visible then
                attribute "data-balloon-visible" ""
                    :: commonAttrs
            else
                commonAttrs

        attributes =
            case length of
                Auto ->
                    baseAttrs

                _ ->
                    attribute "data-balloon-length"
                        (tooltipLengthString length)
                        :: baseAttrs
    in
    Html.span attributes body


tooltipPositionString : Position -> String
tooltipPositionString position =
    case position of
        Top ->
            "up"

        Bottom ->
            "down"

        Left ->
            "left"

        Right ->
            "right"


tooltipLengthString : TooltipLength -> String
tooltipLengthString length =
    case length of
        Small ->
            "small"

        Medium ->
            "medium"

        Large ->
            "large"

        XLarge ->
            "xlarge"

        Fit ->
            "fit"

        Auto ->
            ""
