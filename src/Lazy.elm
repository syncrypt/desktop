module Lazy exposing (Lazy(..), withDefault)


type Lazy data
    = NotLoaded
    | Loading
    | Loaded data


withDefault : a -> Lazy a -> a
withDefault default lazy =
    case lazy of
        Loaded val ->
            val

        _ ->
            default
