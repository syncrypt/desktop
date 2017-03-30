module Config exposing (..)

import Time exposing (Time)
import Maybe exposing (withDefault)


type alias Config =
    { apiUrl : String
    , apiAuthToken : String
    , updateInterval : Time
    }
