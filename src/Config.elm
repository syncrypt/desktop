module Config exposing (..)

import Time exposing (Time)


type alias Config =
    { apiUrl : String
    , apiAuthToken : String
    , updateInterval : Time
    , pathSeparator : String
    }
