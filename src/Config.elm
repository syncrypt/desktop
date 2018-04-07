module Config exposing (Config)

import Time exposing (Time)


type alias Config =
    { apiUrl : String
    , apiAuthToken : String
    , updateInterval : Time
    , pathSeparator : String
    , version : String
    , locale : String
    }
