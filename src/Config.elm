module Config exposing (..)

import Time exposing (Time)


type alias Config =
    { apiUrl : String
    , apiAuthToken : String
    , updateInterval : Time
    }


initialConfig : Config
initialConfig =
    { updateInterval = 5000
    , apiUrl = "http://127.0.0.1:28080/v1/"
    , apiAuthToken =
        -- set this to your actual api auth token
        "my API token here"
    }
