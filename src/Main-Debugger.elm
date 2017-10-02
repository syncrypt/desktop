module Main exposing (..)

import MainScreen
import TimeTravel.Html as TimeTravel


main =
    TimeTravel.programWithFlags
        { init = MainScreen.init
        , subscriptions = MainScreen.subscriptions
        , view = MainScreen.view
        , update = MainScreen.update
        }
