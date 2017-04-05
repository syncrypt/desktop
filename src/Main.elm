module Main exposing (..)

import Html
import Model exposing (Model, Msg)
import MainScreen
import Config exposing (Config)


main : Program Config Model Msg
main =
    Html.programWithFlags
        { init = MainScreen.init
        , subscriptions = MainScreen.subscriptions
        , view = MainScreen.view
        , update = MainScreen.update
        }
