module Main exposing (..)

import Config exposing (Config)
import Html
import MainScreen
import Model exposing (Model, Msg)


main : Program Config Model Msg
main =
    Html.programWithFlags
        { init = MainScreen.init
        , subscriptions = MainScreen.subscriptions
        , view = MainScreen.view
        , update = MainScreen.update
        }
