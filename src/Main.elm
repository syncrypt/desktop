module Main exposing (..)

import Html exposing (Html, div, text)
import Model exposing (Model, Msg)
import View.MainScreen as MainScreen
import Config exposing (Config)


main : Program Config Model Msg
main =
    Html.programWithFlags
        { init = MainScreen.init
        , subscriptions = MainScreen.subscriptions
        , view = MainScreen.view
        , update = MainScreen.update
        }
