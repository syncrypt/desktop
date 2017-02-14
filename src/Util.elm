module Util exposing (..)

import Time exposing (Time)
import Task exposing (Task, andThen, attempt)
import Process
import Model exposing (Msg)
import Result exposing (Result)


{-| Creates a new `Task` that delays a given `Task` by a given time.

    delay 1000 (Task.succeed "Done!")
-}
delay : Time -> Task err a -> Task err a
delay time task =
    Process.sleep 1000
        |> andThen (\x -> task)


{-| Attempts to perform a `Task` after a given delay.

    let
        request = Daemon.getVaults model.config
    in
        attemptDelayed 1000 UpdatedVaultsFromApi request
-}
attemptDelayed : Time -> (Result err a -> Msg) -> Task err a -> Cmd Msg
attemptDelayed time msg task =
    task
        |> delay time
        |> attempt msg
