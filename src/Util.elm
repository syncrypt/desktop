module Util exposing (..)

import Time exposing (Time)
import Task exposing (Task, andThen)
import Process


{-| Creates a new `Task` that delays a given `Task` by a given time.

    delay 1000 (Task.succeed "Done!")
-}
delay : Time -> Task err a -> Task err a
delay time task =
    Process.sleep 1000
        |> andThen (\x -> task)
