module Cmds exposing (delay)

import Process
import Task


delay : Int -> msg -> Cmd msg
delay ms msg =
    Process.sleep (toFloat ms)
        |> Task.map (\_ -> msg)
        |> Task.perform identity
