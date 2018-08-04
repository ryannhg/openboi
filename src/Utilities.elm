module Utilities exposing (delayedCommand)

import Task exposing (Task)
import Process


delayedCommand : Float -> msg -> Cmd msg
delayedCommand delayInMs msg =
    Task.perform
        (always msg)
        (Process.sleep delayInMs)
