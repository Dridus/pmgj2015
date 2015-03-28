module Gamepad where

import Maybe (Maybe)
import Native.Gamepad
import Signal (Signal)

count : Signal Int
count = Native.Gamepad.count

connectedCount : Signal Int
connectedCount = Native.Gamepad.connectedCount

type alias Gamepad = Int
type alias Button = Int
type alias Axis = Int

withPolling : Signal Bool -> (() -> a) -> a
withPolling = Native.Gamepad.withPolling

type alias Status = { id : String }

status : Gamepad -> Signal (Maybe Status)
status = Native.Gamepad.status

buttonIsDown : Gamepad -> Button -> Signal Bool
buttonIsDown = Native.Gamepad.buttonIsDown

buttonValue : Gamepad -> Button -> Signal Float
buttonValue = Native.Gamepad.buttonValue

axis : Gamepad -> Axis -> Signal Float
axis = Native.Gamepad.axisValue


