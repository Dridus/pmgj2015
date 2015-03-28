module Gamepad.XBox where

import Gamepad
import Gamepad (Gamepad)
import Signal (Signal, (<~), (~))

a : Gamepad -> Signal Bool
a pad = Gamepad.buttonIsDown pad 0

b : Gamepad -> Signal Bool
b pad = Gamepad.buttonIsDown pad 1

x : Gamepad -> Signal Bool
x pad = Gamepad.buttonIsDown pad 2

y : Gamepad -> Signal Bool
y pad = Gamepad.buttonIsDown pad 3

lb : Gamepad -> Signal Bool
lb pad = Gamepad.buttonIsDown pad 4

rb : Gamepad -> Signal Bool
rb pad = Gamepad.buttonIsDown pad 5

lt : Gamepad -> Signal Bool
lt pad = Gamepad.buttonIsDown pad 6

ltValue : Gamepad -> Signal Float
ltValue pad = Gamepad.buttonValue pad 6

rt : Gamepad -> Signal Bool
rt pad = Gamepad.buttonIsDown pad 7

rtValue : Gamepad -> Signal Float
rtValue pad = Gamepad.buttonValue pad 7

back : Gamepad -> Signal Bool
back pad = Gamepad.buttonIsDown pad 8

start : Gamepad -> Signal Bool
start pad = Gamepad.buttonIsDown pad 9

leftStickPressed : Gamepad -> Signal Bool
leftStickPressed pad = Gamepad.buttonIsDown pad 10

rightStickPressed : Gamepad -> Signal Bool
rightStickPressed pad = Gamepad.buttonIsDown pad 11

up : Gamepad -> Signal Bool
up pad = Gamepad.buttonIsDown pad 12

down : Gamepad -> Signal Bool
down pad = Gamepad.buttonIsDown pad 13

left : Gamepad -> Signal Bool
left pad = Gamepad.buttonIsDown pad 14

right : Gamepad -> Signal Bool
right pad = Gamepad.buttonIsDown pad 15

leftStick : Gamepad -> Signal (Float, Float)
leftStick pad = (,) <~ leftStickX pad ~ leftStickY pad

leftStickX : Gamepad -> Signal Float
leftStickX pad = Gamepad.axis pad 0

leftStickY : Gamepad -> Signal Float
leftStickY pad = Gamepad.axis pad 1

rightStick : Gamepad -> Signal (Float, Float)
rightStick pad = (,) <~ rightStickX pad ~ rightStickY pad

rightStickX : Gamepad -> Signal Float
rightStickX pad = Gamepad.axis pad 2

rightStickY : Gamepad -> Signal Float
rightStickY pad = Gamepad.axis pad 3

