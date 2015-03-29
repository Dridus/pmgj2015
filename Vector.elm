module Vector where

import Basics

type alias Vect = { x : Float, y : Float }

toTuple : Vect -> (Float, Float)
toTuple { x, y } = (x, y)

toTupleTruncate : Vect -> (Int ,Int)
toTupleTruncate { x, y } = (truncate x, truncate y)

scalar : (Float -> Float) -> Vect -> Vect
scalar f { x, y } = { x = f x, y = f y }

scale : Float -> Vect -> Vect
scale s { x, y } = { x = x*s, y = y*s }

vadd : Vect -> Vect -> Vect
vadd u v = { x = u.x + v.x, y = u.y + v.y }

vsub : Vect -> Vect -> Vect
vsub u v = { x = u.x - v.x, y = u.y - v.y }

magnitude : Vect -> Float
magnitude { x, y } = sqrt (x*x + y*y)

dist : Vect -> Vect -> Float
dist u v = magnitude (vsub v u)

clampMagnitude : Float -> Vect -> Vect
clampMagnitude maxMag v = 
    let mag = magnitude v
        norm = if mag == 0 then zero else scale (1/mag) v
    in scale (min mag (magnitude v)) norm

normalize : Vect -> Vect
normalize v =
    let mag = magnitude v
    in if mag == 0 then zero else scale (1/mag) v

zeroish : Vect -> Bool
zeroish = flip (<) 0.1 << magnitude

negate : Vect -> Vect
negate { x, y } = { x = Basics.negate x, y = Basics.negate y }

dot : Vect -> Vect -> Float
dot u v = u.x * v.x + u.y * v.y

angle : Vect -> Float
angle v = atan2 v.y v.x

angleDelta : Vect -> Vect -> Float
angleDelta u v = angle v - angle u

timeTween : Float -> Float -> Vect -> Vect -> Vect
timeTween deltaMS targetS from to =
    let factor = min 1.0 (deltaMS / (targetS * 1000.0))
        result = vadd from (scale factor (vsub to from))
    in if dist result to < 0.01 then to else result

xUnit : Vect
xUnit = { x = 1.0, y = 0.0 }
yUnit : Vect
yUnit = { x = 0.0, y = 1.0 }
zero : Vect
zero = { x = 0.0, y = 0.0 }

timeScale : Float -> Vect -> Vect -> Vect
timeScale timeDelta vel pos =
    vadd pos (scale (timeDelta / 1000.0) vel)

