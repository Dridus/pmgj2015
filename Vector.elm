module Vector where

type alias Vect = { x : Float, y : Float }

toTuple : Vect -> (Float, Float)
toTuple { x, y } = (x, y)

scalar : (Float -> Float) -> Vect -> Vect
scalar f { x, y } = { x = f x, y = f y }

smul : Vect -> Float -> Vect
smul { x, y } s = { x = x*s, y = y*s }

sdiv : Vect -> Float -> Vect
sdiv { x, y } s = { x = x/s, y = y/s }

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
        norm = if mag == 0 then zero else sdiv v mag
    in smul norm (min mag (magnitude v))

normalize : Vect -> Vect
normalize v =
    let mag = magnitude v
    in if mag == 0 then zero else sdiv v mag

zeroish : Vect -> Bool
zeroish = flip (<) 0.1 << magnitude

dot : Vect -> Vect -> Float
dot u v = u.x * v.x + u.y * v.y

angle : Vect -> Float
angle v = atan2 v.y v.x

angleDelta : Vect -> Vect -> Float
angleDelta u v = angle v - angle u

timeTween : Float -> Float -> Vect -> Vect -> Vect
timeTween deltaMS targetS from to =
    let factor = min 1.0 (deltaMS / (targetS * 1000.0))
        result = vadd from (smul (vsub to from) factor)
    in if dist result to < 0.01 then to else result

xUnit : Vect
xUnit = { x = 1.0, y = 0.0 }
yUnit : Vect
yUnit = { x = 0.0, y = 1.0 }
zero : Vect
zero = { x = 0.0, y = 0.0 }

