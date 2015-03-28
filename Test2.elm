import Color
import Gamepad
import Gamepad.XBox as XB
import Graphics.Collage as C
import Graphics.Collage (defaultLine)
import Graphics.Element (Element)
import Maybe
import Signal
import Signal (Signal, (<~), (~))
import Text
import Window


main : Signal Element
main =
    Gamepad.withPolling (Signal.constant True) <| \ _ ->
        scene
            <~ Gamepad.status 0
             ~ XB.leftStick 0
             ~ XB.rightStick 0
             ~ Window.dimensions

scene : Maybe Gamepad.Status -> (Float, Float) -> (Float, Float) -> (Int, Int) -> Element
scene statusMaybe leftStick rightStick (wi, hi) =
    let w = toFloat wi
        h = toFloat hi
        stickSize = ((min w h) / 3.0)
    in
        C.collage wi hi <|
            [ C.move (0.0, (negate <| h / 2.0) + 20.0)
                <| C.toForm <| Text.centered <| Text.fromString <| toString <| Maybe.map .id <| statusMaybe
            , C.move (negate stickSize, 0.0) <| showStick stickSize leftStick
            , C.move (       stickSize, 0.0) <| showStick stickSize rightStick
            ]

showStick : Float -> (Float, Float) -> C.Form
showStick size pos =
    let (x, y) = pos
        formatPos = (round <| x * 1000.0, round <| y * 1000.0)
    in C.group
        [ C.outlined defaultLine <| C.square size
        , C.move (x/2.0 * size, negate <| y/2.0 * size) <| C.group
            [ C.filled Color.red <| C.circle 5.0
            , C.move (0.0, -13.0) <| C.toForm <| Text.centered <| Text.fromString <| toString formatPos
            ]
        ]

