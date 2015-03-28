import Gamepad
import Graphics.Element (Element, flow, down)
import Maybe
import Signal (Signal, (<~), (~))
import Signal
import Text
import Window

main : Signal Element
main =
    Gamepad.withPolling (Signal.constant True) <| \ _ ->
        scene
            <~ Gamepad.count
             ~ Gamepad.connectedCount
             ~ Gamepad.status 0
             ~ Gamepad.buttonIsDown 0 0
             ~ Gamepad.axis 0 0
             ~ Gamepad.axis 0 1
             ~ Window.dimensions

scene : Int -> Int -> Maybe Gamepad.Status -> Bool -> Float -> Float -> (Int, Int) -> Element
scene count connectedCount statusMaybe aDown x y (w,h) =
    flow down
        [ Text.leftAligned <| Text.fromString <| "Count: " ++ toString count
        , Text.leftAligned <| Text.fromString <| "Connected count: " ++ toString connectedCount
        , Text.leftAligned <| Text.fromString <| "Gamepad status 0: " ++ (toString <| Maybe.map .id <| statusMaybe)
        , Text.leftAligned <| Text.fromString <| "Gamepad 0 'A' button: " ++ (toString aDown)
        , Text.leftAligned <| Text.fromString <| "Gamepad 0 X: " ++ (toString x)
        , Text.leftAligned <| Text.fromString <| "Gamepad 0 Y: " ++ (toString y)
        ]
