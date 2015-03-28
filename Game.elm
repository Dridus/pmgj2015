import Color
import Gamepad
import Gamepad (Gamepad)
import Gamepad.XBox as XB
import Graphics.Collage as C
import Graphics.Element (Element)
import Maybe
import Maybe (Maybe)
import Signal
import Signal (Signal, (<~), (~))
import Time
import Window

type alias Vect = { x: Float, y: Float }
type alias PlayerControls = { move: Vect, aim: Vect }
type alias Controls = { first: PlayerControls, second: PlayerControls }
type alias Input = { timeDelta: Float, controls: Controls }
type alias GameState = {}

initialState: GameState
initialState = {}

main: Signal Element
main = Gamepad.withPolling (Signal.constant True) <| \ _ ->
    display <~ Window.dimensions ~ gameState

delta: Signal Float
delta = Time.fps 30
gameState: Signal GameState
gameState = Signal.foldp stepGame initialState input
input: Signal Input
input = Signal.sampleOn delta (Input <~ delta ~ controls)
controls: Signal Controls
controls = Controls <~ playerControls 0 ~ playerControls 1
playerControls: Gamepad -> Signal PlayerControls
playerControls controller = PlayerControls <~ (uncurry Vect <~ XB.leftStick controller) ~ (uncurry Vect <~ XB.rightStick controller)

stepGame: Input -> GameState -> GameState
stepGame { timeDelta, controls } state =
    state

display: (Int, Int) -> GameState -> Element
display (w, h) state =
    let square = C.filled Color.blue <| C.square 50
    in C.collage w h [square]

