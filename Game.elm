import Color
import Gamepad
import Gamepad (Gamepad)
import Gamepad.XBox as XB
import Graphics.Collage as C
import Graphics.Element (Element)
import List
import Maybe
import Maybe (Maybe(Just, Nothing))
import Signal
import Signal (Signal, (<~), (~))
import Text
import Time
import Transform2D as T2D
import Transform2D (Transform2D)
import Vector
import Vector (Vect)
import Window

type alias PlayerControls = { move : Vect, aim : Vect, shoot : Bool, jump : Bool }
type alias Controls = { first : PlayerControls, second : PlayerControls }
type alias Input = { timeDelta : Float, controls : Controls }
type GameState = Preparing | Going GoingState | Dancing DancingState | Won
type alias GoingState = { first : PlayerGoing, second : PlayerGoing }
type alias VelPos = { vel : Vect, pos : Vect }
type alias Grapple = { vp : VelPos, fixed : Bool }
type alias PlayerGoing =
    { aim      : Maybe Float
    , normMove : Vect
    , vp       : VelPos
    , jump     : Vect
    , grapple  : Maybe Grapple
    }
type alias DancingState = {}

initialState : GameState
initialState = Going { first = initialPlayerGoing, second = initialPlayerGoing }
initialPlayerGoing : PlayerGoing
initialPlayerGoing =
    { aim      = Nothing
    , normMove = Vect 0 0
    , vp       = { vel = Vect 0 0, pos = Vect 0 groundY }
    , jump     = Vector.zero
    , grapple  = Nothing
    }

stageDim : Vect
stageDim = Vect 1000.0 400.0

groundY : Float
groundY = 40.0

airRest : Float
airRest = 1.0

airSpeed : Float
airSpeed = 50.0

groundRest : Float
groundRest = 0.1

groundSpeed : Float
groundSpeed = 100.0

gravity : Vect
gravity = Vect 0.0 -60.0

jumpForce : Float
jumpForce = 100.0

jumpRest : Float
jumpRest = 5.0

main : Signal Element
main = Gamepad.withPolling (Signal.constant True) <| \ _ ->
    display <~ Window.dimensions ~ delta ~ gameState

delta     : Signal Float
delta     = Time.fps 30
gameState : Signal GameState
gameState = Signal.foldp stepGame initialState input
input     : Signal Input
input     = Signal.sampleOn delta (Input <~ delta ~ controls)
controls  : Signal Controls
controls  = Controls <~ playerControls 0 ~ playerControls 1
playerControls : Gamepad -> Signal PlayerControls
playerControls controller =
    let processStick = filterStick << flipStick << uncurry Vect
        flipStick { x, y } = { x = x, y = negate y }
        filterStick v = if Vector.magnitude v < 0.3 then Vector.zero else v
    in PlayerControls
            <~ (processStick <~ XB.leftStick controller)
             ~ (processStick <~ XB.rightStick controller)
             ~ XB.rb controller
             ~ XB.a controller

stepGame : Input -> GameState -> GameState
stepGame { timeDelta, controls } state =
    case state of
        Going gs ->
            let newFirst = stepPlayerGoing timeDelta controls.first gs.first
                newSecond = stepPlayerGoing timeDelta controls.second gs.second
            in Going { first = newFirst, second = newSecond }
        other -> other

stepVP : Float -> VelPos -> VelPos
stepVP timeDelta old =
    { old | pos <- Vector.vadd old.pos (Vector.smul old.vel <| timeDelta / 1000.0) }

stepPlayerGoing : Float -> PlayerControls -> PlayerGoing -> PlayerGoing
stepPlayerGoing timeDelta pc pg =
    let newAim = if Vector.magnitude pc.aim >= 0.7 then Just (Vector.angle <| Vector.normalize pc.aim) else Nothing
        onGround = pg.vp.pos.y <= groundY
        normMoveStick = Vector.clampMagnitude 1.0 pc.move
        newNormMove =
            if onGround
                then Vector.timeTween timeDelta groundRest pg.normMove { x = normMoveStick.x, y = 0.0 }
                else Vector.timeTween timeDelta airRest    pg.normMove { x = normMoveStick.x, y = min 0.0 normMoveStick.y }
        newVel =
            Vector.vadd newJump <|
                if onGround
                    then Vector.smul newNormMove groundSpeed
                    else    Vector.smul newNormMove airSpeed
                         |> Vector.vadd gravity
        newJump =
            if onGround
                then if pc.jump
                    then Vect 0.0 jumpForce
                    else Vector.zero
                else Vector.timeTween timeDelta jumpRest pg.jump Vector.zero
        ropeClamp pos =
            case pg.grapple of
                Just { vp, fixed } ->
                    if not fixed
                        then pos
                        else 
                            let mag = Vector.magnitude (Vector.vsub pg.vp.pos vp.pos)
                            in Vector.smul (Vector.normalize (Vector.vsub pos vp.pos)) mag
                Nothing -> pos
        groundClamp pos =
            { pos | y <- max groundY pos.y }
        steppedVP = let vp = pg.vp in stepVP timeDelta { vp | vel <- newVel }
        newVP = { steppedVP | pos <- groundClamp <| ropeClamp <| steppedVP.pos }
        newGrapple =
            case pg.grapple of
                Just g ->
                    if g.fixed
                        then pg.grapple
                        else Just { g | vp <- stepVP timeDelta g.vp }
                Nothing ->
                    Nothing
    in
        { aim      = newAim
        , normMove = newNormMove
        , vp       = newVP
        , jump     = newJump
        , grapple  = newGrapple
        }

display : (Int, Int) -> Float -> GameState -> Element
display (w, h) timeDelta state =
    case state of
        Going gs ->
            displayGoing (w, h) timeDelta gs
        other ->
            Text.asText other

displayGoing : (Int, Int) -> Float -> GoingState -> Element
displayGoing (w, h) timeDelta { first, second } =
    let stageScale = toFloat (w+5) / stageDim.x
        reticle pg =
            case pg.aim of
                Just a -> C.groupTransform (T2D.multiply (T2D.rotation a) (T2D.translation 50.0 0.0)) [C.filled Color.black <| C.circle 2.0]
                Nothing -> C.group []
        rope pg =
            case pg.grapple of
                Just { vp } -> C.traced C.defaultLine <| C.segment (Vector.toTuple pg.vp.pos) (Vector.toTuple vp.pos)
                Nothing -> C.group []
        player color pg =
            C.move (Vector.toTuple pg.vp.pos) <| C.group
                [ reticle pg, C.filled color <| C.square 10 ]
        stage =
            C.groupTransform (T2D.multiply (T2D.scale stageScale) (T2D.translation -(stageDim.x / 2.0) -(stageDim.y / 2.0)))
                [ C.move (stageDim.x / 2, stageDim.y / 2) <| C.filled Color.gray <| C.rect stageDim.x stageDim.y
                , player Color.red first
                , rope first
                , player Color.blue second
                , rope second
                ]
        hud = C.group [] -- [C.toForm <| Text.centered <| Text.fromString <| toString first]
    in C.collage w h [ stage, hud ]
