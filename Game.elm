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
    , vp       : VelPos
    , grapple  : Maybe Grapple
    , jump     : Bool
    }
type alias DancingState = {}

initialState : GameState
initialState = Going { first = initialPlayerGoing, second = initialPlayerGoing }
initialPlayerGoing : PlayerGoing
initialPlayerGoing =
    { aim      = Nothing
    , vp       = { vel = Vect 0 0, pos = Vect 0 groundY }
    , grapple  = Nothing
    , jump     = False
    }

stageDim : Vect
stageDim = Vect 50.0 30.0

groundY : Float
groundY = 1.0

gravityForce : Float
gravityForce = 9.8

gravityVect : Vect
gravityVect = Vect 0.0 (negate gravityForce)

dragCoefficient : Float
dragCoefficient = 1.1

playerArea : Float
playerArea = 1.0

airDensity : Float
airDensity = 1.293

runForce : Float
runForce = 20.0

runTopSpeed : Float
runTopSpeed = 20.0

stoppingForce : Float
stoppingForce = 10.0

jumpForce : Float
jumpForce = 20.0


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

applyVelocity : Float -> Vect -> Vect -> Vect
applyVelocity timeDelta vel pos =
    Vector.vadd pos (Vector.scale (timeDelta / 1000.0) vel)

stepPlayerGoing : Float -> PlayerControls -> PlayerGoing -> PlayerGoing
stepPlayerGoing timeDelta pc pg =
    let

        newAim = if Vector.magnitude pc.aim >= 0.7 then Just (Vector.angle <| Vector.normalize pc.aim) else Nothing

        onGround = pg.vp.pos.y <= groundY
        controlForceVector = Vector.clampMagnitude 1.0 pc.move

        didJump = pc.jump && not pg.jump
        jumpVect = if didJump && onGround then Vect 0.0 jumpForce else Vector.zero

        forceTime : Vect -> Vect
        forceTime vec = Vector.scale (timeDelta / 1000.0) vec

        applyDrag : Vect -> Vect
        applyDrag vec =
            let vel = Vector.magnitude vec
                dragForce = 0.1 * 0.5 * airDensity * (vel * vel) * dragCoefficient * playerArea
            in Vector.vadd (forceTime <| Vector.scale dragForce << Vector.normalize << Vector.negate <| vec) vec

        applyRunningForce : Vect -> Vect
        applyRunningForce vec =
            if onGround
                then
                    if Vector.magnitude vec < runTopSpeed
                        then Vector.vadd (forceTime <| Vector.scale runForce controlForceVector) vec
                        else vec
                else
                    vec -- FIXME air control

        applyStoppingForce : Vect -> Vect
        applyStoppingForce vec =
            if onGround && Vector.zeroish controlForceVector && not (Vector.zeroish vec)
                then Vector.vadd vec <| forceTime <| { x = (negate vec.x * stoppingForce), y = 0.0 }
                else vec

        applyRope : Vect -> Vect
        applyRope vec = vec -- FIXME
        {-
        ropeClamp pos =
            case pg.grapple of
                Just { vp, fixed } ->
                    if not fixed
                        then pos
                        else 
                            let mag = Vector.magnitude (Vector.vsub pg.vp.pos vp.pos)
                            in Vector.smul (Vector.normalize (Vector.vsub pos vp.pos)) mag
                Nothing -> pos
        -}

        newVel : Vect
        newVel = pg.vp.vel
            |> Vector.vadd jumpVect -- FIXME? because instantaneous, don't apply time scaling
            |> applyRunningForce
            |> Vector.vadd (forceTime gravityVect)
            |> applyDrag
            |> applyStoppingForce
            |> applyRope

        groundClamp pos = { pos | y <- max groundY pos.y }

        oldVP = pg.vp
        steppedVP = { pos = applyVelocity timeDelta newVel oldVP.pos, vel = newVel }

        applyGrappleVelocity = Maybe.map <| \ g ->
            if g.fixed
                then g
                else { g | vp <- { pos = applyVelocity timeDelta g.vp.vel g.vp.pos, vel = g.vp.vel } }

        newGrapple = pg.grapple
            |> applyGrappleVelocity

    in
        { aim     = newAim
        , vp      = { steppedVP | pos <- groundClamp steppedVP.pos }
        , jump    = pc.jump
        , grapple = newGrapple
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
    let
        stageScale = toFloat (w+5) / stageDim.x
        cameraX = negate ((first.vp.pos.x + second.vp.pos.x) / 2) + stageDim.y / 2.0

        stageScaleXf = T2D.scale stageScale
        stageTranslationXf = T2D.translation (cameraX + negate (stageDim.x / 2.0)) (negate (stageDim.y / 2.0))

        reticle pg =
            case pg.aim of
                Just a -> C.groupTransform (T2D.multiply (T2D.rotation a) (T2D.translation 3.0 0.0)) [C.filled Color.black <| C.circle 0.1]
                Nothing -> C.group []

        rope pg =
            case pg.grapple of
                Just { vp } -> C.traced C.defaultLine <| C.segment (Vector.toTuple pg.vp.pos) (Vector.toTuple vp.pos)
                Nothing -> C.group []

        player color pg =
            C.move (Vector.toTuple pg.vp.pos) <| C.moveY 2.0 <| C.group
                [ reticle pg, C.filled color <| C.rect 0.5 2.0 ]
        stage =
            C.groupTransform (T2D.multiply stageScaleXf stageTranslationXf)
                [ C.move (stageDim.x / 2, stageDim.y / 2) <| C.filled Color.gray <| C.rect stageDim.x stageDim.y
                , player Color.red first
                , rope first
                , player Color.blue second
                , rope second
                ]
        hud = C.group [] -- [C.toForm <| Text.centered <| Text.fromString <| toString first]
    in C.collage w h [ stage, hud ]
