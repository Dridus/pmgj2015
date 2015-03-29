import Array
import Array (Array)
import Color
import Debug
import Gamepad
import Gamepad (Gamepad)
import Gamepad.XBox as XB
import Graphics.Collage as C
import Graphics.Element as E
import Graphics.Element (Element)
import List
import Maybe
import Maybe (Maybe(Just, Nothing))
import Random
import Signal
import Signal (Signal, (<~), (~))
import Text
import Time
import Transform2D as T2D
import Transform2D (Transform2D)
import Vector
import Vector (Vect)
import Window

numberOfTreesInCycle : Int
numberOfTreesInCycle = 8

treeNominalInterval : Float
treeNominalInterval = 20.0

treeIntervalError : Float
treeIntervalError = 3.0

treeCyclePeriod : Float
treeCyclePeriod = toFloat numberOfTreesInCycle * treeNominalInterval

stageDim : Vect
stageDim = Vect 25.0 14.3388210

groundY : Float
groundY = 0.0

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
runForce = 10.0

runTopSpeed : Float
runTopSpeed = 20.0

stoppingForce : Float
stoppingForce = 10.0

jumpForce : Float
jumpForce = 20.0

type alias PlayerControls = { move : Vect, aim : Vect, shoot : Bool, jump : Bool }
type alias Controls = { first : PlayerControls, second : PlayerControls }
type alias Input = { timeDelta : Float, controls : Controls }
type GameState = Preparing | Going GoingState | Dancing DancingState | Won
type alias TreeInstance = { style : TreeStyle, x : Float }
type alias TreeStyle = { path : String, dim : Vect }
type alias GoingState =
    { first        : PlayerGoing
    , second       : PlayerGoing
    , treeCycle    : Array TreeInstance
    , visibleTrees : List TreeInstance
    , centerX      : Float
    , minX         : Float
    , maxX         : Float
    }
type alias VelPos = { vel : Vect, pos : Vect }
type alias Grapple = { vp : VelPos, fixed : Bool }
type alias PlayerGoing =
    { aim      : Maybe Float
    , vp       : VelPos
    , grapple  : Maybe Grapple
    , jump     : Bool
    }
type alias DancingState = {}

tree1 : TreeStyle
tree1 = { path = "Assets/tree1.png", dim = { x = 566 * (stageDim.y / 1080), y = stageDim.y } }
tree2 : TreeStyle
tree2 = { path = "Assets/tree2.png", dim = { x = 537 * (stageDim.y / 1080), y = stageDim.y } }
tree3 : TreeStyle
tree3 = { path = "Assets/tree3.png", dim = { x = 325 * (stageDim.y / 1080), y = stageDim.y } }

treeStyles : List TreeStyle
treeStyles =
    [ tree1
    , tree2
    , tree3
    ]

makeTreeCycle : Random.Seed -> (Array TreeInstance, Random.Seed)
makeTreeCycle seed =
    let

        halfError      = treeIntervalError / 2.0
        xGenerator     = Random.float (negate halfError) halfError
        styleGenerator = Random.int 0 (List.length treeStyles - 1)

        generateTree i s =
            let (xVariance, s') = Random.generate xGenerator s
                (styleIndex, s'') = Random.generate styleGenerator s'
                tree =
                    { style = List.head <| List.drop styleIndex treeStyles
                    , x = toFloat i * treeNominalInterval + xVariance
                    }
            in (tree, s'')

    in    Array.initialize numberOfTreesInCycle identity
       |> Array.foldr (\ i (ts, s) -> let (t, s') = generateTree i s in (t :: ts, s')) ([], seed)
       |> (\ (treeList, seed) -> (Array.fromList treeList, seed))

initialState : GameState
initialState =
    let
        treeCycle = fst <| makeTreeCycle (Random.initialSeed 0 {- FIXME -})
    in Going
        { first        = initialPlayerGoing
        , second       = initialPlayerGoing
        , treeCycle    = treeCycle
        , visibleTrees = computeVisibleTrees treeCycle 0.0
        , centerX      = 0.0
        , minX         = 0.0 - stageDim.x / 2.0
        , maxX         = 0.0 + stageDim.x / 2.0
        }

initialPlayerGoing : PlayerGoing
initialPlayerGoing =
    { aim      = Nothing
    , vp       = { vel = Vect 0 0, pos = Vect 0 groundY }
    , grapple  = Nothing
    , jump     = False
    }

computeVisibleTrees : Array TreeInstance -> Float -> List TreeInstance
computeVisibleTrees cycle centerX =
    let
        mod = roundDownModulus treeCyclePeriod centerX
        cX  = centerX - mod

        candidateTree t =
            let visibleDistance = stageDim.x / 2.0 + t.style.dim.x
            in if | abs( t.x                    - cX) < visibleDistance -> Just <| { t | x <- t.x + mod }
                  | abs((t.x - treeCyclePeriod) - cX) < visibleDistance -> Just <| { t | x <- t.x + mod - treeCyclePeriod }
                  | abs((t.x + treeCyclePeriod) - cX) < visibleDistance -> Just <| { t | x <- t.x + mod + treeCyclePeriod }
                  | otherwise                                           -> Nothing

    in Array.foldl (\ t fs -> candidateTree t |> Maybe.map (flip (::) fs) |> Maybe.withDefault fs) [] cycle

stepGame : Input -> GameState -> GameState
stepGame { timeDelta, controls } state =
    case state of
        Going gs -> stepGoing timeDelta controls gs
        other -> other

stepGoing : Float -> Controls -> GoingState -> GameState
stepGoing timeDelta controls gs =
    let

        newFirst        = stepPlayerGoing timeDelta controls.first  gs.first
        newSecond       = stepPlayerGoing timeDelta controls.second gs.second

        newCenterX      = (newFirst.vp.pos.x + newSecond.vp.pos.x) / 2

        newVisibleTrees = computeVisibleTrees gs.treeCycle newCenterX

    in Going
        { first        = newFirst
        , second       = newSecond
        , treeCycle    = gs.treeCycle
        , visibleTrees = newVisibleTrees
        , centerX      = newCenterX
        , minX         = newCenterX - stageDim.x / 2.0
        , maxX         = newCenterX + stageDim.x / 2.0
        }

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
        steppedVP = { pos = Vector.timeScale timeDelta newVel oldVP.pos, vel = newVel }

        applyGrappleVelocity = Maybe.map <| \ g ->
            if g.fixed
                then g
                else { g | vp <- { pos = Vector.timeScale timeDelta g.vp.vel g.vp.pos, vel = g.vp.vel } }

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

rearUnscaledDim : Vect
rearUnscaledDim = Vect 3766 1080

middleUnscaledDim : Vect
middleUnscaledDim = Vect 4693 1080

roundDownModulus : Float -> Float -> Float
roundDownModulus modulus f =
    f / modulus |> floor |> toFloat |> (*) modulus

displayGoing : (Int, Int) -> Float -> GoingState -> Element
displayGoing (w, h) timeDelta state =
    let
        stageScale     = toFloat (w+5) / stageDim.x
        halfStageWidth = stageDim.x / 2.0
        cameraOffset   = Vect (negate state.centerX) (negate <| stageDim.y / 2.0)

        stageScaleXf       = T2D.scale stageScale
        stageTranslationXf = T2D.translation cameraOffset.x cameraOffset.y

        stageToScene = Vector.scale stageScale >> Vector.vadd cameraOffset

        background =
            let
                backgroundScale = toFloat h / rearUnscaledDim.y

                rearDim         = Vector.scale backgroundScale rearUnscaledDim
                rearDimTrunc    = Vector.toTupleTruncate rearDim
                rear            = C.toForm <| E.image (fst rearDimTrunc) (snd rearDimTrunc) "Assets/background.jpg"

                middleDim       = Vector.scale backgroundScale middleUnscaledDim
                middleDimTrunc  = Vector.toTupleTruncate middleDim
                middle          = C.toForm <| E.image (fst middleDimTrunc) (snd middleDimTrunc) "Assets/huge-trees.png"

                viewOffset = stageToScene cameraOffset
                rearX      = roundDownModulus rearDim.x (negate viewOffset.x) + viewOffset.x / 4.0
                rearX2     = if rearX >= 0 then rearX - rearDim.x else rearX + rearDim.x
                middleX    = roundDownModulus middleDim.x (negate viewOffset.x) + viewOffset.x / 2.0
                middleX2   = if middleX >= 0 then middleX - middleDim.x else middleX + middleDim.x
            in C.group
                [ C.moveX rearX rear
                , C.moveX rearX2 rear
                , C.moveX middleX middle
                , C.moveX middleX2 middle
                ]
                {-
                [ C.move (halfStageWidth + rearX, stageDim.y / 2.0) rear
                , C.move (halfStageWidth + rearX + width, stageDim.y / 2.0) rear
                , C.move (halfStageWidth + rearX - width, stageDim.y / 2.0) rear
                , C.move (halfStageWidth + middleX, stageDim.y / 2.0) middle
                , C.move (halfStageWidth + middleX + width, stageDim.y / 2.0) middle
                , C.move (halfStageWidth + middleX - width, stageDim.y / 2.0) middle
                ]
                -}

        trees = C.group <| List.map tree <| state.visibleTrees
        tree { style, x } =
            let (width, height) = Vector.toTupleTruncate style.dim
            in C.moveX x <| C.moveY (style.dim.y / 2.0) <| C.toForm <| E.image width height style.path

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
                [ trees
                , player Color.red state.first
                , rope state.first
                , player Color.blue state.second
                , rope state.second
                ]
        hud = C.group
            [
                -- C.toForm <| Text.centered <| Text.style (Text.defaultStyle |> \ s -> { s | color <- Color.white }) <| Text.fromString <| toString (treeModular centerX)
                -- [C.toForm <| Text.centered <| Text.fromString <| toString first]
            ]
    in C.collage w h
        [ C.filled Color.black <| C.rect (toFloat w) (toFloat h)
        , background
        , stage
        , hud
        ]



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
