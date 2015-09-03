module Main where

import Prelude

import Control.Arrow
import Control.Apply
import Control.Extend
import Control.Monad
import Control.Monad.Eff
import Control.Monad.Eff.Random
import Control.Monad.Eff.Unsafe
import Control.Monad.Eff.Ref
import Data.Either
import Data.Maybe
import Data.Functor
import Data.Foldable
import Data.Tuple
import Data.Profunctor
import Data.Profunctor.Strong
import Control.Bind
import Data.Time (Milliseconds(..))
import Data.Date (nowEpochMilliseconds, Now())

import qualified Control.Monad.Eff.JQuery as JQ

import MiniYampa

import DOM
import DOM.RequestAnimationFrame (requestAnimationFrame)

import qualified Graphics.Canvas as C
import qualified Graphics.Drawing as D

canvasHeight  = 600.0
canvasWidth   = 300.0
groundHeight  = canvasHeight / 8.0
pipeWidth     = 60.0
pipeGap       = 200.0
pipeMaxHeight = canvasHeight - pipeGap - groundHeight
pipeMinHeight = groundHeight + 50.0
cubeX         = 100.0
cubeWidth     = 30.0
cubeHeight    = 30.0
cubeColour    = D.rgb 237.0 186.0 0.0
pipeColour    = D.rgb 26.0 175.0 93.0
groundColour  = D.rgb 206.0 177.0 113.0

newtype AppState = AppState { cube   :: Cube
                            , pipe   :: Pipe
                            }

instance eqAppState :: Eq AppState where
  eq (AppState a) (AppState b) = a.cube.y == b.cube.y
                              && a.cube.v == b.cube.v
                              && a.pipe.x == b.pipe.x
                              && a.pipe.h == b.pipe.h

initAppState = AppState { cube: initCube, pipe: initPipe }

type AppInput = { increment :: Event Int }

type Cube = { y :: Number
            , v :: Number
            }

initCube = { y: canvasHeight / 2.0, v: 0.0 }

type Pipe = { x :: Number
            , h :: Number
            }

initPipe = { x: canvasWidth, h: 200.0 }

game :: SF AppInput AppState
game = switch sf idle
  where sf = gameSession >>> arr dup >>> second (checkCollision ^>> edge)
                                     >>^ extend (uncurry tagWith)

gameSession :: SF AppInput AppState
gameSession = (flappingCube initCube &&& movingPipe initPipe)
          >>^ uncurry { cube: _, pipe: _ } >>> AppState

idle :: AppState -> SF AppInput AppState
idle s = switch sf (\_ -> game)
  where sf = constant s &&& arr _.increment

fallingCube :: forall a. Cube -> SF a Cube
fallingCube { y: y0, v: v0 } = (y &&& v) >>^ uncurry { y: _, v: _ }
  where v = constant 200.0 >>> imIntegral v0
        y = v >>> imIntegral y0

flappingCube :: Cube -> SF AppInput Cube
flappingCube cube0 = dSwitch sf cont
  where sf = (fallingCube cube0 &&& arr _.increment) >>^ extend (uncurry tagWith)
        cont cube = flappingCube (cube { v = cube.v - 300.0 })

movingPipe :: forall a. Pipe -> SF a Pipe
movingPipe { x: x0, h: h0 } =
  dSwitch sf (\_ -> movingPipe { x: x0, h: unsafePure $ randomRange pipeMinHeight
                                                                    pipeMaxHeight })
  where
    sf = constant (-100.0) >>> imIntegral x0
                           >>> arr ({ x: _, h: h0 } &&& (< (-pipeWidth)))
                           >>> second edge
    unsafePure :: forall eff a. Eff eff a -> a
    unsafePure = runPure <<< unsafeInterleaveEff

checkCollision :: AppState -> Boolean
checkCollision (AppState s) = collidesPipes || collidesGround
  where collidesGround = s.cube.y + cubeHeight >= canvasHeight - groundHeight
        collidesPipeX = and [ cubeX + cubeWidth > s.pipe.x
                            , cubeX             < s.pipe.x + pipeWidth ]
        collidesPipes = collidesPipeX && not fitsPipeGap
        fitsPipeGap = and [ s.cube.y + cubeHeight < canvasHeight - s.pipe.h
                          , s.cube.y > canvasHeight - s.pipe.h - pipeGap ]

render :: AppState -> D.Drawing
render (AppState s) = fold [ground, bottomPipe, topPipe, cube]
  where
    cube       = D.filled (D.fillColor cubeColour)
                          (D.rectangle cubeX s.cube.y cubeWidth cubeHeight)
    topPipe    = D.filled (D.fillColor pipeColour)
                          (D.rectangle s.pipe.x 0.0 pipeWidth
                                       (canvasHeight - s.pipe.h - pipeGap))
    bottomPipe = D.filled (D.fillColor pipeColour)
                          (D.rectangle s.pipe.x
                                       (canvasHeight - s.pipe.h)
                                       pipeWidth s.pipe.h)
    ground     = D.filled (D.fillColor groundColour)
                          (D.rectangle 0.0 (canvasHeight - groundHeight)
                                       canvasWidth canvasHeight)

runDrawingApp :: forall input eff.
          SF input (Event D.Drawing)
       -> C.Context2D
       -> Ref input
       -> (input -> input)
       -> Eff (ref :: REF, now :: Now, dom :: DOM, canvas :: C.Canvas | eff) Unit
runDrawingApp sf ctx inputRef inputMod = do
  let render NoEvent = pure true
      render (Event drawing) = do
        C.setFillStyle "#ADD4F4" ctx
        C.fillRect ctx { x: 0.0, y: 0.0, w: canvasWidth, h: canvasHeight }
        D.render ctx drawing
        pure true

  rh <- reactInit (readRef inputRef) (\_ _ -> render) sf
  lastInteractionRef <- do Milliseconds t0 <- nowEpochMilliseconds
                           newRef t0
  let step = do
        Milliseconds now <- nowEpochMilliseconds
        lastInteraction <- readRef lastInteractionRef
        let dt = now - lastInteraction
        writeRef lastInteractionRef now
        input <- readRef inputRef
        modifyRef inputRef inputMod
        react rh $ Tuple (dt / 1000.0) $ Just input
        requestAnimationFrame step
  requestAnimationFrame step

main = do
  Just canvas <- C.getCanvasElementById "canvas"
  ctx <- C.getContext2D canvas
  canvasJQ <- JQ.select "canvas"
  inputRef <- newRef { increment: NoEvent }
  JQ.on "click" (\_ _ -> modifyRef inputRef (_ { increment = Event 1 })) canvasJQ
  runDrawingApp (game >>> updated >>^ map render) ctx inputRef (_ { increment = NoEvent })
