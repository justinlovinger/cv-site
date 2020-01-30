-- Based on [purescript-behaviors example]
-- (https://github.com/paf31/purescript-behaviors/blob/master/test/Main.purs).
module Component.Dynamiccircles (dynamicCircles) where

import Prelude

import Color.Scheme.Website as C
import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (canvas)
import Concur.React.Props (_id, height, width)
import Control.Alt ((<|>))
import Control.Plus (empty)
import Data.Array (sortBy, (..))
import Data.Foldable (foldMap)
import Data.Int (toNumber)
import Data.Maybe (Maybe(Just,Nothing), maybe)
import Data.Newtype (unwrap)
import Data.Set (isEmpty)
import Effect.Class (liftEffect)
import Effect.Random (random)
import Effect.Timer (setTimeout)
import FRP.Behavior (Behavior, animate, fixB, integral', switcher)
import FRP.Behavior.Mouse (buttons)
import FRP.Behavior.Mouse as Mouse
import FRP.Behavior.Time as Time
import FRP.Event.Mouse (Mouse, getMouse, down)
import Global (infinity)
import Graphics.Canvas (getCanvasElementById, getContext2D)
import Graphics.Drawing (Drawing, circle, fillColor, filled, lineWidth, outlineColor, outlined, rectangle, render, scale, translate)

type Circle = { x ∷ Number, y ∷ Number, size ∷ Number }

scene ∷ Mouse → { w ∷ Number, h ∷ Number } → Behavior Drawing
scene mouse { w, h } = pure background <> map renderCircles circles where
  background ∷ Drawing
  background = filled (fillColor C.background) (rectangle 0.0 0.0 w h)

  scaleFactor ∷ Number
  scaleFactor = max w h / 16.0

  renderCircle ∷ Circle → Drawing
  renderCircle { x, y, size } =
    scale scaleFactor scaleFactor <<< translate x y <<< scale size size $
      outlined
        (outlineColor (C.altBackground) <> lineWidth ((1.0 + size * 2.0) / scaleFactor))
        (circle 0.0 0.0 0.5)

  renderCircles ∷ Array Circle → Drawing
  renderCircles = foldMap renderCircle

  -- `swell` is an interactive function of time defined by a differential equation:
  --
  -- d^2s/dt^2
  --   | mouse down = ⍺ - βs
  --   | mouse up   = ɣ - δs - ε ds/dt
  --
  -- So the function exhibits either decay or growth depending on if
  -- the mouse is pressed or not.
  --
  -- We can solve the differential equation by integration using `solve2'`.
  swell ∷ Behavior Number
  swell =
      fixB 2.0 \b →
        integral' 2.0 (unwrap <$> Time.seconds)
          let db = fixB 10.0 \db_ →
                     integral' 10.0 (unwrap <$> Time.seconds) (f <$> buttons mouse <*> b <*> db_)
          in switcher db (down $> db)
    where
      f bs s ds | isEmpty bs = -8.0 * (s - 1.0) - ds * 2.0
                | otherwise = 2.0 * (4.0 - s)

  circles ∷ Behavior (Array Circle)
  circles = toCircles <$> Mouse.position mouse <*> swell where
    toCircles m sw =
        sortBy (comparing (\{ x, y } → -(dist x y m))) do
          i ← 0 .. 16
          j ← 0 .. 16
          let x = toNumber i
              y = toNumber j
              d = dist x y m
          pure { x
               , y
               , size: 0.1 + (1.0 + sw) / (d + 1.5)
               }
      where
        dist x y = maybe infinity \{ x: mx, y: my } →
          let dx = x - toNumber mx / scaleFactor
              dy = y - toNumber my / scaleFactor
          in dx * dx + dy * dy

dynamicCircles ∷ ∀ a. Number → Number → Widget HTML a
dynamicCircles w h = do
    canvasId ← liftEffect random
    canvas [ _id $ show canvasId, width (show w), height (show h) ] []
      <|> (liftEffect (runCanvas canvasId) *> empty)
  where
    runCanvas idx = do
      mcanvas ← getCanvasElementById $ show idx
      case mcanvas of
        Just canvas → do
          ctx ← getContext2D canvas
          mouse ← getMouse
          _ ← animate (scene mouse { w, h }) (render ctx)
          pure unit
        Nothing → do -- Wait for canvas
          _ ← setTimeout 10 (runCanvas idx)
          pure unit
