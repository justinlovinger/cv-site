-- Based on [purescript-behaviors example]
-- (https://github.com/paf31/purescript-behaviors/blob/master/test/Main.purs).
module Component.DynamicCircles (dynamicCircles) where

import Prelude

import Color.Scheme.Website as C
import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (canvas)
import Concur.React.Props (_id, height, width)
import Control.Alt ((<|>))
import Control.MonadZero (guard)
import Control.Plus (empty)
import Data.Array ((..))
import Data.Foldable (foldMap)
import Data.Int (toNumber)
import Data.Maybe (Maybe(Just,Nothing), maybe)
import Data.Newtype (unwrap)
import Data.Set (isEmpty)
import Data.Time.Duration (Milliseconds(Milliseconds), Seconds(Seconds), fromDuration)
import Effect (Effect)
import Effect.Aff (Aff, delay, launchAff)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Random (random)
import FRP.Behavior (Behavior, animate, fixB, integral')
import FRP.Behavior.Mouse (buttons, position)
import FRP.Behavior.Time (seconds)
import FRP.Event.Mouse (Mouse, getMouse)
import Global (infinity)
import Graphics.Canvas (getCanvasElementById, getContext2D)
import Graphics.Drawing (Drawing, circle, fillColor, filled, lineWidth, outlineColor, outlined, rectangle, render, scale, translate)

type Circle = { x ∷ Number, y ∷ Number, size ∷ Number }

scene ∷ Mouse → { w ∷ Number, h ∷ Number } → Behavior Drawing
scene mouse { w, h } = pure background <> map renderCircles circles where
  background ∷ Drawing
  background = filled (fillColor C.background) (rectangle 0.0 0.0 w h)

  renderCircles ∷ Array Circle → Drawing
  renderCircles = foldMap renderCircle

  renderCircle ∷ Circle → Drawing
  renderCircle { x, y, size } =
    scale scaleFactor scaleFactor <<< translate x y <<< scale size size $
      outlined
        (outlineColor C.altBackground <> lineWidth ((1.0 + size * 2.0) / scaleFactor))
        (circle 0.0 0.0 0.5)

  circles ∷ Behavior (Array Circle)
  circles = toCircles <$> position mouse <*> swell where
    toCircles m sw = do
        i ← 0 .. (numCircles - 1)
        j ← 0 .. (numCircles - 1)
        let x = toNumber i
            y = toNumber j
            d = dist x y m
            size = (1.0 + sw) / (d + 1.5) - 0.2
        guard $ size > 0.0
        pure { x
             , y
             , size
             }
      where
        dist x y = maybe infinity \{ x: mx, y: my } →
          let dx = x - (toNumber mx) / scaleFactor
              dy = y - (toNumber my) / scaleFactor
          in dx * dx + dy * dy

  -- `swell` is an interactive function of time defined by a differential equation:
  --
  -- d^2s/dt^2
  --   | mouse down = ⍺ - βs
  --   | mouse up   = ɣ - δs - ε ds/dt
  --
  -- So the function exhibits either decay or growth depending on if
  -- the mouse is pressed or not.
  --
  -- We can solve the differential equation using an integral and a fixed point.
  swell ∷ Behavior Number
  swell =
      fixB 2.0 \b →
        integral' 2.0 (unwrap <$> seconds) $
          fixB 0.0 \db →
            integral' 0.0 (unwrap <$> seconds) $
              f <$> buttons mouse <*> b <*> db
    where
      f bs s ds | isEmpty bs = -8.0 * (s - 1.0) - ds * 2.0
                | otherwise = 2.0 * (4.0 - s)

  scaleFactor ∷ Number
  scaleFactor = (max w h) / (toNumber $ numCircles - 1)

  numCircles ∷ Int
  numCircles = 17

dynamicCircles ∷ ∀ a. Number → Number → Widget HTML a
dynamicCircles w h = do
    canvasId ← liftEffect random
    canvas [ _id $ show canvasId, width (show w), height (show h) ] []
      -- Start with delay, for canvas to mount.
      -- Canvas should mount
      -- before async runs.
      <|> (liftAff (delay (Milliseconds 0.0) `discard` \_ → liftEffect $ runCanvas canvasId) *> empty)
  where
    runCanvas ∷ Number → Effect Unit
    runCanvas idx = do
      mcanvas ← getCanvasElementById $ show idx
      case mcanvas of
        Just canvas → do
          ctx ← getContext2D canvas
          mouse ← getMouse
          stop ← animate (scene mouse { w, h }) (render ctx)
          _ ← launchAff $ stopWithoutCanvas idx stop -- Stop when canvas disappears
          pure unit
        Nothing → pure unit

    stopWithoutCanvas ∷ Number → Effect Unit → Aff Unit
    stopWithoutCanvas idx stop = do
      delay (fromDuration $ Seconds 2.0)
      mcanvas ← liftEffect $ getCanvasElementById $ show idx
      case mcanvas of
        Just canvas → stopWithoutCanvas idx stop
        Nothing → liftEffect stop
