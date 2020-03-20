-- Based on [purescript-behaviors example]
-- (https://github.com/paf31/purescript-behaviors/blob/master/test/Main.purs).
module Component.DynamicCircles (dynamicCircles) where

import Prelude

import Color.Scheme.SixteenAnsi (ColorScheme)
import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (canvas)
import Concur.React.Props (_id, height, width)
import Control.Alt ((<|>))
import Control.Plus (empty)
import Data.Array (filter, (..))
import Data.Foldable (foldMap)
import Data.Int (toNumber)
import Data.Maybe (Maybe(Just, Nothing), maybe)
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
import FRP.Event.Mouse (Mouse, disposeMouse)
import FRP.Event.RelativeMouse (getMouse)
import Graphics.Canvas (getCanvasElementById, getContext2D)
import Graphics.Drawing (Drawing, circle, fillColor, filled, lineWidth, outlineColor, outlined, rectangle, render, scale, translate)
import Unsafe.Coerce (unsafeCoerce)

type Circle = { x ∷ Number, y ∷ Number, size ∷ Number }

scene ∷ ColorScheme → Mouse → { w ∷ Number, h ∷ Number } → Behavior Drawing
scene c mouse { w, h } = pure background <> map renderCircles circles where
  background ∷ Drawing
  background = filled (fillColor c.background) (rectangle 0.0 0.0 w h)

  renderCircles ∷ Array Circle → Drawing
  renderCircles = foldMap renderCircle

  renderCircle ∷ Circle → Drawing
  renderCircle { x, y, size } =
    scale scaleFactor scaleFactor <<< translate x y <<< scale size size $
      outlined
        (outlineColor c.altBackground <> lineWidth ((1.0 + size * 2.0) / scaleFactor))
        (circle 0.0 0.0 0.5)

  circles ∷ Behavior (Array Circle)
  circles = toCircles <$> inPosition mouse <*> swell

  inPosition ∷ Mouse → Behavior (Maybe { x ∷ Int, y ∷ Int })
  inPosition = map (maybe Nothing (\p → if isIn p then Just p else Nothing)) <<< position where
    isIn { x, y } = xn >= lx && yn >= ly && xn <= hx && yn <= hy where
      xn = toNumber x
      yn = toNumber y
    -- Add a slight buffer
    -- so circles don't disappear abruptly
    -- as the mouse leaves
    lx = -buffer
    hx = w + buffer
    ly = -buffer
    hy = h + buffer
    buffer = (2.0 * scaleFactor)

  toCircles ∷ Maybe { x ∷ Int, y ∷ Int } → Number → Array Circle
  toCircles Nothing _ = []
  toCircles (Just pos) sw = filter (\x → x.size > 0.0) do
      i ← 0 .. (numCircles - 1)
      j ← 0 .. (numCircles - 1)
      let x = toNumber i
          y = toNumber j
          d = dist x y pos
          size = (1.0 + sw) / (d + 1.5)
      pure { x
           , y
           , size
           }
    where
      dist x y { x : mx, y : my } =
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
  numCircles = 13

dynamicCircles ∷ ∀ a. ColorScheme → Number → Number → Widget HTML a
dynamicCircles c w h = do
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
          -- canvas is an HTMLElement,
          -- but there is no function to go from
          -- CanvasElement
          -- to HTMLElement
          -- or Element.
          mouse ← getMouse $ unsafeCoerce canvas
          ctx ← getContext2D canvas
          stop ← animate (scene c mouse { w, h }) (render ctx)
          _ ← launchAff $ stopWithoutCanvas idx stop mouse -- Stop when canvas disappears
          pure unit
        Nothing → pure unit

    stopWithoutCanvas ∷ Number → Effect Unit → Mouse → Aff Unit
    stopWithoutCanvas idx stop mouse = do
      delay (fromDuration $ Seconds 2.0)
      mcanvas ← liftEffect $ getCanvasElementById $ show idx
      case mcanvas of
        Just canvas → stopWithoutCanvas idx stop mouse
        Nothing → do
          liftEffect stop
          liftEffect $ disposeMouse mouse
