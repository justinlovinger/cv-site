-- | A modified FRP.Event.Mouse for relative position
module FRP.Event.RelativeMouse where

import Prelude

import Data.Foldable (traverse_)
import Data.Int (round)
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Data.Set as Set
import Effect (Effect)
import Effect.Ref as Ref
import FRP.Event.Mouse (Mouse)
import Unsafe.Coerce (unsafeCoerce)
import Web.Event.EventTarget (addEventListener, eventListener, removeEventListener)
import Web.HTML (window)
import Web.HTML.HTMLElement (HTMLElement, getBoundingClientRect)
import Web.HTML.Window (toEventTarget)
import Web.UIEvent.MouseEvent (button, clientX, clientY, fromEvent)

-- | Get a handle for working with the mouse,
-- | relative to the given element.
getMouse :: HTMLElement -> Effect Mouse
getMouse el = do
  position <- Ref.new Nothing
  buttons <- Ref.new Set.empty
  target <- toEventTarget <$> window
  mouseMoveListener <- eventListener \e -> do
    fromEvent e # traverse_ \me -> do
      elRect <- getBoundingClientRect el
      Ref.write (Just { x: (clientX me) - (round elRect.left)
                      , y: (clientY me) - (round elRect.top)
                      })
                position
  mouseDownListener <- eventListener \e -> do
    fromEvent e # traverse_ \me ->
      Ref.modify (Set.insert (button me)) buttons
  mouseUpListener <- eventListener \e -> do
    fromEvent e # traverse_ \me ->
      Ref.modify (Set.delete (button me)) buttons
  addEventListener (wrap "mousemove") mouseMoveListener false target
  addEventListener (wrap "mousedown") mouseDownListener false target
  addEventListener (wrap "mouseup") mouseUpListener false target
  let dispose = do
        removeEventListener (wrap "mousemove") mouseMoveListener false target
        removeEventListener (wrap "mousedown") mouseDownListener false target
        removeEventListener (wrap "mouseup") mouseUpListener false target
  pure (unsafeCoerce { position, buttons, dispose })
