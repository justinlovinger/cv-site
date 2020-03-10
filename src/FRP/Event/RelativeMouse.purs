-- | A modified FRP.Event.Mouse for relative position
module FRP.Event.RelativeMouse where

import Prelude

import Data.Foldable (length, sum, traverse_)
import Data.Int (round)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..), fromJust)
import Data.Newtype (wrap)
import Data.Set as Set
import Effect (Effect)
import Effect.Ref as Ref
import FRP.Event.Mouse (Mouse)
import Partial.Unsafe (unsafePartial)
import Unsafe.Coerce (unsafeCoerce)
import Web.Event.EventTarget (addEventListener, eventListener, removeEventListener)
import Web.HTML (window)
import Web.HTML.HTMLElement (HTMLElement, getBoundingClientRect)
import Web.HTML.Window (toEventTarget)
import Web.TouchEvent.Touch as T
import Web.TouchEvent.TouchEvent as TE
import Web.TouchEvent.TouchList as TL
import Web.UIEvent.MouseEvent as ME

-- | Get a handle for working with the mouse,
-- | relative to the given element.
getMouse :: HTMLElement -> Effect Mouse
getMouse el = do
    position <- Ref.new Nothing
    buttons <- Ref.new Set.empty
    target <- toEventTarget <$> window

    -- Mouse
    mouseMoveListener <- eventListener \e -> do
      ME.fromEvent e # traverse_ \me -> do
        elRect <- getBoundingClientRect el
        Ref.write (Just { x: (ME.clientX me) - (round elRect.left)
                        , y: (ME.clientY me) - (round elRect.top)
                        })
                  position
    mouseDownListener <- eventListener \e -> do
      ME.fromEvent e # traverse_ \me ->
        Ref.modify (Set.insert (ME.button me)) buttons
    mouseUpListener <- eventListener \e -> do
      ME.fromEvent e # traverse_ \me ->
        Ref.modify (Set.delete (ME.button me)) buttons
    addEventListener (wrap "mousemove") mouseMoveListener false target
    addEventListener (wrap "mousedown") mouseDownListener false target
    addEventListener (wrap "mouseup") mouseUpListener false target

    -- Touch
    touchMoveListener <- eventListener \e -> do
      TE.fromEvent e # traverse_ \te -> do
        let avgX = (sum $ map T.clientX tl) / length tl
            avgY = (sum $ map T.clientY tl) / length tl
            tl = toList $ TE.touches te
        elRect <- getBoundingClientRect el
        Ref.write (Just { x: avgX - (round elRect.left)
                        , y: avgY - (round elRect.top)
                        })
                  position
    touchDownListener <- eventListener \e -> do
      TE.fromEvent e # traverse_ \te ->
        Ref.modify (Set.insert leftClick) buttons
    touchUpListener <- eventListener \e -> do
      TE.fromEvent e # traverse_ \te ->
        -- Only trigger mouse up when no touches
        if TL.length (TE.touches te) == 0 then
          Ref.modify (Set.delete leftClick) buttons
        else
          Ref.read buttons
    addEventListener (wrap "touchmove") touchMoveListener false target
    addEventListener (wrap "touchstart") touchMoveListener false target
    addEventListener (wrap "touchstart") touchDownListener false target
    addEventListener (wrap "touchend") touchUpListener false target
    addEventListener (wrap "touchcancel") touchUpListener false target

    -- Cleanup
    let dispose = do
          removeEventListener (wrap "mousemove") mouseMoveListener false target
          removeEventListener (wrap "mousedown") mouseDownListener false target
          removeEventListener (wrap "mouseup") mouseUpListener false target

          removeEventListener (wrap "touchmove") touchMoveListener false target
          removeEventListener (wrap "touchstart") touchMoveListener false target
          removeEventListener (wrap "touchstart") touchDownListener false target
          removeEventListener (wrap "touchend") touchUpListener false target
          removeEventListener (wrap "touchcancel") touchUpListener false target

    pure (unsafeCoerce { position, buttons, dispose })
  where
    toList ∷ TL.TouchList → List T.Touch
    toList tl = toList' tl ((TL.length tl) - 1)
    toList' tl i = if i >= 0 then
                     (unsafePartial fromJust $ TL.item i tl) : toList' tl (i - 1)
                   else
                     Nil

    leftClick = 0 -- See https://developer.mozilla.org/en-US/docs/Web/API/MouseEvent/button
