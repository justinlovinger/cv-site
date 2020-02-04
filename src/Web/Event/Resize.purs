module Web.Event.Resize (waitForResize) where

import Prelude

import Effect.AVar (empty, tryPut)
import Effect.Aff (Aff)
import Effect.Aff.AVar (take)
import Effect.Class (liftEffect)
import Web.Event.Event (EventType(EventType))
import Web.Event.EventTarget (addEventListener, eventListener, removeEventListener)
import Web.HTML (window)
import Web.HTML.Window (toEventTarget)

waitForResize ∷ Aff Unit
waitForResize = do
  var ← liftEffect empty
  reloadListener ← liftEffect $ eventListener $ \_ → tryPut true var
  _window ← liftEffect window
  let
    reloadEvent = EventType "resize"
    windowTarget = toEventTarget _window
  _ ← liftEffect $ addEventListener reloadEvent reloadListener false windowTarget
  _ ← take var
  liftEffect $ removeEventListener reloadEvent reloadListener false windowTarget
