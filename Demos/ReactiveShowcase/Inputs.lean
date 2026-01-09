/-
  ReactiveShowcase - Reactive Input Infrastructure
  Creates trigger events that the demo loop fires when FFI events occur.
-/
import Reactive
import Demos.ReactiveShowcase.Types

open Reactive Reactive.Host

namespace Demos.ReactiveShowcase

/-- Trigger functions to fire from the demo loop when FFI events occur. -/
structure ReactiveInputs where
  /-- Fire when a click event occurs. -/
  fireClick : ClickData → IO Unit
  /-- Fire when mouse position changes (hover). -/
  fireHover : HoverData → IO Unit
  /-- Fire when a key is pressed. -/
  fireKey : KeyData → IO Unit
  /-- Fire each frame with delta time (for animations). -/
  fireAnimationFrame : Float → IO Unit

/-- Global reactive event streams that widgets subscribe to. -/
structure ReactiveEvents where
  /-- Click events with layout context. -/
  clickEvent : Event Spider ClickData
  /-- Hover events with position and layout context. -/
  hoverEvent : Event Spider HoverData
  /-- Keyboard events. -/
  keyEvent : Event Spider KeyData
  /-- Animation frame events (fires each frame with dt). -/
  animationFrame : Event Spider Float

/-- Create the reactive input infrastructure.
    Returns both the event streams (for subscriptions) and triggers (for firing). -/
def createInputs : SpiderM (ReactiveEvents × ReactiveInputs) := do
  let (clickEvent, fireClick) ← newTriggerEvent (t := Spider) (a := ClickData)
  let (hoverEvent, fireHover) ← newTriggerEvent (t := Spider) (a := HoverData)
  let (keyEvent, fireKey) ← newTriggerEvent (t := Spider) (a := KeyData)
  let (animFrame, fireAnimFrame) ← newTriggerEvent (t := Spider) (a := Float)

  let events : ReactiveEvents := {
    clickEvent := clickEvent
    hoverEvent := hoverEvent
    keyEvent := keyEvent
    animationFrame := animFrame
  }
  let inputs : ReactiveInputs := {
    fireClick := fireClick
    fireHover := fireHover
    fireKey := fireKey
    fireAnimationFrame := fireAnimFrame
  }
  pure (events, inputs)

end Demos.ReactiveShowcase
