/-
  Switch Component - Self-contained FRP toggle switch with animation.
-/
import Reactive
import Afferent
import Afferent.Canopy
import Demos.ReactiveShowcase.Types
import Demos.ReactiveShowcase.Component

open Reactive Reactive.Host
open Afferent CanvasM
open Afferent.Arbor
open Afferent.Canopy
open Trellis

namespace Demos.ReactiveShowcase.Components

/-- Switch component output - exposes state and render function. -/
structure SwitchComponent where
  /-- Event that fires with the new on/off state when toggled. -/
  onToggle : Event Spider Bool
  /-- Current on/off state as a Dynamic. -/
  isOn : Dynamic Spider Bool
  /-- Animation progress (0.0 = off, 1.0 = on) as a Dynamic. -/
  animProgress : Dynamic Spider Float
  /-- Render function that samples state and returns the switch widget. -/
  render : ComponentRender

/-- Create a self-contained switch component with animation.
    The component manages its own hover, on/off state, and animation progress. -/
def switch (name : String) (label : Option String) (theme : Theme)
    (initialOn : Bool)
    : ReactiveM SwitchComponent := do
  -- Create internal hover state
  let isHovered ← useHover name

  -- Create internal on/off state using proper FRP pattern
  let (isOnEvent, fireIsOn) ← liftSpider <| newTriggerEvent (t := Spider) (a := Bool)
  let isOn ← liftSpider <| holdDyn initialOn isOnEvent

  -- Create animation progress state (starts at target position)
  let initialAnim := if initialOn then 1.0 else 0.0
  let (animProgressEvent, fireAnimProgress) ← liftSpider <| newTriggerEvent (t := Spider) (a := Float)
  let animProgress ← liftSpider <| holdDyn initialAnim animProgressEvent

  -- Create onToggle event (same as isOnEvent for external consumers)
  let onToggle := isOnEvent

  -- Wire click to toggle state
  let clicks ← useClick name
  let _ ← liftSpider <| SpiderM.liftIO <| clicks.subscribe fun _ => do
    let current ← isOn.sample
    let newValue := !current
    fireIsOn newValue

  -- Wire animation frame to update animation progress
  let animFrames ← useAnimationFrame
  let _ ← liftSpider <| SpiderM.liftIO <| animFrames.subscribe fun dt => do
    let animSpeed := 8.0
    let rawFactor := animSpeed * dt
    let lerpFactor := if rawFactor > 1.0 then 1.0 else rawFactor

    let on ← isOn.sample
    let anim ← animProgress.sample
    let target := if on then 1.0 else 0.0
    let diff := target - anim
    let newAnim := if diff.abs < 0.01 then target else anim + diff * lerpFactor
    fireAnimProgress newAnim

  -- Render function samples state at render time
  let render : ComponentRender := do
    let hovered ← isHovered.sample
    let anim ← animProgress.sample
    let state : WidgetState := { hovered, pressed := false, focused := false }
    pure (animatedSwitchVisual name label theme anim state)

  pure { onToggle, isOn, animProgress, render }

end Demos.ReactiveShowcase.Components
