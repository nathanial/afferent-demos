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
def switch (label : Option String) (theme : Theme) (initialOn : Bool)
    : ReactiveM SwitchComponent := do
  -- Auto-generate name via registry
  let events ← getEvents
  let name ← liftSpider <| SpiderM.liftIO <| events.registry.register "switch"

  -- Create internal hover state
  let isHovered ← useHover name

  -- Get event streams
  let clicks ← useClick name
  let animFrames ← useAnimationFrame

  -- Pure FRP: foldDyn toggles state on each click
  let isOn ← liftSpider <| foldDyn (fun _ on => !on) initialOn clicks
  let onToggle := isOn.updated

  -- Animation requires fixDynM for self-referential state:
  -- animProgress depends on animFrames AND its own current value
  let initialAnim := if initialOn then 1.0 else 0.0
  let animProgress ← liftSpider <| SpiderM.fixDynM fun animBehavior => do
    -- Attach current animation value AND isOn state to each frame
    let updateEvent ← Event.attachWithM
      (fun (anim, on) dt =>
        let animSpeed := 8.0
        let rawFactor := animSpeed * dt
        let lerpFactor := if rawFactor > 1.0 then 1.0 else rawFactor
        let target := if on then 1.0 else 0.0
        let diff := target - anim
        if diff.abs < 0.01 then target else anim + diff * lerpFactor)
      (Behavior.zipWith Prod.mk animBehavior isOn.current)
      animFrames
    holdDyn initialAnim updateEvent

  -- Render function samples state at render time
  let render : ComponentRender := do
    let hovered ← isHovered.sample
    let anim ← animProgress.sample
    let state : WidgetState := { hovered, pressed := false, focused := false }
    pure (animatedSwitchVisual name label theme anim state)

  pure { onToggle, isOn, animProgress, render }

end Demos.ReactiveShowcase.Components
