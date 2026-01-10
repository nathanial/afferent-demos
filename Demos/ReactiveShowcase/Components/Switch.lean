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

/-- Switch result - events and dynamics. -/
structure SwitchResult where
  onToggle : Event Spider Bool
  isOn : Dynamic Spider Bool
  animProgress : Dynamic Spider Float

/-- Create a switch component using WidgetM with animation.
    Emits the switch widget and returns toggle state. -/
def switch (label : Option String) (theme : Theme) (initialOn : Bool := false)
    : WidgetM SwitchResult := do
  let name ← registerComponentW "switch"
  let isHovered ← useHover name
  let clicks ← useClick name
  let animFrames ← useAnimationFrame

  let isOn ← foldDyn (fun _ on => !on) initialOn clicks
  let onToggle := isOn.updated

  let initialAnim := if initialOn then 1.0 else 0.0
  let animProgress ← SpiderM.fixDynM fun animBehavior => do
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

  emit do
    let hovered ← isHovered.sample
    let anim ← animProgress.sample
    let state : WidgetState := { hovered, pressed := false, focused := false }
    pure (animatedSwitchVisual name label theme anim state)

  pure { onToggle, isOn, animProgress }

end Demos.ReactiveShowcase.Components
