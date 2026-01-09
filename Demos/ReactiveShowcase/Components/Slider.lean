/-
  Slider Component - Self-contained FRP slider with value tracking.
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

/-- Slider component output - exposes value and render function. -/
structure SliderComponent where
  /-- Event that fires with the new value when changed. -/
  onChange : Event Spider Float
  /-- Current value (0.0 to 1.0) as a Dynamic. -/
  value : Dynamic Spider Float
  /-- Render function that samples state and returns the slider widget. -/
  render : ComponentRender

/-- Create a self-contained slider component.
    The component manages its own hover and value state. -/
def slider (name : String) (label : Option String) (theme : Theme)
    (initialValue : Float)
    : ReactiveM SliderComponent := do
  -- Create internal hover state
  let isHovered ← useHover name

  -- Create internal value state using proper FRP pattern
  let (valueEvent, fireValue) ← liftSpider <| newTriggerEvent (t := Spider) (a := Float)
  let value ← liftSpider <| holdDyn initialValue valueEvent

  -- onChange is the same as valueEvent for external consumers
  let onChange := valueEvent

  -- Wire clicks to update value based on click position
  let clicks ← useClickData name
  let _ ← liftSpider <| SpiderM.liftIO <| clicks.subscribe fun data => do
    match calculateSliderValue data.click.x data.layouts data.widget name with
    | some v => fireValue v
    | none => pure ()

  -- Render function samples state at render time
  let render : ComponentRender := do
    let hovered ← isHovered.sample
    let v ← value.sample
    let state : WidgetState := { hovered, pressed := false, focused := false }
    pure (sliderVisual name label theme v state)

  pure { onChange, value, render }

end Demos.ReactiveShowcase.Components
