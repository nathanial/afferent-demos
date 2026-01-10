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

/-- Slider result - events and dynamics. -/
structure SliderResult where
  onChange : Event Spider Float
  value : Dynamic Spider Float

/-- Create a slider component using WidgetM.
    Emits the slider widget and returns value state. -/
def slider (label : Option String) (theme : Theme) (initialValue : Float := 0.5)
    : WidgetM SliderResult := do
  let name ← registerComponentW "slider"
  let isHovered ← useHover name
  let clicks ← useClickData name

  let valueChanges ← Event.mapMaybeM
    (fun data => calculateSliderValue data.click.x data.layouts data.widget name) clicks
  let value ← holdDyn initialValue valueChanges
  let onChange := valueChanges

  emit do
    let hovered ← isHovered.sample
    let v ← value.sample
    let state : WidgetState := { hovered, pressed := false, focused := false }
    pure (sliderVisual name label theme v state)

  pure { onChange, value }

end Demos.ReactiveShowcase.Components
