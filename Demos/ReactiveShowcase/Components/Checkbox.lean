/-
  Checkbox Component - Self-contained FRP checkbox with internal state.
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

/-- Checkbox component output - exposes state and render function. -/
structure CheckboxComponent where
  /-- Event that fires with the new checked state when toggled. -/
  onToggle : Event Spider Bool
  /-- Current checked state as a Dynamic. -/
  isChecked : Dynamic Spider Bool
  /-- Render function that samples state and returns the checkbox widget. -/
  render : ComponentRender

/-- Build the visual for a checkbox given its state. -/
private def checkboxVisual (name : String) (labelText : String) (theme : Theme)
    (checked : Bool) (state : WidgetState) : WidgetBuilder := do
  let colors := theme.input
  let boxSize : Float := 20.0
  let boxBg := if checked then theme.primary.background else colors.background
  let borderColor := if state.focused then colors.borderFocused else colors.border

  let checkboxBox : WidgetBuilder := do
    if checked then
      custom (Checkbox.boxSpec checked state.hovered theme boxSize) {
        minWidth := some boxSize
        minHeight := some boxSize
        cornerRadius := 4
        borderColor := some borderColor
        borderWidth := if state.focused then 2 else 1
        backgroundColor := some boxBg
      }
    else
      box {
        minWidth := some boxSize
        minHeight := some boxSize
        cornerRadius := 4
        borderColor := some borderColor
        borderWidth := if state.focused then 2 else 1
        backgroundColor := some boxBg
      }

  let wid ← freshId
  let props : Trellis.FlexContainer := { Trellis.FlexContainer.row 8 with alignItems := .center }
  let checkBox ← checkboxBox
  let label ← text' labelText theme.font theme.text .left
  pure (.flex wid (some name) props {} #[checkBox, label])

/-- Create a self-contained checkbox component.
    The component manages its own hover and checked state. -/
def checkbox (name : String) (label : String) (theme : Theme)
    (initialChecked : Bool)
    : ReactiveM CheckboxComponent := do
  -- Create internal hover state
  let isHovered ← useHover name

  -- Create internal checked state using proper FRP pattern
  let (isCheckedEvent, fireIsChecked) ← liftSpider <| newTriggerEvent (t := Spider) (a := Bool)
  let isChecked ← liftSpider <| holdDyn initialChecked isCheckedEvent

  -- onToggle is the same as isCheckedEvent for external consumers
  let onToggle := isCheckedEvent

  -- Wire click to toggle checked state
  let clicks ← useClick name
  let _ ← liftSpider <| SpiderM.liftIO <| clicks.subscribe fun _ => do
    let current ← isChecked.sample
    let newValue := !current
    fireIsChecked newValue

  -- Render function samples state at render time
  let render : ComponentRender := do
    let hovered ← isHovered.sample
    let checked ← isChecked.sample
    let state : WidgetState := { hovered, pressed := false, focused := false }
    pure (checkboxVisual name label theme checked state)

  pure { onToggle, isChecked, render }

end Demos.ReactiveShowcase.Components
