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

/-- Checkbox result - events and dynamics. -/
structure CheckboxResult where
  onToggle : Event Spider Bool
  isChecked : Dynamic Spider Bool

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

/-- Create a checkbox component using WidgetM.
    Emits the checkbox widget and returns toggle state. -/
def checkbox (label : String) (theme : Theme) (initialChecked : Bool := false)
    : WidgetM CheckboxResult := do
  let name ← registerComponentW "checkbox"
  let isHovered ← useHover name
  let clicks ← useClick name
  let isChecked ← foldDyn (fun _ checked => !checked) initialChecked clicks
  let onToggle := isChecked.updated

  emit do
    let hovered ← isHovered.sample
    let checked ← isChecked.sample
    let state : WidgetState := { hovered, pressed := false, focused := false }
    pure (checkboxVisual name label theme checked state)

  pure { onToggle, isChecked }

end Demos.ReactiveShowcase.Components
