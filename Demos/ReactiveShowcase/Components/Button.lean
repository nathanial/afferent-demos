/-
  Button Component - Self-contained FRP button with hover state.
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

/-- Build the visual for a button given its state. -/
private def buttonVisual (name : String) (labelText : String) (theme : Theme)
    (variant : ButtonVariant) (state : WidgetState) : WidgetBuilder := do
  let colors := Button.variantColors theme variant
  let bgColor := Button.backgroundColor colors state
  let fgColor := Button.foregroundColor colors state
  let bw := Button.borderWidth variant

  let style : BoxStyle := {
    backgroundColor := some bgColor
    borderColor := if bw > 0 then some colors.border else none
    borderWidth := bw
    cornerRadius := theme.cornerRadius
    padding := EdgeInsets.symmetric theme.padding (theme.padding * 0.6)
  }

  namedCenter name (style := style) do
    text' labelText theme.font fgColor .center

/-- Create a button component using WidgetM.
    Emits the button widget and returns the onClick event. -/
def button (label : String) (theme : Theme) (variant : ButtonVariant := .primary)
    : WidgetM (Event Spider Unit) := do
  let name ← registerComponentW "button"
  let isHovered ← useHover name
  let onClick ← useClick name

  emit do
    let hovered ← isHovered.sample
    let state : WidgetState := { hovered, pressed := false, focused := false }
    pure (buttonVisual name label theme variant state)

  pure onClick

end Demos.ReactiveShowcase.Components
