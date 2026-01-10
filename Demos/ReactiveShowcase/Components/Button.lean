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

/-- Button component output - exposes onClick event and render function. -/
structure ButtonComponent where
  /-- Event that fires when the button is clicked. -/
  onClick : Event Spider Unit
  /-- Render function that samples hover state and returns the button widget. -/
  render : ComponentRender

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

/-- Create a self-contained button component.
    The component manages its own hover state and exposes an onClick event. -/
def button (label : String) (theme : Theme) (variant : ButtonVariant)
    : ReactiveM ButtonComponent := do
  -- Auto-generate name via registry
  let name ← registerComponent "button"

  -- Internal hover state (via hook that accesses events implicitly)
  let isHovered ← useHover name

  -- Create onClick event (filtered from global clicks)
  let onClick ← useClick name

  -- Render function samples hover state at render time
  let render : ComponentRender := do
    let hovered ← isHovered.sample
    let state : WidgetState := { hovered, pressed := false, focused := false }
    pure (buttonVisual name label theme variant state)

  pure { onClick, render }

end Demos.ReactiveShowcase.Components
