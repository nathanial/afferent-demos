/-
  Demo Card Helpers - Shared utilities for card-based demo widgets.
-/
import Afferent
import Afferent.Arbor
import Trellis

open Afferent.Arbor
open Trellis (EdgeInsets)

namespace Demos

/-- Font ids used across card-based demos. -/
structure DemoFonts where
  label : FontId
  small : FontId
  medium : FontId
  large : FontId
  huge : FontId

/-- Convert Trellis layout rect to Arbor rect. -/
def layoutRectToRect (r : Trellis.LayoutRect) : Rect :=
  Rect.mk' r.x r.y r.width r.height

/-- Inset a rect by padding on all sides. -/
def insetRect (r : Rect) (pad : Float) : Rect :=
  Rect.mk' (r.origin.x + pad) (r.origin.y + pad)
    (max 1.0 (r.size.width - pad * 2)) (max 1.0 (r.size.height - pad * 2))

/-- Center point of a rect. -/
def rectCenter (r : Rect) : Point :=
  { x := r.origin.x + r.size.width / 2, y := r.origin.y + r.size.height / 2 }

/-- Minimum side length of a rect. -/
def minSide (r : Rect) : Float :=
  min r.size.width r.size.height

/-- Default label color for cards. -/
def cardLabelColor : Color :=
  Afferent.Color.gray 0.85

/-- Create a flexible custom spec with minimum size that expands. -/
def cardSpecFlex (draw : Rect → RenderCommands) : CustomSpec :=
  { measure := fun _ _ => (60, 60)  -- Minimum content size
    collect := fun layout =>
      let rect := layoutRectToRect layout.borderRect
      let pad := min rect.size.width rect.size.height * 0.08
      let inner := insetRect rect pad
      draw inner }

/-- Flexible card style for responsive layout. -/
def cardStyleFlex : BoxStyle :=
  { backgroundColor := some (Afferent.Color.gray 0.15)
    borderColor := some (Afferent.Color.gray 0.35)
    borderWidth := 1
    cornerRadius := 6
    padding := EdgeInsets.uniform 4
    height := .percent 1.0 }

/-- Build a flexible card that fills available space in a grid. -/
def demoCardFlex (labelFont : FontId) (label : String) (draw : Rect → RenderCommands)
    : WidgetBuilder := do
  column (gap := 4) (style := cardStyleFlex) #[
    -- Shape area uses flex-grow to fill remaining space after label
    custom (cardSpecFlex draw) { flexItem := some (Trellis.FlexItem.growing 1) },
    text' label labelFont cardLabelColor .center none
  ]

/-- Create a flexible grid that fills available space using fr units.
    Creates a grid with the specified number of rows and columns,
    where each cell expands equally to fill the container.
    Uses flexItem.grow to fill remaining space when used as a flex child. -/
def gridFlex (rows cols : Nat) (gap : Float := 4) (children : Array WidgetBuilder)
    (padding : EdgeInsets := EdgeInsets.uniform 0) : WidgetBuilder := do
  let rowTemplate := Array.replicate rows (.fr 1)
  let colTemplate := Array.replicate cols (.fr 1)
  let props := Trellis.GridContainer.withTemplate rowTemplate colTemplate gap
  -- Use flexItem.grow to fill remaining space in flex parent (e.g., cellWidget column)
  Afferent.Arbor.gridCustom props {
    flexItem := some (Trellis.FlexItem.growing 1),
    padding := padding
  } children

end Demos
