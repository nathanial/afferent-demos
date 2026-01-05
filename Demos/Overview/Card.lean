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

/-- Shared card style for demo widgets. -/
def cardStyle (cardWidth cardHeight : Float) : BoxStyle :=
  {
    backgroundColor := some (Afferent.Color.gray 0.15)
    borderColor := some (Afferent.Color.gray 0.35)
    borderWidth := 1
    cornerRadius := 10
    padding := EdgeInsets.uniform 8
    minWidth := some cardWidth
    minHeight := some cardHeight
  }

/-- Default label color for cards. -/
def cardLabelColor : Color :=
  Afferent.Color.gray 0.85

/-- Create a custom spec that draws within a fixed square area. -/
def cardSpec (draw : Rect → RenderCommands) (size : Float) : CustomSpec :=
  { measure := fun _ _ => (size, size)
    collect := fun layout =>
      let rect := layoutRectToRect layout.borderRect
      let pad := min rect.size.width rect.size.height * 0.12
      let inner := insetRect rect pad
      draw inner }

/-- Build a labeled card with a custom drawing spec. -/
def demoCard (labelFont : FontId) (label : String) (draw : Rect → RenderCommands)
    (contentSize : Float := 90) (cardWidth : Float := 140) (cardHeight : Float := 120)
    : WidgetBuilder := do
  let labelMaxWidth : Float := cardWidth - 16.0
  column (gap := 6) (style := cardStyle cardWidth cardHeight) #[
    custom (cardSpec draw contentSize) { minWidth := some contentSize, minHeight := some contentSize },
    text' label labelFont cardLabelColor .center (some labelMaxWidth)
  ]

end Demos
