/-
  Dashed Lines Demo
  Demonstrates various dash patterns for stroked lines.
-/
import Afferent
import Afferent.Widget
import Afferent.Arbor
import Demos.Demo
import Trellis

open Afferent CanvasM

namespace Demos

/-- Render dashed lines demo showing various dash patterns. -/
def renderDashedLinesM (font : Font) : CanvasM Unit := do
  -- Title
  setFillColor Color.white
  fillTextXY "Dashed & Dotted Lines" 80 60 font

  let x1 := 100.0
  let x2 := 500.0
  let lineWidth := 4.0
  let spacing := 50.0

  -- Solid (reference)
  let y1 := 120.0
  setStrokeColor Color.white
  setLineWidth lineWidth
  setSolid
  drawLine ⟨x1, y1⟩ ⟨x2, y1⟩
  setFillColor Color.white
  fillTextXY "Solid" (x2 + 20) (y1 + 4) font

  -- Short dashes
  let y2 := y1 + spacing
  setStrokeColor Color.cyan
  setDashed 10 5
  drawLine ⟨x1, y2⟩ ⟨x2, y2⟩
  fillTextXY "Dashed (10, 5)" (x2 + 20) (y2 + 4) font

  -- Long dashes
  let y3 := y2 + spacing
  setStrokeColor Color.yellow
  setDashed 20 10
  drawLine ⟨x1, y3⟩ ⟨x2, y3⟩
  fillTextXY "Dashed (20, 10)" (x2 + 20) (y3 + 4) font

  -- Dotted
  let y4 := y3 + spacing
  setStrokeColor Color.magenta
  setDotted
  drawLine ⟨x1, y4⟩ ⟨x2, y4⟩
  fillTextXY "Dotted" (x2 + 20) (y4 + 4) font

  -- Dash-dot pattern
  let y5 := y4 + spacing
  setStrokeColor Color.orange
  setDashPattern (some ⟨#[15, 5, 3, 5], 0⟩)
  setLineCap .butt
  drawLine ⟨x1, y5⟩ ⟨x2, y5⟩
  setFillColor Color.white
  fillTextXY "Dash-dot" (x2 + 20) (y5 + 4) font

  -- Dense dashes
  let y6 := y5 + spacing
  setStrokeColor Color.green
  setDashed 5 5
  drawLine ⟨x1, y6⟩ ⟨x2, y6⟩
  fillTextXY "Dense (5, 5)" (x2 + 20) (y6 + 4) font

  -- Dashed shapes section
  let shapeY := y6 + 70
  setFillColor Color.white
  fillTextXY "Dashed Shapes" 80 shapeY font

  -- Dashed rectangle
  setStrokeColor Color.cyan
  setLineWidth 3
  setDashed 8 4
  setLineCap .butt
  strokeRectXYWH 100 (shapeY + 30) 120 80
  setFillColor (Color.white.withAlpha 0.7)
  fillTextXY "Rectangle" 120 (shapeY + 130) font

  -- Dashed circle
  setStrokeColor Color.yellow
  strokeCircle ⟨350, shapeY + 70⟩ 50
  fillTextXY "Circle" 325 (shapeY + 130) font

  -- Dashed rounded rect
  setStrokeColor Color.magenta
  setDashed 10 6
  strokeRoundedRect (Rect.mk' 480 (shapeY + 30) 120 80) 15
  fillTextXY "Rounded" 510 (shapeY + 130) font

def dashedLinesWidget (screenScale : Float) (fontSmall fontMedium : Font) : Afferent.Arbor.WidgetBuilder := do
  Afferent.Arbor.custom (spec := {
    measure := fun _ _ => (0, 0)
    collect := fun _ => #[]
    draw := some (fun layout => do
      withContentRect layout fun _ _ => do
        resetTransform
        scale screenScale screenScale
        renderDashedLinesM fontSmall
        setFillColor Color.white
        fillTextXY "Dashed Lines (Space to advance)" 20 30 fontMedium
    )
  }) (style := { flexItem := some (Trellis.FlexItem.growing 1) })

end Demos
