/-
  Line Caps Demo
  Demonstrates butt, round, and square line cap styles.
-/
import Afferent

open Afferent CanvasM

namespace Demos

/-- Render line caps demo showing butt, round, and square cap styles. -/
def renderLineCapsM (font : Font) : CanvasM Unit := do
  -- Title
  setFillColor Color.white
  fillTextXY "Line Caps" 80 60 font

  -- Layout
  let x1 := 100.0
  let x2 := 450.0
  let lineWidth := 20.0
  let spacing := 80.0

  -- Butt cap (default)
  let y1 := 140.0
  setStrokeColor Color.red
  setLineWidth lineWidth
  setLineCap .butt
  setSolid  -- Ensure solid line
  drawLine ⟨x1, y1⟩ ⟨x2, y1⟩
  setFillColor Color.white
  fillTextXY "Butt (default)" (x2 + 30) (y1 + 6) font

  -- Round cap
  let y2 := y1 + spacing
  setStrokeColor Color.green
  setLineCap .round
  drawLine ⟨x1, y2⟩ ⟨x2, y2⟩
  fillTextXY "Round" (x2 + 30) (y2 + 6) font

  -- Square cap
  let y3 := y2 + spacing
  setStrokeColor Color.blue
  setLineCap .square
  drawLine ⟨x1, y3⟩ ⟨x2, y3⟩
  fillTextXY "Square" (x2 + 30) (y3 + 6) font

  -- Reference lines showing exact endpoints
  setStrokeColor (Color.white.withAlpha 0.4)
  setLineWidth 1
  setLineCap .butt
  drawLine ⟨x1, y1 - 40⟩ ⟨x1, y3 + 40⟩
  drawLine ⟨x2, y1 - 40⟩ ⟨x2, y3 + 40⟩

  -- Explanation text
  setFillColor (Color.white.withAlpha 0.7)
  fillTextXY "Vertical lines show exact endpoints" 100 (y3 + 70) font

  -- Line Joins section
  let joinY := y3 + 140
  setFillColor Color.white
  fillTextXY "Line Joins" 80 joinY font

  -- Create a corner path
  let cornerPath := fun (x y : Float) =>
    Path.empty |>.moveTo ⟨x, y + 50⟩ |>.lineTo ⟨x + 50, y⟩ |>.lineTo ⟨x + 100, y + 50⟩

  let joinX1 := 100.0
  let joinX2 := 250.0
  let joinX3 := 400.0
  let joinY1 := joinY + 50

  -- Miter join
  setStrokeColor Color.cyan
  setLineWidth 15
  setLineJoin .miter
  setLineCap .butt
  strokePath (cornerPath joinX1 joinY1)
  setFillColor Color.white
  fillTextXY "Miter" (joinX1 + 25) (joinY1 + 90) font

  -- Round join
  setStrokeColor Color.yellow
  setLineJoin .round
  strokePath (cornerPath joinX2 joinY1)
  fillTextXY "Round" (joinX2 + 25) (joinY1 + 90) font

  -- Bevel join
  setStrokeColor Color.magenta
  setLineJoin .bevel
  strokePath (cornerPath joinX3 joinY1)
  fillTextXY "Bevel" (joinX3 + 25) (joinY1 + 90) font

def renderLineCapsDemoFrame (c : Canvas) (screenScale : Float) (fontSmall fontMedium : Font)
    : IO Canvas := do
  run' c do
    resetTransform
    scale screenScale screenScale
    renderLineCapsM fontSmall
    setFillColor Color.white
    fillTextXY "Line Caps & Joins (Space to advance)" 20 30 fontMedium

end Demos
