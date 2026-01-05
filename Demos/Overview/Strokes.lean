/-
  Strokes Demo - Cards showing line widths and stroked paths.
-/
import Afferent
import Afferent.Widget
import Afferent.Arbor
import Demos.Overview.Card
import Trellis

open Afferent.Arbor
open Trellis (EdgeInsets)

namespace Demos

/-- Build a straight line path. -/
private def linePath (p1 p2 : Point) : Path :=
  Path.empty.moveTo p1 |>.lineTo p2

/-- Stroke a circle via path. -/
private def strokeCircle (center : Point) (radius : Float) (color : Color) (lineWidth : Float) : RenderCommand :=
  .strokePath (Path.circle center radius) color lineWidth

/-- Stroke an ellipse via path. -/
private def strokeEllipse (center : Point) (rx ry : Float) (color : Color) (lineWidth : Float) : RenderCommand :=
  .strokePath (Path.ellipse center rx ry) color lineWidth

/-- Rect stroke width examples. -/
private def rectWidthCommands (r : Rect) : RenderCommands :=
  let w := r.size.width * 0.18
  let h := r.size.height * 0.45
  let y := r.origin.y + r.size.height * 0.28
  let startX := r.origin.x + r.size.width * 0.08
  let spacing := r.size.width * 0.22
  #[
    .strokeRect (Rect.mk' startX y w h) Afferent.Color.white 1.0,
    .strokeRect (Rect.mk' (startX + spacing) y w h) Afferent.Color.yellow 2.0,
    .strokeRect (Rect.mk' (startX + spacing * 2) y w h) Afferent.Color.cyan 4.0,
    .strokeRect (Rect.mk' (startX + spacing * 3) y w h) Afferent.Color.magenta 8.0
  ]

/-- Circle stroke widths. -/
private def circleWidthCommands (r : Rect) : RenderCommands :=
  let y := r.origin.y + r.size.height * 0.5
  let radius := minSide r * 0.18
  let startX := r.origin.x + r.size.width * 0.2
  let spacing := r.size.width * 0.28
  #[
    strokeCircle ⟨startX, y⟩ radius Afferent.Color.red 2.0,
    strokeCircle ⟨startX + spacing, y⟩ radius Afferent.Color.green 4.0,
    strokeCircle ⟨startX + spacing * 2, y⟩ radius Afferent.Color.blue 6.0
  ]

/-- Horizontal line widths. -/
private def lineWidthCommands (r : Rect) : RenderCommands :=
  let x0 := r.origin.x + r.size.width * 0.1
  let x1 := r.origin.x + r.size.width * 0.9
  let y0 := r.origin.y + r.size.height * 0.25
  let step := r.size.height * 0.15
  #[
    .strokePath (linePath ⟨x0, y0⟩ ⟨x1, y0⟩) Afferent.Color.white 1.0,
    .strokePath (linePath ⟨x0, y0 + step⟩ ⟨x1, y0 + step⟩) Afferent.Color.white 2.0,
    .strokePath (linePath ⟨x0, y0 + step * 2⟩ ⟨x1, y0 + step * 2⟩) Afferent.Color.white 4.0,
    .strokePath (linePath ⟨x0, y0 + step * 3⟩ ⟨x1, y0 + step * 3⟩) Afferent.Color.white 8.0
  ]

/-- Diagonal line examples. -/
private def diagonalCommands (r : Rect) : RenderCommands :=
  let x0 := r.origin.x + r.size.width * 0.15
  let y0 := r.origin.y + r.size.height * 0.25
  let x1 := r.origin.x + r.size.width * 0.85
  let y1 := r.origin.y + r.size.height * 0.75
  #[
    .strokePath (linePath ⟨x0, y0⟩ ⟨x1, y1⟩) Afferent.Color.yellow 2.0,
    .strokePath (linePath ⟨x0 + 10, y0⟩ ⟨x1 + 10, y1⟩) Afferent.Color.cyan 3.0,
    .strokePath (linePath ⟨x0 + 20, y0⟩ ⟨x1 + 20, y1⟩) Afferent.Color.magenta 4.0
  ]

/-- Rounded rectangle strokes. -/
private def roundedRectCommands (r : Rect) : RenderCommands :=
  let w := r.size.width * 0.25
  let h := r.size.height * 0.45
  let y := r.origin.y + r.size.height * 0.28
  let startX := r.origin.x + r.size.width * 0.1
  let spacing := r.size.width * 0.3
  #[
    .strokeRect (Rect.mk' startX y w h) Afferent.Color.orange 3.0 10,
    .strokeRect (Rect.mk' (startX + spacing) y w h) Afferent.Color.green 4.0 20,
    .strokeRect (Rect.mk' (startX + spacing * 2) y w h) Afferent.Color.purple 5.0 30
  ]

/-- Ellipse stroke examples. -/
private def ellipseCommands (r : Rect) : RenderCommands :=
  let y := r.origin.y + r.size.height * 0.5
  let startX := r.origin.x + r.size.width * 0.2
  let spacing := r.size.width * 0.28
  #[
    strokeEllipse ⟨startX, y⟩ (minSide r * 0.3) (minSide r * 0.16) Afferent.Color.red 2.0,
    strokeEllipse ⟨startX + spacing, y⟩ (minSide r * 0.16) (minSide r * 0.28) Afferent.Color.green 3.0,
    strokeEllipse ⟨startX + spacing * 2, y⟩ (minSide r * 0.24) (minSide r * 0.24) Afferent.Color.blue 4.0
  ]

/-- Star stroke examples. -/
private def starCommands (r : Rect) : RenderCommands :=
  let y := r.origin.y + r.size.height * 0.5
  let startX := r.origin.x + r.size.width * 0.22
  let spacing := r.size.width * 0.26
  #[
    .strokePath (Path.star ⟨startX, y⟩ (minSide r * 0.25) (minSide r * 0.12) 5) Afferent.Color.yellow 2.0,
    .strokePath (Path.star ⟨startX + spacing, y⟩ (minSide r * 0.22) (minSide r * 0.1) 6) Afferent.Color.cyan 3.0,
    .strokePath (Path.star ⟨startX + spacing * 2, y⟩ (minSide r * 0.2) (minSide r * 0.09) 8) Afferent.Color.magenta 4.0
  ]

/-- Polygon strokes. -/
private def polygonCommands (r : Rect) : RenderCommands :=
  let y := r.origin.y + r.size.height * 0.5
  let startX := r.origin.x + r.size.width * 0.14
  let spacing := r.size.width * 0.18
  #[
    .strokePath (Path.polygon ⟨startX, y⟩ (minSide r * 0.18) 3) Afferent.Color.red 2.0,
    .strokePath (Path.polygon ⟨startX + spacing, y⟩ (minSide r * 0.18) 4) Afferent.Color.orange 2.0,
    .strokePath (Path.polygon ⟨startX + spacing * 2, y⟩ (minSide r * 0.18) 5) Afferent.Color.yellow 2.0,
    .strokePath (Path.polygon ⟨startX + spacing * 3, y⟩ (minSide r * 0.18) 6) Afferent.Color.green 2.0,
    .strokePath (Path.polygon ⟨startX + spacing * 4, y⟩ (minSide r * 0.18) 8) Afferent.Color.cyan 2.0
  ]

/-- Heart stroke example. -/
private def heartCommands (r : Rect) : RenderCommands :=
  let center := rectCenter r
  #[
    .strokePath (Path.heart center (minSide r * 0.45)) Afferent.Color.red 3.0
  ]

/-- Combined fill + stroke example. -/
private def fillStrokeCommands (r : Rect) : RenderCommands :=
  let center := rectCenter r
  let circleR := minSide r * 0.25
  let rectW := minSide r * 0.45
  let rectH := minSide r * 0.32
  #[
    .fillPath (Path.circle ⟨center.x - rectW * 0.4, center.y⟩ circleR) (Afferent.Color.hsva 0.667 0.75 0.8 1.0),
    .strokePath (Path.circle ⟨center.x - rectW * 0.4, center.y⟩ circleR) Afferent.Color.white 3.0,
    .fillPath (Path.roundedRect (Rect.mk' (center.x + rectW * 0.05) (center.y - rectH / 2) rectW rectH) 12)
      (Afferent.Color.hsva 0.0 0.75 0.8 1.0),
    .strokePath (Path.roundedRect (Rect.mk' (center.x + rectW * 0.05) (center.y - rectH / 2) rectW rectH) 12)
      Afferent.Color.white 2.0
  ]

/-- Zigzag path stroke. -/
private def zigzagCommands (r : Rect) : RenderCommands :=
  let x0 := r.origin.x + r.size.width * 0.1
  let y0 := r.origin.y + r.size.height * 0.65
  let dx := r.size.width * 0.1
  let dy := r.size.height * 0.25
  let zigzag := Path.empty
    |>.moveTo ⟨x0, y0⟩
    |>.lineTo ⟨x0 + dx, y0 - dy⟩
    |>.lineTo ⟨x0 + dx * 2, y0⟩
    |>.lineTo ⟨x0 + dx * 3, y0 - dy⟩
    |>.lineTo ⟨x0 + dx * 4, y0⟩
    |>.lineTo ⟨x0 + dx * 5, y0 - dy⟩
    |>.lineTo ⟨x0 + dx * 6, y0⟩
  #[
    .strokePath zigzag Afferent.Color.yellow 3.0
  ]

/-- Wave path stroke. -/
private def waveCommands (r : Rect) : RenderCommands :=
  let x0 := r.origin.x + r.size.width * 0.1
  let y0 := r.origin.y + r.size.height * 0.6
  let w := r.size.width * 0.8
  let h := r.size.height * 0.25
  let wave := Path.empty
    |>.moveTo ⟨x0, y0⟩
    |>.bezierCurveTo ⟨x0 + w * 0.2, y0 - h⟩ ⟨x0 + w * 0.4, y0 + h⟩ ⟨x0 + w * 0.6, y0⟩
    |>.bezierCurveTo ⟨x0 + w * 0.8, y0 - h⟩ ⟨x0 + w, y0 + h⟩ ⟨x0 + w, y0⟩
  #[
    .strokePath wave Afferent.Color.cyan 4.0
  ]

/-- Spiral-like path stroke. -/
private def spiralCommands (r : Rect) : RenderCommands :=
  let x0 := r.origin.x + r.size.width * 0.15
  let y0 := r.origin.y + r.size.height * 0.55
  let w := r.size.width * 0.7
  let h := r.size.height * 0.3
  let spiral := Path.empty
    |>.moveTo ⟨x0, y0⟩
    |>.quadraticCurveTo ⟨x0 + w * 0.2, y0 - h⟩ ⟨x0 + w * 0.4, y0⟩
    |>.quadraticCurveTo ⟨x0 + w * 0.6, y0 + h⟩ ⟨x0 + w * 0.8, y0⟩
    |>.quadraticCurveTo ⟨x0 + w, y0 - h * 0.6⟩ ⟨x0 + w, y0 + h * 0.4⟩
  #[
    .strokePath spiral Afferent.Color.magenta 3.0
  ]

/-- Arc stroke examples. -/
private def arcCommands (r : Rect) : RenderCommands :=
  let y := r.origin.y + r.size.height * 0.6
  let startX := r.origin.x + r.size.width * 0.2
  let spacing := r.size.width * 0.28
  #[
    .strokePath (Path.arcPath ⟨startX, y⟩ (minSide r * 0.22) 0 Path.pi) Afferent.Color.red 3.0,
    .strokePath (Path.arcPath ⟨startX + spacing, y⟩ (minSide r * 0.22) 0 (Path.pi * 1.5)) Afferent.Color.green 3.0,
    .strokePath (Path.semicircle ⟨startX + spacing * 2, y⟩ (minSide r * 0.22) 0) Afferent.Color.blue 4.0
  ]

/-- Pie slice outlines. -/
private def pieCommands (r : Rect) : RenderCommands :=
  let y := r.origin.y + r.size.height * 0.6
  let startX := r.origin.x + r.size.width * 0.3
  let spacing := r.size.width * 0.35
  #[
    .strokePath (Path.pie ⟨startX, y⟩ (minSide r * 0.22) 0 Path.halfPi) Afferent.Color.yellow 2.0,
    .strokePath (Path.pie ⟨startX + spacing, y⟩ (minSide r * 0.22) (Path.pi * 0.25) (Path.pi * 1.25)) Afferent.Color.cyan 2.0
  ]

/-- Arrow stroke. -/
private def arrowCommands (r : Rect) : RenderCommands :=
  let cx := r.origin.x + r.size.width * 0.5
  let cy := r.origin.y + r.size.height * 0.55
  let arrow := Path.empty
    |>.moveTo ⟨cx - 30, cy - 20⟩
    |>.lineTo ⟨cx + 30, cy⟩
    |>.lineTo ⟨cx - 30, cy + 20⟩
    |>.moveTo ⟨cx - 30, cy⟩
    |>.lineTo ⟨cx + 30, cy⟩
  #[
    .strokePath arrow Afferent.Color.white 3.0
  ]

/-- Cross stroke. -/
private def crossCommands (r : Rect) : RenderCommands :=
  let cx := r.origin.x + r.size.width * 0.5
  let cy := r.origin.y + r.size.height * 0.55
  let cross := Path.empty
    |>.moveTo ⟨cx, cy - 30⟩
    |>.lineTo ⟨cx, cy + 30⟩
    |>.moveTo ⟨cx - 30, cy⟩
    |>.lineTo ⟨cx + 30, cy⟩
  #[
    .strokePath cross Afferent.Color.red 4.0
  ]

/-- Strokes rendered as cards in a grid. -/
def strokesWidget (labelFont : FontId) : WidgetBuilder := do
  let cards : Array (String × (Rect → RenderCommands)) := #[(
    "Rect Widths", rectWidthCommands
  ), (
    "Circle Widths", circleWidthCommands
  ), (
    "Line Widths", lineWidthCommands
  ), (
    "Diagonals", diagonalCommands
  ), (
    "Rounded Rects", roundedRectCommands
  ), (
    "Ellipses", ellipseCommands
  ), (
    "Stars", starCommands
  ), (
    "Polygons", polygonCommands
  ), (
    "Heart", heartCommands
  ), (
    "Fill+Stroke", fillStrokeCommands
  ), (
    "Zigzag", zigzagCommands
  ), (
    "Wave", waveCommands
  ), (
    "Spiral", spiralCommands
  ), (
    "Arcs", arcCommands
  ), (
    "Pie Outlines", pieCommands
  ), (
    "Arrow", arrowCommands
  ), (
    "Cross", crossCommands
  )]
  let widgets := cards.map fun (label, draw) => demoCard labelFont label draw
  grid 4 10 { padding := EdgeInsets.uniform 10 } widgets

/-- Render strokes demo content to canvas using Arbor widgets. -/
def renderStrokesM (reg : Afferent.FontRegistry) (labelFont : FontId) : Afferent.CanvasM Unit := do
  let widget := Afferent.Arbor.build (strokesWidget labelFont)
  Afferent.Widget.renderArborWidget reg widget 1000 800

end Demos
