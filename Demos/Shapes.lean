/-
  Shapes Demo - Rendered via Arbor widgets using path render commands.
-/
import Afferent
import Afferent.Widget
import Afferent.Arbor
import Trellis
import Demos.Card

open Afferent.Arbor
open Trellis (EdgeInsets)

namespace Demos

structure ShapeDef where
  label : String
  color : Afferent.Arbor.Color
  path : Afferent.Arbor.Rect → Afferent.Arbor.Path
  stroke : Option (Afferent.Arbor.Color × Float) := none

def shapeCommands (shape : ShapeDef) (rect : Rect) : RenderCommands :=
  let path := shape.path rect
  let base : RenderCommands := #[RenderCommand.fillPath path shape.color]
  match shape.stroke with
  | some (strokeColor, strokeWidth) =>
    base.push (RenderCommand.strokePath path strokeColor strokeWidth)
  | none => base

/-- Build a labeled card for a single shape. -/
def shapeCard (labelFont : FontId) (shape : ShapeDef) : WidgetBuilder := do
  demoCard labelFont shape.label (shapeCommands shape)

/-- Shapes rendered as cards in a grid. -/
def shapesWidget (labelFont : FontId) : WidgetBuilder := do
  let shapes : Array ShapeDef := #[
    { label := "Rect (red)", color := Afferent.Color.red, path := fun r => Path.rectangle r },
    { label := "Rect (green)", color := Afferent.Color.green, path := fun r => Path.rectangle r },
    { label := "Rect (blue)", color := Afferent.Color.blue, path := fun r => Path.rectangle r },
    { label := "Circle (yellow)", color := Afferent.Color.yellow, path := fun r => Path.circle (rectCenter r) (minSide r / 2) },
    { label := "Circle (cyan)", color := Afferent.Color.cyan, path := fun r => Path.circle (rectCenter r) (minSide r / 2) },
    { label := "Circle (magenta)", color := Afferent.Color.magenta, path := fun r => Path.circle (rectCenter r) (minSide r / 2) },
    { label := "Rounded Rect", color := Afferent.Color.white, path := fun r => Path.roundedRect r (minSide r * 0.15) },

    { label := "Star (5)", color := Afferent.Color.yellow, path := fun r =>
        Path.star (rectCenter r) (minSide r * 0.5) (minSide r * 0.22) 5 },
    { label := "Star (6)", color := Afferent.Color.orange, path := fun r =>
        Path.star (rectCenter r) (minSide r * 0.48) (minSide r * 0.25) 6 },
    { label := "Star (8)", color := Afferent.Color.red, path := fun r =>
        Path.star (rectCenter r) (minSide r * 0.46) (minSide r * 0.24) 8 },

    { label := "Polygon (3)", color := Afferent.Color.green, path := fun r =>
        Path.polygon (rectCenter r) (minSide r * 0.48) 3 },
    { label := "Polygon (5)", color := Afferent.Color.cyan, path := fun r =>
        Path.polygon (rectCenter r) (minSide r * 0.48) 5 },
    { label := "Polygon (6)", color := Afferent.Color.blue, path := fun r =>
        Path.polygon (rectCenter r) (minSide r * 0.48) 6 },
    { label := "Polygon (8)", color := Afferent.Color.purple, path := fun r =>
        Path.polygon (rectCenter r) (minSide r * 0.48) 8 },

    { label := "Heart (large)", color := Afferent.Color.red, path := fun r =>
        Path.heart (rectCenter r) (minSide r * 0.9) },
    { label := "Heart (small)", color := Afferent.Color.magenta, path := fun r =>
        Path.heart (rectCenter r) (minSide r * 0.7) },
    { label := "Ellipse (wide)", color := Afferent.Color.orange, path := fun r =>
        Path.ellipse (rectCenter r) (r.size.width / 2) (r.size.height * 0.35) },
    { label := "Ellipse (tall)", color := Afferent.Color.green, path := fun r =>
        Path.ellipse (rectCenter r) (r.size.width * 0.35) (r.size.height / 2) },

    { label := "Pie 0-90", color := Afferent.Color.red, path := fun r =>
        Path.pie (rectCenter r) (minSide r * 0.5) 0 (Path.halfPi) },
    { label := "Pie 90-180", color := Afferent.Color.green, path := fun r =>
        Path.pie (rectCenter r) (minSide r * 0.5) (Path.halfPi) (Path.pi) },
    { label := "Pie 180-270", color := Afferent.Color.blue, path := fun r =>
        Path.pie (rectCenter r) (minSide r * 0.5) (Path.pi) (Path.pi * 1.5) },
    { label := "Pie 270-360", color := Afferent.Color.yellow, path := fun r =>
        Path.pie (rectCenter r) (minSide r * 0.5) (Path.pi * 1.5) (Path.twoPi) },

    { label := "Semicircle", color := Afferent.Color.purple, path := fun r =>
        Path.semicircle (rectCenter r) (minSide r * 0.5) 0.0 },

    { label := "Banner", color := Afferent.Color.cyan, path := fun r =>
        let x := r.origin.x
        let y := r.origin.y
        let w := r.size.width
        let h := r.size.height
        let top := y + h * 0.2
        let bottom := y + h * 0.8
        let mid := y + h * 0.5
        Path.empty
          |>.moveTo ⟨x, top⟩
          |>.lineTo ⟨x + w, top⟩
          |>.quadraticCurveTo ⟨x + w * 1.05, mid⟩ ⟨x + w, bottom⟩
          |>.lineTo ⟨x, bottom⟩
          |>.quadraticCurveTo ⟨x - w * 0.05, mid⟩ ⟨x, top⟩
          |>.closePath },

    { label := "Teardrop", color := Afferent.Color.orange, path := fun r =>
        let x := r.origin.x
        let y := r.origin.y
        let w := r.size.width
        let h := r.size.height
        let cx := x + w / 2
        let top := y + h * 0.1
        let bottom := y + h * 0.9
        Path.empty
          |>.moveTo ⟨cx, top⟩
          |>.bezierCurveTo ⟨x + w * 0.95, y + h * 0.25⟩ ⟨x + w * 0.85, bottom⟩ ⟨cx, bottom⟩
          |>.bezierCurveTo ⟨x + w * 0.15, bottom⟩ ⟨x + w * 0.05, y + h * 0.25⟩ ⟨cx, top⟩
          |>.closePath },

    { label := "Arc Wedge", color := Afferent.Color.green, path := fun r =>
        Path.arcPath (rectCenter r) (minSide r * 0.5) 0 (Path.pi * 1.5) |>.closePath },

    { label := "Rounded (small)", color := Afferent.Color.red, path := fun r => Path.roundedRect r (minSide r * 0.06) },
    { label := "Rounded (large)", color := Afferent.Color.blue, path := fun r => Path.roundedRect r (minSide r * 0.35) },

    { label := "Triangle", color := Afferent.Color.yellow, path := fun r =>
        let x := r.origin.x
        let y := r.origin.y
        let w := r.size.width
        let h := r.size.height
        Path.triangle ⟨x + w * 0.5, y⟩ ⟨x + w, y + h⟩ ⟨x, y + h⟩ },

    { label := "Equilateral (L)", color := Afferent.Color.green, path := fun r =>
        Path.equilateralTriangle (rectCenter r) (minSide r * 0.55) },
    { label := "Equilateral (S)", color := Afferent.Color.cyan, path := fun r =>
        Path.equilateralTriangle (rectCenter r) (minSide r * 0.4) },

    { label := "Speech Bubble", color := Afferent.Color.white, path := fun r =>
        let x := r.origin.x
        let y := r.origin.y
        let w := r.size.width
        let h := r.size.height
        let tailW := w * 0.2
        let tailH := h * 0.18
        let tailX := x + w * 0.55
        let tailY := y + h * 0.75
        Path.empty
          |>.moveTo ⟨x + w * 0.1, y + h * 0.05⟩
          |>.lineTo ⟨x + w * 0.9, y + h * 0.05⟩
          |>.bezierCurveTo ⟨x + w, y + h * 0.05⟩ ⟨x + w, y + h * 0.25⟩ ⟨x + w, y + h * 0.35⟩
          |>.lineTo ⟨x + w, y + h * 0.65⟩
          |>.bezierCurveTo ⟨x + w, y + h * 0.75⟩ ⟨x + w * 0.9, y + h * 0.75⟩ ⟨x + w * 0.8, y + h * 0.75⟩
          |>.lineTo ⟨tailX + tailW * 0.1, tailY⟩
          |>.lineTo ⟨tailX, tailY + tailH⟩
          |>.lineTo ⟨tailX - tailW * 0.1, tailY⟩
          |>.lineTo ⟨x + w * 0.2, y + h * 0.75⟩
          |>.bezierCurveTo ⟨x + w * 0.1, y + h * 0.75⟩ ⟨x, y + h * 0.65⟩ ⟨x, y + h * 0.55⟩
          |>.lineTo ⟨x, y + h * 0.35⟩
          |>.bezierCurveTo ⟨x, y + h * 0.25⟩ ⟨x + w * 0.1, y + h * 0.05⟩ ⟨x + w * 0.1, y + h * 0.05⟩
          |>.closePath },

    { label := "Diamond", color := Afferent.Color.cyan, path := fun r =>
        let x := r.origin.x
        let y := r.origin.y
        let w := r.size.width
        let h := r.size.height
        Path.empty
          |>.moveTo ⟨x + w * 0.5, y⟩
          |>.lineTo ⟨x + w, y + h * 0.5⟩
          |>.lineTo ⟨x + w * 0.5, y + h⟩
          |>.lineTo ⟨x, y + h * 0.5⟩
          |>.closePath }
  ]

  let cards := shapes.map (shapeCard labelFont)
  grid 6 10 { padding := EdgeInsets.uniform 10 } cards

/-- Render shapes demo content to canvas using Arbor widgets. -/
def renderShapesM (reg : Afferent.FontRegistry) (labelFont : FontId) : Afferent.CanvasM Unit := do
  let widget := Afferent.Arbor.build (shapesWidget labelFont)
  Afferent.Widget.renderArborWidget reg widget 1000 800

end Demos
