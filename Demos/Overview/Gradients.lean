/-
  Gradients Demo - Cards showing linear and radial gradients.
-/
import Afferent
import Afferent.Widget
import Afferent.Arbor
import Demos.Overview.Card
import Trellis

open Afferent.Arbor
open Trellis (EdgeInsets)

namespace Demos

/-- Fill a rect with a linear gradient style. -/
private def linearRect (r : Rect) (start finish : Afferent.Point) (stops : Array Afferent.GradientStop)
    (cornerRadius : Float := 8) : RenderCommands :=
  let style := Afferent.FillStyle.linearGradient start finish stops
  #[.fillRectStyle r style cornerRadius]

/-- Fill a shape path with a gradient style. -/
private def fillPathStyle (path : Path) (style : Afferent.FillStyle) : RenderCommands :=
  #[.fillPathStyle path style]

/-- Linear gradient horizontal. -/
private def linearHorizontal (colors : Array Color) : Rect → RenderCommands := fun r =>
  let start := Afferent.Point.mk' r.origin.x (r.origin.y + r.size.height / 2)
  let finish := Afferent.Point.mk' (r.origin.x + r.size.width) (r.origin.y + r.size.height / 2)
  linearRect r start finish (Afferent.GradientStop.distribute colors)

/-- Linear gradient vertical. -/
private def linearVertical (colors : Array Color) : Rect → RenderCommands := fun r =>
  let start := Afferent.Point.mk' (r.origin.x + r.size.width / 2) r.origin.y
  let finish := Afferent.Point.mk' (r.origin.x + r.size.width / 2) (r.origin.y + r.size.height)
  linearRect r start finish (Afferent.GradientStop.distribute colors)

/-- Diagonal linear gradient. -/
private def linearDiagonal (colors : Array Color) : Rect → RenderCommands := fun r =>
  let start := Afferent.Point.mk' r.origin.x r.origin.y
  let finish := Afferent.Point.mk' (r.origin.x + r.size.width) (r.origin.y + r.size.height)
  linearRect r start finish (Afferent.GradientStop.distribute colors)

/-- Radial gradient circle. -/
private def radialCircle (colors : Array Color) : Rect → RenderCommands := fun r =>
  let center := Afferent.Point.mk' (r.origin.x + r.size.width / 2) (r.origin.y + r.size.height / 2)
  let radius := minSide r * 0.45
  let style := Afferent.FillStyle.radialGradient center radius (Afferent.GradientStop.distribute colors)
  fillPathStyle (Path.circle ⟨center.x, center.y⟩ radius) style

/-- Radial gradient ellipse. -/
private def radialEllipse (colors : Array Color) : Rect → RenderCommands := fun r =>
  let center := Afferent.Point.mk' (r.origin.x + r.size.width / 2) (r.origin.y + r.size.height / 2)
  let rx := r.size.width * 0.45
  let ry := r.size.height * 0.32
  let style := Afferent.FillStyle.radialGradient center (min rx ry) (Afferent.GradientStop.distribute colors)
  fillPathStyle (Path.ellipse ⟨center.x, center.y⟩ rx ry) style

/-- Gradient cards rendered as widgets. -/
def gradientsWidget (labelFont : FontId) : WidgetBuilder := do
  let sunset : Array Afferent.GradientStop := #[(
    { position := 0.0, color := Afferent.Color.hsva 0.667 0.667 0.3 1.0 }
  ), (
    { position := 0.3, color := Afferent.Color.hsva 0.833 0.6 0.5 1.0 }
  ), (
    { position := 0.5, color := Afferent.Color.hsva 0.024 0.778 0.9 1.0 }
  ), (
    { position := 0.7, color := Afferent.Color.hsva 0.083 0.8 1.0 1.0 }
  ), (
    { position := 1.0, color := Afferent.Color.hsva 0.139 0.6 1.0 1.0 }
  )]

  let spotlight : Array Afferent.GradientStop := #[(
    { position := 0.0, color := Afferent.Color.white }
  ), (
    { position := 0.7, color := Afferent.Color.hsva 0.0 0.0 1.0 0.3 }
  ), (
    { position := 1.0, color := Afferent.Color.hsva 0.0 0.0 1.0 0.0 }
  )]

  let stripes : Array Afferent.GradientStop := #[(
    { position := 0.0, color := Afferent.Color.red }
  ), (
    { position := 0.33, color := Afferent.Color.red }
  ), (
    { position := 0.34, color := Afferent.Color.white }
  ), (
    { position := 0.66, color := Afferent.Color.white }
  ), (
    { position := 0.67, color := Afferent.Color.blue }
  ), (
    { position := 1.0, color := Afferent.Color.blue }
  )]

  let chrome : Array Afferent.GradientStop := #[(
    { position := 0.0, color := Afferent.Color.hsva 0.0 0.0 0.3 1.0 }
  ), (
    { position := 0.2, color := Afferent.Color.hsva 0.0 0.0 0.9 1.0 }
  ), (
    { position := 0.4, color := Afferent.Color.hsva 0.0 0.0 0.5 1.0 }
  ), (
    { position := 0.6, color := Afferent.Color.hsva 0.0 0.0 0.8 1.0 }
  ), (
    { position := 0.8, color := Afferent.Color.hsva 0.0 0.0 0.4 1.0 }
  ), (
    { position := 1.0, color := Afferent.Color.hsva 0.0 0.0 0.6 1.0 }
  )]

  let gold : Array Afferent.GradientStop := #[(
    { position := 0.0, color := Afferent.Color.hsva 0.1 0.833 0.6 1.0 }
  ), (
    { position := 0.3, color := Afferent.Color.hsva 0.125 0.6 1.0 1.0 }
  ), (
    { position := 0.5, color := Afferent.Color.hsva 0.111 0.75 0.8 1.0 }
  ), (
    { position := 0.7, color := Afferent.Color.hsva 0.133 0.5 1.0 1.0 }
  ), (
    { position := 1.0, color := Afferent.Color.hsva 0.104 0.8 0.5 1.0 }
  )]

  let cards : Array (String × (Rect → RenderCommands)) := #[(
    "Linear Red-Yellow", linearHorizontal #[Afferent.Color.red, Afferent.Color.yellow]
  ), (
    "Linear Blue-Cyan", linearHorizontal #[Afferent.Color.blue, Afferent.Color.cyan]
  ), (
    "Linear Green-White", linearHorizontal #[Afferent.Color.green, Afferent.Color.white]
  ), (
    "Linear Vertical", linearVertical #[Afferent.Color.purple, Afferent.Color.orange]
  ), (
    "Linear Diagonal", linearDiagonal #[Afferent.Color.magenta, Afferent.Color.cyan]
  ), (
    "Rainbow", linearHorizontal #[
      Afferent.Color.red, Afferent.Color.orange, Afferent.Color.yellow,
      Afferent.Color.green, Afferent.Color.blue, Afferent.Color.purple, Afferent.Color.magenta
    ]
  ), (
    "Sunset", fun r =>
      let start := Afferent.Point.mk' (r.origin.x + r.size.width / 2) r.origin.y
      let finish := Afferent.Point.mk' (r.origin.x + r.size.width / 2) (r.origin.y + r.size.height)
      linearRect r start finish sunset
  ), (
    "Grayscale", linearHorizontal #[Afferent.Color.black, Afferent.Color.white]
  ), (
    "Radial Blue", radialCircle #[Afferent.Color.white, Afferent.Color.blue]
  ), (
    "Radial Warm", radialCircle #[Afferent.Color.yellow, Afferent.Color.orange, Afferent.Color.red]
  ), (
    "Spotlight", fun r =>
      let center := Afferent.Point.mk' (r.origin.x + r.size.width / 2) (r.origin.y + r.size.height / 2)
      let radius := minSide r * 0.45
      let style := Afferent.FillStyle.radialGradient center radius spotlight
      fillPathStyle (Path.circle ⟨center.x, center.y⟩ radius) style
  ), (
    "Radial Green", radialCircle #[
      Afferent.Color.hsva 0.333 0.5 1.0 1.0, Afferent.Color.green, Afferent.Color.hsva 0.333 1.0 0.3 1.0
    ]
  ), (
    "Radial Cyan", radialCircle #[Afferent.Color.cyan, Afferent.Color.magenta]
  ), (
    "Rounded Rect", fun r =>
      let start := Afferent.Point.mk' r.origin.x r.origin.y
      let finish := Afferent.Point.mk' (r.origin.x + r.size.width) (r.origin.y + r.size.height)
      linearRect r start finish (Afferent.GradientStop.distribute #[Afferent.Color.red, Afferent.Color.blue]) 12
  ), (
    "Ellipse", radialEllipse #[Afferent.Color.yellow, Afferent.Color.purple]
  ), (
    "Star", fun r =>
      let center := Afferent.Point.mk' (r.origin.x + r.size.width / 2) (r.origin.y + r.size.height / 2)
      let radius := minSide r * 0.4
      let start := Afferent.Point.mk' r.origin.x r.origin.y
      let finish := Afferent.Point.mk' (r.origin.x + r.size.width) (r.origin.y + r.size.height)
      let style := Afferent.FillStyle.linearGradient start finish
        (Afferent.GradientStop.distribute #[Afferent.Color.yellow, Afferent.Color.orange, Afferent.Color.red])
      fillPathStyle (Path.star ⟨center.x, center.y⟩ radius (radius * 0.5) 5) style
  ), (
    "Heart", fun r =>
      let center := Afferent.Point.mk' (r.origin.x + r.size.width / 2) (r.origin.y + r.size.height / 2)
      let radius := minSide r * 0.45
      let style := Afferent.FillStyle.radialGradient center radius
        (Afferent.GradientStop.distribute #[Afferent.Color.hsva 0.0 0.5 1.0 1.0, Afferent.Color.red, Afferent.Color.hsva 0.0 1.0 0.5 1.0])
      fillPathStyle (Path.heart ⟨center.x, center.y⟩ radius) style
  ), (
    "Stripes", fun r =>
      let start := Afferent.Point.mk' r.origin.x (r.origin.y + r.size.height / 2)
      let finish := Afferent.Point.mk' (r.origin.x + r.size.width) (r.origin.y + r.size.height / 2)
      linearRect r start finish stripes
  ), (
    "Chrome", fun r =>
      let start := Afferent.Point.mk' (r.origin.x + r.size.width / 2) r.origin.y
      let finish := Afferent.Point.mk' (r.origin.x + r.size.width / 2) (r.origin.y + r.size.height)
      linearRect r start finish chrome
  ), (
    "Gold", fun r =>
      let start := Afferent.Point.mk' (r.origin.x + r.size.width / 2) r.origin.y
      let finish := Afferent.Point.mk' (r.origin.x + r.size.width / 2) (r.origin.y + r.size.height)
      linearRect r start finish gold
  ), (
    "Deep Radial", fun r =>
      let center := Afferent.Point.mk' (r.origin.x + r.size.width / 2) (r.origin.y + r.size.height / 2)
      let radius := minSide r * 0.55
      let style := Afferent.FillStyle.radialGradient center radius #[(
        { position := 0.0, color := Afferent.Color.hsva 0.5 1.0 1.0 1.0 }
      ), (
        { position := 0.4, color := Afferent.Color.hsva 0.583 1.0 1.0 0.8 }
      ), (
        { position := 1.0, color := Afferent.Color.hsva 0.667 1.0 0.3 1.0 }
      )]
      #[.fillRectStyle r style 8]
  ), (
    "Purple-Pink", fun r =>
      let start := Afferent.Point.mk' (r.origin.x + r.size.width) r.origin.y
      let finish := Afferent.Point.mk' r.origin.x (r.origin.y + r.size.height)
      linearRect r start finish
        (Afferent.GradientStop.distribute #[Afferent.Color.hsva 0.778 1.0 0.6 1.0, Afferent.Color.hsva 0.944 0.6 1.0 1.0])
  )]

  let widgets := cards.map fun (label, draw) => demoCard labelFont label draw
  grid 4 10 { padding := EdgeInsets.uniform 10 } widgets

/-- Curated subset of gradients for responsive grid display. -/
def gradientsSubset : Array (String × (Rect → RenderCommands)) := #[
  ("Linear Red-Yellow", linearHorizontal #[Afferent.Color.red, Afferent.Color.yellow]),
  ("Linear Blue-Cyan", linearHorizontal #[Afferent.Color.blue, Afferent.Color.cyan]),
  ("Linear Vertical", linearVertical #[Afferent.Color.purple, Afferent.Color.orange]),
  ("Linear Diagonal", linearDiagonal #[Afferent.Color.magenta, Afferent.Color.cyan]),
  ("Rainbow", linearHorizontal #[
    Afferent.Color.red, Afferent.Color.orange, Afferent.Color.yellow,
    Afferent.Color.green, Afferent.Color.blue, Afferent.Color.purple
  ]),
  ("Radial Blue", radialCircle #[Afferent.Color.white, Afferent.Color.blue]),
  ("Radial Warm", radialCircle #[Afferent.Color.yellow, Afferent.Color.orange, Afferent.Color.red]),
  ("Ellipse", radialEllipse #[Afferent.Color.yellow, Afferent.Color.purple]),
  ("Grayscale", linearHorizontal #[Afferent.Color.black, Afferent.Color.white])
]

/-- Responsive gradients widget that fills available space. -/
def gradientsWidgetFlex (labelFont : FontId) : WidgetBuilder := do
  let widgets := gradientsSubset.map fun (label, draw) => demoCardFlex labelFont label draw
  gridFlex 3 3 4 widgets

/-- Render gradients demo content to canvas using Arbor widgets. -/
def renderGradientsM (reg : Afferent.FontRegistry) (labelFont : FontId) : Afferent.CanvasM Unit := do
  let widget := Afferent.Arbor.build (gradientsWidget labelFont)
  Afferent.Widget.renderArborWidget reg widget 1000 800

end Demos
