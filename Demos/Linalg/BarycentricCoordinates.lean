/-
  Barycentric Coordinates - triangle with movable test point.
-/
import Afferent
import Afferent.Widget
import Afferent.Arbor
import Demos.Core.Demo
import Demos.Linalg.Shared
import Trellis
import Linalg.Core
import Linalg.Vec2
import Linalg.Vec3
import Linalg.Geometry.Triangle
import AfferentMath.Widget.MathView2D

open Afferent CanvasM Linalg
open Afferent.Widget
open AfferentMath.Widget

namespace Demos.Linalg

/-- State for barycentric coordinate demo. -/
structure BarycentricCoordinatesState where
  v0 : Vec2 := Vec2.mk (-2.0) (-1.2)
  v1 : Vec2 := Vec2.mk 2.2 (-0.6)
  v2 : Vec2 := Vec2.mk (-0.6) 2.0
  point : Vec2 := Vec2.mk 0.4 0.2
  dragging : Bool := false
  deriving Inhabited

/-- Initial state. -/
def barycentricCoordinatesInitialState : BarycentricCoordinatesState := {}

def barycentricMathViewConfig (screenScale : Float) : MathView2D.Config := {
  style := { flexItem := some (Trellis.FlexItem.growing 1) }
  scale := 70.0 * screenScale
  minorStep := 1.0
  majorStep := 2.0
  gridMinorColor := Color.gray 0.2
  gridMajorColor := Color.gray 0.4
  axisColor := Color.gray 0.6
  labelColor := VecColor.label
  labelPrecision := 0
}

private def drawTriangleEdges (v0 v1 v2 : Vec2) (origin : Float × Float) (scale : Float) : CanvasM Unit := do
  let (x0, y0) := worldToScreen v0 origin scale
  let (x1, y1) := worldToScreen v1 origin scale
  let (x2, y2) := worldToScreen v2 origin scale
  setStrokeColor (Color.gray 0.7)
  setLineWidth 2.0
  let path := Afferent.Path.empty
    |>.moveTo (Point.mk x0 y0)
    |>.lineTo (Point.mk x1 y1)
    |>.lineTo (Point.mk x2 y2)
    |>.closePath
  strokePath path

/-- Render barycentric coordinates. -/
def renderBarycentricCoordinates (state : BarycentricCoordinatesState)
    (view : MathView2D.View) (screenScale : Float) (fontMedium fontSmall : Font) : CanvasM Unit := do
  let w := view.width
  let h := view.height
  let origin : Float × Float := (view.origin.x, view.origin.y)
  let scale := view.scale

  drawTriangleEdges state.v0 state.v1 state.v2 origin scale

  -- Vertex markers (RGB)
  let (x0, y0) := worldToScreen state.v0 origin scale
  let (x1, y1) := worldToScreen state.v1 origin scale
  let (x2, y2) := worldToScreen state.v2 origin scale
  setFillColor (Color.rgba 1.0 0.2 0.2 1.0)
  fillPath (Afferent.Path.circle (Point.mk x0 y0) 6.0)
  setFillColor (Color.rgba 0.2 1.0 0.2 1.0)
  fillPath (Afferent.Path.circle (Point.mk x1 y1) 6.0)
  setFillColor (Color.rgba 0.2 0.4 1.0 1.0)
  fillPath (Afferent.Path.circle (Point.mk x2 y2) 6.0)

  -- Barycentric coordinates
  let tri := Triangle.mk' (Vec3.mk state.v0.x state.v0.y 0.0)
    (Vec3.mk state.v1.x state.v1.y 0.0)
    (Vec3.mk state.v2.x state.v2.y 0.0)
  let p3 := Vec3.mk state.point.x state.point.y 0.0
  let bc := tri.barycentric p3
  let inside := bc.isInside

  let u := Float.clamp bc.u 0.0 1.0
  let v := Float.clamp bc.v 0.0 1.0
  let w' := Float.clamp bc.w 0.0 1.0
  let color := Color.rgba u v w' 1.0

  let (px, py) := worldToScreen state.point origin scale
  setFillColor color
  fillPath (Afferent.Path.circle (Point.mk px py) 6.0)
  if inside then
    setStrokeColor Color.white
  else
    setStrokeColor (Color.rgba 1.0 0.4 0.4 1.0)
  setLineWidth 2.0
  strokePath (Afferent.Path.circle (Point.mk px py) 8.0)

  -- Reconstructed point from barycentric
  let recon := tri.fromBarycentric bc
  let (rx, ry) := worldToScreen (Vec2.mk recon.x recon.y) origin scale
  setFillColor (Color.gray 0.8)
  fillPath (Afferent.Path.circle (Point.mk rx ry) 3.0)

  -- Info panel
  let infoY := h - 140 * screenScale
  setFillColor VecColor.label
  fillTextXY s!"u={formatFloat bc.u}, v={formatFloat bc.v}, w={formatFloat bc.w}"
    (20 * screenScale) infoY fontSmall
  fillTextXY s!"inside: {inside}" (20 * screenScale) (infoY + 22 * screenScale) fontSmall

  -- Title and instructions
  fillTextXY "BARYCENTRIC COORDINATES" (20 * screenScale) (30 * screenScale) fontMedium
  setFillColor (Color.gray 0.6)
  fillTextXY "Drag the point | RGB = (u, v, w)" (20 * screenScale) (55 * screenScale) fontSmall

/-- Create the barycentric coordinates widget. -/
def barycentricCoordinatesWidget (env : DemoEnv) (state : BarycentricCoordinatesState)
    : Afferent.Arbor.WidgetBuilder := do
  let config := barycentricMathViewConfig env.screenScale
  MathView2D.mathView2D config env.fontSmall (fun view => do
    renderBarycentricCoordinates state view env.screenScale env.fontMedium env.fontSmall
  )

end Demos.Linalg
