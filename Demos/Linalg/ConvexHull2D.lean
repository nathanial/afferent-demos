/-
  Convex Hull 2D Demo - point cloud with hull construction animation.
-/
import Afferent
import Afferent.Widget
import Afferent.Arbor
import Demos.Core.Demo
import Demos.Linalg.Shared
import Trellis
import Linalg.Core
import Linalg.Vec2
import Linalg.Geometry.Polygon2D

open Afferent CanvasM Linalg
open Afferent.Widget

namespace Demos.Linalg

/-- Default point set for convex hull demo. -/
def defaultHullPoints : Array Vec2 := #[
  Vec2.mk (-3.2) (-1.8),
  Vec2.mk (-2.6) (1.5),
  Vec2.mk (-2.0) (-0.6),
  Vec2.mk (-1.3) (2.2),
  Vec2.mk (-0.7) (-2.4),
  Vec2.mk (-0.1) (0.6),
  Vec2.mk (0.6) (-1.3),
  Vec2.mk (1.1) (2.4),
  Vec2.mk (1.8) (0.5),
  Vec2.mk (2.3) (-2.0),
  Vec2.mk (2.9) (1.2),
  Vec2.mk (3.3) (-0.4),
  Vec2.mk (-1.8) (0.9),
  Vec2.mk (0.2) (-0.1),
  Vec2.mk (1.5) (1.6),
  Vec2.mk (-2.9) (-0.2)
]

/-- State for convex hull demo. -/
structure ConvexHull2DState where
  points : Array Vec2 := defaultHullPoints
  dragging : Option Nat := none
  draggingQuery : Bool := false
  queryPoint : Vec2 := Vec2.mk 0.2 0.4
  animating : Bool := true
  time : Float := 0.0
  speed : Float := 1.6
  showHull : Bool := true
  showGiftWrap : Bool := true
  showHullFill : Bool := false
  lastMouse : Vec2 := Vec2.zero
  deriving Inhabited

/-- Initial state. -/
def convexHull2DInitialState : ConvexHull2DState := {}

def convexHullMathViewConfig (screenScale : Float) : MathView2D.Config := {
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

private def leftmostIndex (points : Array Vec2) : Nat :=
  if points.size == 0 then 0
  else Id.run do
    let mut idx := 0
    let mut best := points[0]!
    for i in [:points.size] do
      let p := points[i]!
      if p.x < best.x || (p.x == best.x && p.y < best.y) then
        idx := i
        best := p
    return idx

private def giftWrapIndices (points : Array Vec2) : Array Nat := Id.run do
  if points.size == 0 then return #[]
  if points.size <= 2 then return (Array.range points.size)

  let start := leftmostIndex points
  let mut hull : Array Nat := #[start]
  let mut current := start
  let mut guard := 0

  while guard < points.size * 2 do
    let mut next := if current == 0 then 1 else 0
    for i in [:points.size] do
      if i != current && i != next then
        let a := points[current]!
        let b := points[next]!
        let c := points[i]!
        let cross := (b - a).cross (c - a)
        if cross > 0.0 then
          next := i
        else if Float.abs cross < 1e-6 then
          if a.distanceSquared c > a.distanceSquared b then
            next := i
    if next == start then
      break
    hull := hull.push next
    current := next
    guard := guard + 1

  return hull

private def drawPolygonWorld (poly : Array Vec2) (origin : Float × Float) (scale : Float)
    (stroke : Color) (fill : Option Color := none) (lineWidth : Float := 1.6) : CanvasM Unit := do
  if poly.size < 2 then return
  let first := poly[0]!
  let (sx, sy) := worldToScreen first origin scale
  let mut path := Afferent.Path.empty
    |>.moveTo (Point.mk sx sy)
  for i in [1:poly.size] do
    let p := poly[i]!
    let (px, py) := worldToScreen p origin scale
    path := path.lineTo (Point.mk px py)
  path := path.closePath

  match fill with
  | some c =>
      setFillColor c
      fillPath path
  | none => pure ()

  setStrokeColor stroke
  setLineWidth lineWidth
  strokePath path

private def drawLineWorld (a b : Vec2) (origin : Float × Float) (scale : Float)
    (color : Color) (lineWidth : Float := 1.4) : CanvasM Unit := do
  let (sx, sy) := worldToScreen a origin scale
  let (ex, ey) := worldToScreen b origin scale
  setStrokeColor color
  setLineWidth lineWidth
  let path := Afferent.Path.empty
    |>.moveTo (Point.mk sx sy)
    |>.lineTo (Point.mk ex ey)
  strokePath path

/-- Render convex hull demo. -/
def renderConvexHull2D (state : ConvexHull2DState)
    (view : MathView2D.View) (screenScale : Float) (fontMedium fontSmall : Font) : CanvasM Unit := do
  let w := view.width
  let h := view.height
  let origin : Float × Float := (view.origin.x, view.origin.y)
  let scale := view.scale

  let hullPoly := Polygon2D.convexHull state.points
  let hullIndices := giftWrapIndices state.points
  let extremeIdx := leftmostIndex state.points

  if state.showHull && hullPoly.vertices.size >= 3 then
    let fill := if state.showHullFill then some (Color.rgba 0.2 0.8 0.3 0.12) else none
    drawPolygonWorld hullPoly.vertices origin scale (Color.rgba 0.2 0.9 0.3 0.8) fill 2.0

  if state.showGiftWrap && hullIndices.size > 1 then
    let stepRaw := Float.floor (state.time * state.speed) |>
      Float.toUInt64 |> UInt64.toNat
    let stepCount := (stepRaw % hullIndices.size) + 1
    for i in [:stepCount - 1] do
      let a := state.points[hullIndices[i]!]!
      let b := state.points[hullIndices[i + 1]!]!
      drawLineWorld a b origin scale (Color.rgba 0.9 0.7 0.2 0.9) 2.2
    if stepCount == hullIndices.size then
      let last := state.points[hullIndices[stepCount - 1]!]!
      let first := state.points[hullIndices[0]!]!
      drawLineWorld last first origin scale (Color.rgba 0.9 0.7 0.2 0.9) 2.2
    else
      let last := state.points[hullIndices[stepCount - 1]!]!
      let next := state.points[hullIndices[stepCount]!]!
      drawLineWorld last next origin scale (Color.rgba 1.0 0.9 0.2 0.9) 3.0

  for i in [:state.points.size] do
    let p := state.points[i]!
    let color := if i == extremeIdx then Color.rgba 1.0 0.9 0.3 1.0 else Color.white
    drawMarker p origin scale color 7.5

  let inside := if hullPoly.vertices.size >= 3 then
      hullPoly.containsPointInclusive state.queryPoint 1e-5
    else
      false
  let queryColor := if inside then Color.rgba 0.2 0.9 0.3 1.0 else Color.rgba 0.9 0.3 0.3 1.0
  drawMarker state.queryPoint origin scale queryColor 9.0

  fillTextXY "CONVEX HULL 2D" (20 * screenScale) (30 * screenScale) fontMedium
  setFillColor (Color.gray 0.6)
  let animText := if state.animating then "animating" else "paused"
  fillTextXY
    s!"Space: {animText} | Click: add point | Drag: move point/query | Right click: remove | H: hull | G: gift wrap | F: fill"
    (20 * screenScale) (55 * screenScale) fontSmall

  let infoY := h - 120 * screenScale
  setFillColor VecColor.label
  fillTextXY s!"points: {state.points.size}" (20 * screenScale) infoY fontSmall
  fillTextXY s!"hull vertices: {hullPoly.vertices.size}" (20 * screenScale) (infoY + 20 * screenScale) fontSmall
  fillTextXY s!"extreme point: {formatVec2 (state.points.getD extremeIdx Vec2.zero)}"
    (20 * screenScale) (infoY + 40 * screenScale) fontSmall
  fillTextXY s!"query inside hull: {inside}" (20 * screenScale) (infoY + 60 * screenScale) fontSmall

/-- Create the convex hull widget. -/
def convexHull2DWidget (env : DemoEnv) (state : ConvexHull2DState)
    : Afferent.Arbor.WidgetBuilder := do
  let config := convexHullMathViewConfig env.screenScale
  MathView2D.mathView2D config env.fontSmall (fun view => do
    renderConvexHull2D state view env.screenScale env.fontMedium env.fontSmall
  )

end Demos.Linalg
