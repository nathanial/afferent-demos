/-
  Voronoi/Delaunay Dual Demo - dual relationship between Delaunay triangulation
  and Voronoi diagram with incremental construction.
-/
import Afferent
import Afferent.Widget
import Afferent.Arbor
import Demos.Core.Demo
import Demos.Linalg.Shared
import Trellis
import Linalg.Core
import Linalg.Vec2
import Linalg.Geometry.Delaunay
import Linalg.Geometry.Voronoi
import Linalg.Geometry.AABB2D
import Linalg.Geometry.Polygon2D
import AfferentMath.Widget.MathView2D

open Afferent CanvasM Linalg
open Afferent.Widget
open AfferentMath.Widget

namespace Demos.Linalg

/-- Default point set for the Voronoi/Delaunay demo. -/
def defaultVoronoiPoints : Array Vec2 := #[
  Vec2.mk (-3.8) (-2.4),
  Vec2.mk (-3.1) (1.7),
  Vec2.mk (-2.6) (-0.2),
  Vec2.mk (-1.9) (2.3),
  Vec2.mk (-1.5) (-1.8),
  Vec2.mk (-0.8) (0.4),
  Vec2.mk (-0.2) (-2.7),
  Vec2.mk (0.1) (1.9),
  Vec2.mk (0.8) (-0.6),
  Vec2.mk (1.2) (2.6),
  Vec2.mk (1.6) (-1.7),
  Vec2.mk (2.1) (0.8),
  Vec2.mk (2.6) (-2.3),
  Vec2.mk (3.1) (1.5),
  Vec2.mk (3.6) (-0.4),
  Vec2.mk (-0.5) (2.8),
  Vec2.mk (-2.9) (0.9),
  Vec2.mk (0.9) (0.2),
  Vec2.mk (2.4) (2.1),
  Vec2.mk (-1.1) (1.1)
]

/-- State for Voronoi/Delaunay demo. -/
structure VoronoiDelaunayDualState where
  points : Array Vec2 := defaultVoronoiPoints
  dragging : Option Nat := none
  selectedSite : Nat := 0
  selectedTriangle : Nat := 0
  showDelaunay : Bool := true
  showVoronoi : Bool := true
  showDual : Bool := true
  showCircumcircle : Bool := true
  animating : Bool := true
  time : Float := 0.0
  speed : Float := 1.2
  activeCount : Nat := defaultVoronoiPoints.size
  deriving Inhabited

/-- Initial state. -/
def voronoiDelaunayDualInitialState : VoronoiDelaunayDualState := {}

def voronoiMathViewConfig (screenScale : Float) : MathView2D.Config := {
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

def computeActiveCount (pointsCount : Nat) (time speed : Float) : Nat :=
  if pointsCount <= 3 then pointsCount
  else
    let steps := pointsCount - 2
    let idx := Float.floor (time * speed) |>
      Float.toUInt64 |> UInt64.toNat
    3 + idx % steps

private def drawLineWorld (a b : Vec2) (origin : Float × Float) (scale : Float)
    (color : Color) (lineWidth : Float := 1.2) : CanvasM Unit := do
  let (sx, sy) := worldToScreen a origin scale
  let (ex, ey) := worldToScreen b origin scale
  setStrokeColor color
  setLineWidth lineWidth
  let path := Afferent.Path.empty
    |>.moveTo (Point.mk sx sy)
    |>.lineTo (Point.mk ex ey)
  strokePath path

private def drawPolygonWorld (poly : Array Vec2) (origin : Float × Float) (scale : Float)
    (stroke : Color) (fill : Option Color := none) (lineWidth : Float := 1.2) : CanvasM Unit := do
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

/-- Render the Voronoi/Delaunay visualization. -/
def renderVoronoiDelaunayDual (state : VoronoiDelaunayDualState)
    (view : MathView2D.View) (screenScale : Float) (fontMedium fontSmall : Font) : CanvasM Unit := do
  let w := view.width
  let h := view.height
  let origin : Float × Float := (view.origin.x, view.origin.y)
  let scale := view.scale

  let activeCount := Nat.min state.activeCount state.points.size
  let activePoints := state.points.take activeCount
  let selectedSite := if activeCount == 0 then 0 else state.selectedSite % activeCount

  let bounds := AABB2D.fromCenterExtents Vec2.zero
    (Vec2.mk (w / (2 * scale)) (h / (2 * scale)))

  let mut circumcircleEmpty := none

  match Delaunay.triangulate activePoints with
  | none =>
      setFillColor (Color.gray 0.7)
      fillTextXY "Need 3 non-collinear points" (20 * screenScale) (80 * screenScale) fontSmall
  | some tri =>
      let triCount := Delaunay.Triangulation.triangleCount tri
      let selectedTriangle := if triCount == 0 then 0 else state.selectedTriangle % triCount

      let diagram := Voronoi.fromDelaunay tri
      let clipped := Voronoi.clipToBounds diagram bounds

      let mut centers : Array (Option Vec2) := #[]
      for t in [:triCount] do
        match Delaunay.Triangulation.getTriangle tri t with
        | some (i0, i1, i2) =>
            let c := Delaunay.circumcenter tri.points[i0]! tri.points[i1]! tri.points[i2]!
            centers := centers.push c
        | none =>
            centers := centers.push none

      if state.showVoronoi then
        for i in [:clipped.size] do
          let poly := clipped[i]!
          let fill := if i == selectedSite then
            some (Color.rgba 1.0 0.7 0.2 0.15)
          else
            none
          drawPolygonWorld poly.vertices origin scale (Color.rgba 1.0 0.6 0.3 0.7) fill 1.1

      if state.showDual then
        for e in [:tri.halfedges.size] do
          match tri.halfedges[e]! with
          | some twin =>
              if e < twin then
                let triA := Delaunay.Triangulation.triangleOfEdge e
                let triB := Delaunay.Triangulation.triangleOfEdge twin
                match centers[triA]!, centers[triB]! with
                | some ca, some cb =>
                    let sa := worldToScreen ca origin scale
                    let sb := worldToScreen cb origin scale
                    drawDashedLine sa sb (Color.rgba 0.9 0.9 0.2 0.8) 6.0 4.0 1.3
                | _, _ => pure ()
          | none => pure ()

      if state.showDelaunay then
        for t in [:triCount] do
          match Delaunay.Triangulation.getTriangle tri t with
          | some (i0, i1, i2) =>
              let p0 := tri.points[i0]!
              let p1 := tri.points[i1]!
              let p2 := tri.points[i2]!
              drawLineWorld p0 p1 origin scale (Color.rgba 0.2 0.7 1.0 0.8) 1.3
              drawLineWorld p1 p2 origin scale (Color.rgba 0.2 0.7 1.0 0.8) 1.3
              drawLineWorld p2 p0 origin scale (Color.rgba 0.2 0.7 1.0 0.8) 1.3
          | none => pure ()

      if state.showCircumcircle && triCount > 0 then
        match Delaunay.Triangulation.getTriangle tri selectedTriangle with
        | some (i0, i1, i2) =>
            let a := tri.points[i0]!
            let b := tri.points[i1]!
            let c := tri.points[i2]!
            match Delaunay.circumcenter a b c with
            | some center =>
                let radius := (a - center).length
                let (sx, sy) := worldToScreen center origin scale
                setStrokeColor (Color.rgba 0.9 0.3 0.9 0.8)
                setLineWidth 1.6
                strokePath (Afferent.Path.circle (Point.mk sx sy) (radius * scale))

                let mut insideCount := 0
                let r2 := radius * radius - 0.0001
                for i in [:tri.points.size] do
                  if i != i0 && i != i1 && i != i2 then
                    let d2 := (tri.points[i]! - center).lengthSquared
                    if d2 < r2 then
                      insideCount := insideCount + 1
                circumcircleEmpty := some (insideCount == 0, insideCount)
            | none => pure ()
        | none => pure ()

      if activePoints.size > 0 then
        let cell := diagram.cells.getD selectedSite { siteIndex := 0, vertices := #[], isUnbounded := false }
        let clippedVerts := clipped.getD selectedSite (Polygon2D.fromVertices #[])

        let infoY := h - 140 * screenScale
        setFillColor VecColor.label
        fillTextXY s!"sites: {activePoints.size}/{state.points.size}" (20 * screenScale) infoY fontSmall
        fillTextXY s!"triangles: {triCount}" (20 * screenScale) (infoY + 20 * screenScale) fontSmall
        fillTextXY s!"site {selectedSite} cell vertices: {cell.vertices.size} clipped: {clippedVerts.vertices.size}"
          (20 * screenScale) (infoY + 40 * screenScale) fontSmall
        fillTextXY s!"cell unbounded: {cell.isUnbounded}" (20 * screenScale) (infoY + 60 * screenScale) fontSmall
        match circumcircleEmpty with
        | some (empty, count) =>
            fillTextXY s!"empty circumcircle: {empty} (points inside: {count})"
              (20 * screenScale) (infoY + 80 * screenScale) fontSmall
        | none => pure ()

  for i in [:activePoints.size] do
    let p := activePoints[i]!
    let color := if i == selectedSite then Color.rgba 1.0 0.9 0.3 1.0 else Color.white
    drawMarker p origin scale color 8.0

  fillTextXY "VORONOI / DELAUNAY DUAL" (20 * screenScale) (30 * screenScale) fontMedium
  setFillColor (Color.gray 0.6)
  let animText := if state.animating then "animating" else "paused"
  fillTextXY
    s!"Space: {animText} | D: Delaunay | V: Voronoi | U: dual | C: circumcircle | T: next tri | N: next site"
    (20 * screenScale) (55 * screenScale) fontSmall

/-- Create the Voronoi/Delaunay dual widget. -/
def voronoiDelaunayDualWidget (env : DemoEnv) (state : VoronoiDelaunayDualState)
    : Afferent.Arbor.WidgetBuilder := do
  let config := voronoiMathViewConfig env.screenScale
  MathView2D.mathView2D config env.fontSmall (fun view => do
    renderVoronoiDelaunayDual state view env.screenScale env.fontMedium env.fontSmall
  )

end Demos.Linalg
