/- 
  B-Spline Curve Demo - adjustable degree, editable knot vector, basis functions.
-/
import Afferent
import Afferent.Widget
import Afferent.Arbor
import Demos.Core.Demo
import Demos.Linalg.Shared
import Trellis
import Linalg.Core
import Linalg.Vec2
import Linalg.Curves

open Afferent CanvasM Linalg
open Afferent.Widget

namespace Demos.Linalg

/-- Drag state for B-spline demo. -/
inductive BSplineDragMode where
  | none
  | point (idx : Nat)
  | knot (idx : Nat)
  deriving BEq, Inhabited

/-- Default control points for the spline. -/
private def defaultControlPoints : Array Vec2 := #[
  Vec2.mk (-3.0) (-1.5), Vec2.mk (-2.0) 1.6, Vec2.mk (-0.8) (-0.8),
  Vec2.mk (0.8) 1.8, Vec2.mk (2.2) (-1.2), Vec2.mk (3.0) 0.6
]

/-- State for B-spline demo. -/
structure BSplineCurveDemoState where
  controlPoints : Array Vec2 := defaultControlPoints
  degree : Nat := 3
  knots : Array Float := #[]
  dragging : BSplineDragMode := .none
  deriving Inhabited

private def makeUniformKnots (points : Array Vec2) (degree : Nat) : Array Float :=
  (Linalg.BSpline.uniform points degree).knots

def bSplineCurveDemoInitialState : BSplineCurveDemoState :=
  let degree := 3
  { controlPoints := defaultControlPoints
    degree := degree
    knots := makeUniformKnots defaultControlPoints degree
    dragging := .none }

def bSplineMathViewConfig (screenScale : Float) : MathView2D.Config := {
  style := { flexItem := some (Trellis.FlexItem.growing 1) }
  scale := 60.0 * screenScale
  originOffset := (0.0, -40.0 * screenScale)
  minorStep := 1.0
  majorStep := 2.0
  gridMinorColor := Color.gray 0.2
  gridMajorColor := Color.gray 0.4
  axisColor := Color.gray 0.6
  labelColor := VecColor.label
  labelPrecision := 0
}

private def clamp01 (t : Float) : Float :=
  if t < 0.0 then 0.0 else if t > 1.0 then 1.0 else t

private def toScreen (view : MathView2D.View) (p : Vec2) : Float × Float :=
  MathView2D.worldToScreen view p

private def drawPolylineWorld (points : Array Vec2) (view : MathView2D.View)
    (color : Color) (lineWidth : Float := 2.0) : CanvasM Unit := do
  if points.size < 2 then return
  let p0 := toScreen view (points.getD 0 Vec2.zero)
  let mut path := Afferent.Path.empty
    |>.moveTo (Point.mk p0.1 p0.2)
  for i in [1:points.size] do
    let p := toScreen view (points.getD i Vec2.zero)
    path := path.lineTo (Point.mk p.1 p.2)
  setStrokeColor color
  setLineWidth lineWidth
  strokePath path

private def drawPointWorld (p : Vec2) (view : MathView2D.View)
    (color : Color) (radius : Float := 5.5) : CanvasM Unit := do
  let (x, y) := toScreen view p
  setFillColor color
  fillPath (Afferent.Path.circle (Point.mk x y) radius)

private def sampleSpline (evalFn : Float → Vec2) (segments : Nat := 140) : Array Vec2 := Id.run do
  let steps := if segments < 2 then 2 else segments
  let mut pts : Array Vec2 := #[]
  for i in [:steps] do
    let t := i.toFloat / (steps - 1).toFloat
    pts := pts.push (evalFn t)
  return pts

/-- Plot rectangle for basis functions. -/
structure BasisPlotRect where
  x : Float
  y : Float
  w : Float
  h : Float

private def basisPlotRect (w h screenScale : Float) : BasisPlotRect :=
  { x := 40.0 * screenScale
    y := h - 190.0 * screenScale
    w := w - 80.0 * screenScale
    h := 120.0 * screenScale }

private def basisToScreen (rect : BasisPlotRect) (t val : Float) : Float × Float :=
  let x := rect.x + t * rect.w
  let y := rect.y + (1.0 - val) * rect.h
  (x, y)

private def drawBasisFunctions (b : Linalg.BSpline Vec2) (rect : BasisPlotRect) : CanvasM Unit := do
  if b.controlPoints.isEmpty then return
  let colors : Array Color := #[(Color.rgba 0.9 0.3 0.3 0.9), (Color.rgba 0.3 0.9 0.4 0.9),
    (Color.rgba 0.3 0.6 1.0 0.9), (Color.rgba 1.0 0.8 0.2 0.9),
    (Color.rgba 0.9 0.4 1.0 0.9), (Color.rgba 0.2 0.9 0.9 0.9)]

  for i in [:b.controlPoints.size] do
    let color := colors.getD (i % colors.size) Color.white
    let mut path := Afferent.Path.empty
    let steps := 60
    for j in [:steps] do
      let t := j.toFloat / (steps - 1).toFloat
      let v := Linalg.BSpline.basisFunction b.knots i b.degree t
      let (x, y) := basisToScreen rect t v
      if j == 0 then
        path := path.moveTo (Point.mk x y)
      else
        path := path.lineTo (Point.mk x y)
    setStrokeColor color
    setLineWidth 1.5
    strokePath path

private def drawKnotMarkers (b : Linalg.BSpline Vec2) (rect : BasisPlotRect)
    (active : Option Nat := none) : CanvasM Unit := do
  if b.knots.isEmpty then return
  let k := b.degree
  let n := b.knots.size
  for i in [:n] do
    let knot := b.knots.getD i 0.0
    let (x, _) := basisToScreen rect knot 0.0
    let editable := i > k && i < n - k - 1
    let isActive := match active with | some idx => idx == i | none => false
    let radius := if editable then (if isActive then 6.0 else 4.5) else 3.5
    let color := if editable then (if isActive then Color.yellow else Color.gray 0.7) else Color.gray 0.4
    setFillColor color
    fillPath (Afferent.Path.circle (Point.mk x (rect.y + rect.h + 12.0)) radius)

/-- Render B-spline demo. -/
def renderBSplineCurveDemo (state : BSplineCurveDemoState)
    (view : MathView2D.View) (screenScale : Float) (fontMedium fontSmall : Font) : CanvasM Unit := do
  let w := view.width
  let h := view.height

  -- Control polygon
  drawPolylineWorld state.controlPoints view (Color.gray 0.4) 1.2
  for i in [:state.controlPoints.size] do
    let p := state.controlPoints.getD i Vec2.zero
    let color := if i == 0 then Color.rgba 1.0 0.3 0.3 1.0
      else if i == state.controlPoints.size - 1 then Color.rgba 0.3 0.9 0.3 1.0
      else Color.rgba 0.4 0.7 1.0 1.0
    drawPointWorld p view color 6.0

  let spline : Linalg.BSpline Vec2 := {
    controlPoints := state.controlPoints,
    knots := state.knots,
    degree := state.degree
  }
  let curve := sampleSpline (fun tt => Linalg.BSpline.evalVec2 spline tt) 140
  drawPolylineWorld curve view (Color.rgba 0.2 0.9 1.0 1.0) 2.4

  -- Basis plot
  let rect := basisPlotRect w h screenScale
  setStrokeColor (Color.gray 0.35)
  setLineWidth 1.0
  strokePath (Afferent.Path.rectangleXYWH rect.x rect.y rect.w rect.h)
  drawBasisFunctions spline rect

  let activeKnot := match state.dragging with | .knot idx => some idx | _ => none
  drawKnotMarkers spline rect activeKnot

  -- Labels
  setFillColor VecColor.label
  fillTextXY "B-SPLINE CURVE" (20 * screenScale) (30 * screenScale) fontMedium
  setFillColor (Color.gray 0.6)
  fillTextXY "Drag points | Drag knot markers | 1-5 set degree | U: reset knots | R: reset"
    (20 * screenScale) (55 * screenScale) fontSmall
  setFillColor VecColor.label
  fillTextXY s!"degree={state.degree}  knots={state.knots.size}" (20 * screenScale) (h - 40 * screenScale) fontSmall

/-- Create B-spline demo widget. -/
def bSplineCurveDemoWidget (env : DemoEnv) (state : BSplineCurveDemoState)
    : Afferent.Arbor.WidgetBuilder := do
  let config := bSplineMathViewConfig env.screenScale
  MathView2D.mathView2D config env.fontSmall (fun view => do
    renderBSplineCurveDemo state view env.screenScale env.fontMedium env.fontSmall
  )

end Demos.Linalg
