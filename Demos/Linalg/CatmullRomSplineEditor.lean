/- 
  Catmull-Rom Spline Editor - add/drag points, adjust alpha, open/closed toggle.
  Compares uniform spline path vs alpha-parameterized variant.
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

/-- Drag state for spline editor. -/
inductive CatmullDragMode where
  | none
  | point (idx : Nat)
  | slider
  deriving BEq, Inhabited

/-- State for Catmull-Rom spline editor. -/
structure CatmullRomSplineEditorState where
  points : Array Vec2 := #[
    Vec2.mk (-3.0) (-1.2), Vec2.mk (-1.2) 1.8,
    Vec2.mk (0.8) (-0.4), Vec2.mk (2.4) 1.6
  ]
  closed : Bool := false
  alpha : Float := 0.5
  t : Float := 0.0
  animating : Bool := true
  dragging : CatmullDragMode := .none
  lastTime : Float := 0.0
  deriving Inhabited

def catmullRomSplineEditorInitialState : CatmullRomSplineEditorState := {}

def catmullRomMathViewConfig (screenScale : Float) : MathView2D.Config := {
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

private def clamp01 (t : Float) : Float :=
  if t < 0.0 then 0.0 else if t > 1.0 then 1.0 else t

/-- Slider layout for alpha. -/
structure AlphaSliderLayout where
  x : Float
  y : Float
  width : Float
  height : Float

private def alphaSliderLayout (w _h screenScale : Float) : AlphaSliderLayout :=
  { x := w - 260.0 * screenScale
    y := 95.0 * screenScale
    width := 190.0 * screenScale
    height := 8.0 * screenScale }

private def renderAlphaSlider (value : Float) (layout : AlphaSliderLayout) (fontSmall : Font)
    (active : Bool := false) : CanvasM Unit := do
  let t := clamp01 value
  let knobX := layout.x + t * layout.width
  let knobY := layout.y + layout.height / 2.0
  let knobRadius := layout.height * 0.75
  let trackHeight := layout.height * 0.5

  setFillColor (if active then Color.gray 0.7 else Color.gray 0.5)
  fillPath (Afferent.Path.rectangleXYWH layout.x (layout.y + layout.height / 2.0 - trackHeight / 2.0)
    layout.width trackHeight)
  setFillColor (if active then Color.yellow else Color.gray 0.8)
  fillPath (Afferent.Path.circle (Point.mk knobX knobY) knobRadius)

  setFillColor (Color.gray 0.8)
  fillTextXY "alpha" (layout.x - 44.0) (layout.y + 6.0) fontSmall

private def drawPolylineWorld (points : Array Vec2) (origin : Float × Float) (scale : Float)
    (color : Color) (lineWidth : Float := 2.0) : CanvasM Unit := do
  if points.size < 2 then return
  let p0 := worldToScreen (points.getD 0 Vec2.zero) origin scale
  let mut path := Afferent.Path.empty
    |>.moveTo (Point.mk p0.1 p0.2)
  for i in [1:points.size] do
    let p := worldToScreen (points.getD i Vec2.zero) origin scale
    path := path.lineTo (Point.mk p.1 p.2)
  setStrokeColor color
  setLineWidth lineWidth
  strokePath path

private def drawPointWorld (p : Vec2) (origin : Float × Float) (scale : Float)
    (color : Color) (radius : Float := 5.5) : CanvasM Unit := do
  let (x, y) := worldToScreen p origin scale
  setFillColor color
  fillPath (Afferent.Path.circle (Point.mk x y) radius)

/-- Parameterized Catmull-Rom segment evaluation with alpha. -/
private def catmullRomEvalAlphaSegment (p0 p1 p2 p3 : Vec2) (alpha t : Float) : Vec2 :=
  let a := clamp01 alpha
  let d01 := (p1.sub p0).length
  let d12 := (p2.sub p1).length
  let d23 := (p3.sub p2).length
  let t0 := 0.0
  let t1 := t0 + Float.pow d01 a
  let t2 := t1 + Float.pow d12 a
  let t3 := t2 + Float.pow d23 a
  let tau := Float.lerp t1 t2 (clamp01 t)

  let blend (pa pb : Vec2) (ta tb : Float) : Vec2 :=
    if Float.abs (tb - ta) < Float.epsilon then pa
    else
      let w0 := (tb - tau) / (tb - ta)
      let w1 := (tau - ta) / (tb - ta)
      pa.scale w0 |>.add (pb.scale w1)

  let a1 := blend p0 p1 t0 t1
  let a2 := blend p1 p2 t1 t2
  let a3 := blend p2 p3 t2 t3
  let b1 := blend a1 a2 t0 t2
  let b2 := blend a2 a3 t1 t3
  let c := blend b1 b2 t1 t2
  c

/-- Evaluate an alpha-parameterized spline path. -/
private def evalAlphaSpline (points : Array Vec2) (closed : Bool) (alpha t : Float) : Vec2 :=
  let sp : Linalg.SplinePath2 := { points := points, closed := closed }
  let numSegs := Linalg.SplinePath2.segmentCount sp
  if numSegs == 0 then
    points.getD 0 Vec2.zero
  else
    let t' := clamp01 t
    let segFloat := t' * numSegs.toFloat
    let segIndex := Float.min segFloat (numSegs.toFloat - 1.0) |>
      Float.floor |> Float.toUInt64 |> UInt64.toNat
    let localT := segFloat - segIndex.toFloat
    let i := Int.ofNat segIndex
    let p0 := points.getD (Linalg.SplinePath2.wrapIndex sp (i - 1)) Vec2.zero
    let p1 := points.getD (Linalg.SplinePath2.wrapIndex sp i) Vec2.zero
    let p2 := points.getD (Linalg.SplinePath2.wrapIndex sp (i + 1)) Vec2.zero
    let p3 := points.getD (Linalg.SplinePath2.wrapIndex sp (i + 2)) Vec2.zero
    catmullRomEvalAlphaSegment p0 p1 p2 p3 alpha localT

private def sampleSpline (evalFn : Float → Vec2) (segments : Nat := 140) : Array Vec2 := Id.run do
  let steps := if segments < 2 then 2 else segments
  let mut pts : Array Vec2 := #[]
  for i in [:steps] do
    let t := i.toFloat / (steps - 1).toFloat
    pts := pts.push (evalFn t)
  return pts

/-- Render spline editor. -/
def renderCatmullRomSplineEditor (state : CatmullRomSplineEditorState)
    (view : MathView2D.View) (screenScale : Float) (fontMedium fontSmall : Font) : CanvasM Unit := do
  let w := view.width
  let h := view.height
  let origin : Float × Float := (view.origin.x, view.origin.y)
  let scale := view.scale
  let t := clamp01 state.t

  -- Control polygon
  if state.points.size > 1 then
    let mut poly : Array Vec2 := state.points
    if state.closed then
      poly := poly.push (state.points.getD 0 Vec2.zero)
    drawPolylineWorld poly origin scale (Color.gray 0.4) 1.2

  -- Control points
  for i in [:state.points.size] do
    let p := state.points.getD i Vec2.zero
    let color := if i == 0 then Color.rgba 1.0 0.3 0.3 1.0
      else if i == state.points.size - 1 then Color.rgba 0.3 0.9 0.3 1.0
      else Color.rgba 0.4 0.7 1.0 1.0
    drawPointWorld p origin scale color 6.0

  let spline : Linalg.SplinePath2 := { points := state.points, closed := state.closed }
  let uniformPts := sampleSpline (fun tt => Linalg.SplinePath2.eval spline tt) 140
  let alphaPts := sampleSpline (fun tt => evalAlphaSpline state.points state.closed state.alpha tt) 140

  -- Uniform spline (baseline)
  drawPolylineWorld uniformPts origin scale (Color.rgba 0.6 0.6 0.6 0.6) 1.5
  -- Alpha-parameterized spline
  drawPolylineWorld alphaPts origin scale (Color.rgba 0.2 0.9 1.0 1.0) 2.5

  -- Moving markers
  let posUniform := Linalg.SplinePath2.eval spline t
  let posAlpha := evalAlphaSpline state.points state.closed state.alpha t
  drawPointWorld posUniform origin scale (Color.rgba 0.6 0.6 0.6 1.0) 5.0
  drawPointWorld posAlpha origin scale (Color.white) 6.5

  -- Slider
  let layout := alphaSliderLayout w h screenScale
  let active := match state.dragging with | .slider => true | _ => false
  renderAlphaSlider state.alpha layout fontSmall active

  -- Labels
  setFillColor VecColor.label
  fillTextXY "CATMULL-ROM SPLINE EDITOR" (20 * screenScale) (30 * screenScale) fontMedium
  setFillColor (Color.gray 0.6)
  fillTextXY "Click to add | Drag points | C: toggle closed | Backspace: delete | Space: animate"
    (20 * screenScale) (55 * screenScale) fontSmall
  setFillColor VecColor.label
  fillTextXY s!"alpha={formatFloat state.alpha}  closed={state.closed}  points={state.points.size}"
    (20 * screenScale) (h - 40 * screenScale) fontSmall

/-- Create spline editor widget. -/
def catmullRomSplineEditorWidget (env : DemoEnv) (state : CatmullRomSplineEditorState)
    : Afferent.Arbor.WidgetBuilder := do
  let config := catmullRomMathViewConfig env.screenScale
  MathView2D.mathView2D config env.fontSmall (fun view => do
    renderCatmullRomSplineEditor state view env.screenScale env.fontMedium env.fontSmall
  )

end Demos.Linalg
