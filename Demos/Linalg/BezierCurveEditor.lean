/- 
  Bezier Curve Editor - Quadratic/Cubic Bezier with draggable control points.
  Shows control polygon, tangent, and de Casteljau construction at parameter t.
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

namespace Demos.Linalg

/-- Bezier curve mode. -/
inductive BezierCurveMode where
  | quadratic
  | cubic
  deriving BEq, Inhabited

/-- Drag mode for Bezier editor. -/
inductive BezierDragMode where
  | none
  | control (idx : Nat)
  | slider
  deriving BEq, Inhabited

/-- State for Bezier curve editor. -/
structure BezierCurveEditorState where
  mode : BezierCurveMode := .cubic
  quadPoints : Array Vec2 := #[Vec2.mk (-2.2) (-1.4), Vec2.mk 0.4 2.1, Vec2.mk 2.3 (-1.2)]
  cubicPoints : Array Vec2 := #[Vec2.mk (-2.6) (-1.6), Vec2.mk (-1.2) 2.2, Vec2.mk 1.4 2.0, Vec2.mk 2.6 (-1.2)]
  t : Float := 0.35
  dragging : BezierDragMode := .none
  deriving Inhabited

def bezierCurveEditorInitialState : BezierCurveEditorState := {}

private def clamp01 (t : Float) : Float :=
  if t < 0.0 then 0.0 else if t > 1.0 then 1.0 else t

private def activePoints (state : BezierCurveEditorState) : Array Vec2 :=
  match state.mode with
  | .quadratic => state.quadPoints
  | .cubic => state.cubicPoints

private def updatePoint (state : BezierCurveEditorState) (idx : Nat) (pos : Vec2)
    : BezierCurveEditorState :=
  match state.mode with
  | .quadratic =>
      if idx < state.quadPoints.size then
        { state with quadPoints := state.quadPoints.set! idx pos }
      else state
  | .cubic =>
      if idx < state.cubicPoints.size then
        { state with cubicPoints := state.cubicPoints.set! idx pos }
      else state

/-- Slider geometry. -/
structure BezierSliderLayout where
  x : Float
  y : Float
  width : Float
  height : Float

private def sliderLayout (w _h screenScale : Float) : BezierSliderLayout :=
  { x := w - 250.0 * screenScale
    y := 95.0 * screenScale
    width := 180.0 * screenScale
    height := 8.0 * screenScale }

private def renderSlider (value : Float) (layout : BezierSliderLayout) (fontSmall : Font)
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
  fillTextXY "t" (layout.x - 18.0) (layout.y + 6.0) fontSmall

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

private def drawArrowFrom (start : Vec2) (vec : Vec2) (origin : Float × Float)
    (scale : Float) (config : ArrowConfig := {}) : CanvasM Unit := do
  let s := worldToScreen start origin scale
  let e := worldToScreen (start + vec) origin scale
  drawArrow2D s e config

private def sampleBezier2 (b : Bezier2 Vec2) (segments : Nat) : Array Vec2 := Id.run do
  let steps := if segments < 2 then 2 else segments
  let mut pts : Array Vec2 := #[]
  for i in [:steps] do
    let t := i.toFloat / (steps - 1).toFloat
    pts := pts.push (Bezier2.evalVec2 b t)
  return pts

private def sampleBezier3 (b : Bezier3 Vec2) (segments : Nat) : Array Vec2 := Id.run do
  let steps := if segments < 2 then 2 else segments
  let mut pts : Array Vec2 := #[]
  for i in [:steps] do
    let t := i.toFloat / (steps - 1).toFloat
    pts := pts.push (Bezier3.evalVec2 b t)
  return pts

/-- Render Bezier curve editor. -/
def renderBezierCurveEditor (state : BezierCurveEditorState)
    (w h : Float) (screenScale : Float) (fontMedium fontSmall : Font) : CanvasM Unit := do
  let origin : Float × Float := (w / 2, h / 2)
  let scale : Float := 70.0 * screenScale
  let t := clamp01 state.t

  drawGrid2D {
    origin := origin
    scale := scale
    width := w
    height := h
    majorSpacing := 2.0
  } fontSmall

  let points := activePoints state

  -- Control polygon
  drawPolylineWorld points origin scale (Color.gray 0.5) 1.5
  for i in [:points.size] do
    let p := points.getD i Vec2.zero
    let color := if i == 0 then Color.rgba 1.0 0.3 0.3 1.0
      else if i == points.size - 1 then Color.rgba 0.3 0.9 0.3 1.0
      else Color.rgba 0.4 0.7 1.0 1.0
    drawPointWorld p origin scale color 6.0

  match state.mode with
  | .quadratic =>
      let p0 := points.getD 0 Vec2.zero
      let p1 := points.getD 1 Vec2.zero
      let p2 := points.getD 2 Vec2.zero
      let bez := Bezier2.mk p0 p1 p2
      let curve := sampleBezier2 bez 120
      drawPolylineWorld curve origin scale (Color.rgba 0.2 0.9 1.0 1.0) 2.5

      -- De Casteljau construction
      let p01 := p0.lerp p1 t
      let p12 := p1.lerp p2 t
      let p012 := p01.lerp p12 t
      drawPolylineWorld #[p01, p12] origin scale (Color.rgba 1.0 0.8 0.2 1.0) 1.5
      drawPointWorld p01 origin scale (Color.rgba 1.0 0.7 0.2 1.0) 4.5
      drawPointWorld p12 origin scale (Color.rgba 1.0 0.7 0.2 1.0) 4.5
      drawPointWorld p012 origin scale Color.white 6.5

      -- Split visualization
      let (left, right) := Bezier2.splitVec2 bez t
      let leftPts := sampleBezier2 left 60
      let rightPts := sampleBezier2 right 60
      drawPolylineWorld leftPts origin scale (Color.rgba 1.0 0.4 0.4 0.9) 2.0
      drawPolylineWorld rightPts origin scale (Color.rgba 0.4 0.8 1.0 0.9) 2.0

      -- Tangent
      let tan := Bezier2.derivativeVec2 bez t |>.normalize
      drawArrowFrom p012 (tan.scale 0.9) origin scale { color := Color.yellow, lineWidth := 2.0 }

  | .cubic =>
      let p0 := points.getD 0 Vec2.zero
      let p1 := points.getD 1 Vec2.zero
      let p2 := points.getD 2 Vec2.zero
      let p3 := points.getD 3 Vec2.zero
      let bez := Bezier3.mk p0 p1 p2 p3
      let curve := sampleBezier3 bez 140
      drawPolylineWorld curve origin scale (Color.rgba 0.25 0.95 0.85 1.0) 2.5

      -- De Casteljau construction
      let p01 := p0.lerp p1 t
      let p12 := p1.lerp p2 t
      let p23 := p2.lerp p3 t
      let p012 := p01.lerp p12 t
      let p123 := p12.lerp p23 t
      let p0123 := p012.lerp p123 t
      drawPolylineWorld #[p01, p12, p23] origin scale (Color.rgba 1.0 0.7 0.2 0.9) 1.2
      drawPolylineWorld #[p012, p123] origin scale (Color.rgba 1.0 0.85 0.4 1.0) 1.5
      drawPointWorld p01 origin scale (Color.rgba 1.0 0.6 0.2 1.0) 4.0
      drawPointWorld p12 origin scale (Color.rgba 1.0 0.6 0.2 1.0) 4.0
      drawPointWorld p23 origin scale (Color.rgba 1.0 0.6 0.2 1.0) 4.0
      drawPointWorld p012 origin scale (Color.rgba 1.0 0.8 0.4 1.0) 4.5
      drawPointWorld p123 origin scale (Color.rgba 1.0 0.8 0.4 1.0) 4.5
      drawPointWorld p0123 origin scale Color.white 6.5

      -- Split visualization
      let (left, right) := Bezier3.splitVec2 bez t
      let leftPts := sampleBezier3 left 70
      let rightPts := sampleBezier3 right 70
      drawPolylineWorld leftPts origin scale (Color.rgba 1.0 0.4 0.4 0.85) 2.0
      drawPolylineWorld rightPts origin scale (Color.rgba 0.4 0.8 1.0 0.85) 2.0

      -- Tangent
      let tan := Bezier3.derivativeVec2 bez t |>.normalize
      drawArrowFrom p0123 (tan.scale 0.9) origin scale { color := Color.yellow, lineWidth := 2.0 }

  -- Slider
  let layout := sliderLayout w h screenScale
  let active := match state.dragging with | .slider => true | _ => false
  renderSlider t layout fontSmall active

  -- Labels
  setFillColor VecColor.label
  fillTextXY (match state.mode with | .quadratic => "BEZIER CURVE EDITOR (Quadratic)" | .cubic => "BEZIER CURVE EDITOR (Cubic)")
    (20 * screenScale) (30 * screenScale) fontMedium
  setFillColor (Color.gray 0.6)
  fillTextXY "Drag control points | Q/C toggle | t slider controls de Casteljau"
    (20 * screenScale) (55 * screenScale) fontSmall
  setFillColor VecColor.label
  fillTextXY s!"t={formatFloat t}" (20 * screenScale) (h - 40 * screenScale) fontSmall

/-- Create the Bezier curve editor widget. -/
def bezierCurveEditorWidget (env : DemoEnv) (state : BezierCurveEditorState)
    : Afferent.Arbor.WidgetBuilder := do
  Afferent.Arbor.custom (spec := {
    measure := fun _ _ => (0, 0)
    collect := fun _ => #[]
    draw := some (fun layout => do
      withContentRect layout fun w h => do
        resetTransform
        renderBezierCurveEditor state w h env.screenScale env.fontMedium env.fontSmall
    )
  }) (style := { flexItem := some (Trellis.FlexItem.growing 1) })

end Demos.Linalg
