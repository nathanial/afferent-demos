/- 
  Arc-Length Parameterization - compare uniform t vs arc-length parameterization.
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

/-- Drag state for arc-length demo. -/
inductive ArcLengthDragMode where
  | none
  | point (idx : Nat)
  | slider
  deriving BEq, Inhabited

/-- State for arc-length parameterization demo. -/
structure ArcLengthParameterizationState where
  controlPoints : Array Vec2 := #[
    Vec2.mk (-2.8) (-1.6), Vec2.mk (-1.2) 2.2, Vec2.mk (1.4) (-2.0), Vec2.mk (2.8) 1.6
  ]
  t : Float := 0.0
  s : Float := 0.0
  speed : Float := 1.2
  animating : Bool := true
  dragging : ArcLengthDragMode := .none
  deriving Inhabited


def arcLengthParameterizationInitialState : ArcLengthParameterizationState := {}

private def clamp01 (t : Float) : Float :=
  if t < 0.0 then 0.0 else if t > 1.0 then 1.0 else t

/-- Slider layout for speed. -/
structure SpeedSliderLayout where
  x : Float
  y : Float
  width : Float
  height : Float

private def speedSliderLayout (w _h screenScale : Float) : SpeedSliderLayout :=
  { x := w - 260.0 * screenScale
    y := 95.0 * screenScale
    width := 190.0 * screenScale
    height := 8.0 * screenScale }

private def renderSpeedSlider (value : Float) (layout : SpeedSliderLayout) (fontSmall : Font)
    (active : Bool := false) : CanvasM Unit := do
  let t := clamp01 ((value - 0.2) / 3.8)
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
  fillTextXY "speed" (layout.x - 46.0) (layout.y + 6.0) fontSmall

private def speedFromSlider (t : Float) : Float :=
  0.2 + clamp01 t * 3.8

private def speedToSlider (speed : Float) : Float :=
  clamp01 ((speed - 0.2) / 3.8)

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

private def sampleCurve (evalFn : Float → Vec2) (segments : Nat := 140) : Array Vec2 := Id.run do
  let steps := if segments < 2 then 2 else segments
  let mut pts : Array Vec2 := #[]
  for i in [:steps] do
    let t := i.toFloat / (steps - 1).toFloat
    pts := pts.push (evalFn t)
  return pts

/-- Render arc-length parameterization demo. -/
def renderArcLengthParameterization (state : ArcLengthParameterizationState)
    (w h : Float) (screenScale : Float) (fontMedium fontSmall : Font) : CanvasM Unit := do
  let origin : Float × Float := (w / 2, h / 2)
  let scale : Float := 70.0 * screenScale

  drawGrid2D {
    origin := origin
    scale := scale
    width := w
    height := h
    majorSpacing := 2.0
  } fontSmall

  let p0 := state.controlPoints.getD 0 Vec2.zero
  let p1 := state.controlPoints.getD 1 Vec2.zero
  let p2 := state.controlPoints.getD 2 Vec2.zero
  let p3 := state.controlPoints.getD 3 Vec2.zero
  let curve := Bezier3.mk p0 p1 p2 p3
  let evalFn := fun t => Bezier3.evalVec2 curve t
  let arcTable := Linalg.ArcLengthTable.build evalFn 120

  -- Control polygon
  drawPolylineWorld state.controlPoints origin scale (Color.gray 0.4) 1.2
  for i in [:state.controlPoints.size] do
    let p := state.controlPoints.getD i Vec2.zero
    let color := if i == 0 then Color.rgba 1.0 0.3 0.3 1.0
      else if i == state.controlPoints.size - 1 then Color.rgba 0.3 0.9 0.3 1.0
      else Color.rgba 0.4 0.7 1.0 1.0
    drawPointWorld p origin scale color 6.0

  -- Curve
  let curvePts := sampleCurve evalFn 140
  drawPolylineWorld curvePts origin scale (Color.rgba 0.2 0.9 1.0 1.0) 2.4

  -- Uniform vs arc-length markers
  let tUniform := clamp01 state.t
  let posUniform := evalFn tUniform
  let tArc := Linalg.ArcLengthTable.sToT arcTable state.s
  let posArc := evalFn tArc
  drawPointWorld posUniform origin scale (Color.rgba 0.6 0.6 0.6 1.0) 5.0
  drawPointWorld posArc origin scale Color.white 6.5

  -- Show evenly spaced markers
  for i in [:10] do
    let u := i.toFloat / 9.0
    let tu := Linalg.ArcLengthTable.uToT arcTable u
    let pU := evalFn tu
    drawPointWorld pU origin scale (Color.rgba 1.0 0.7 0.2 0.8) 3.5

  -- Speed slider
  let layout := speedSliderLayout w h screenScale
  let active := match state.dragging with | .slider => true | _ => false
  renderSpeedSlider state.speed layout fontSmall active

  -- Labels
  setFillColor VecColor.label
  fillTextXY "ARC-LENGTH PARAMETERIZATION" (20 * screenScale) (30 * screenScale) fontMedium
  setFillColor (Color.gray 0.6)
  fillTextXY "Drag points | Space: animate | t (gray) vs arc-length (white)"
    (20 * screenScale) (55 * screenScale) fontSmall
  setFillColor VecColor.label
  fillTextXY s!"totalLength={formatFloat arcTable.totalLength}  speed={formatFloat state.speed}"
    (20 * screenScale) (h - 40 * screenScale) fontSmall

/-- Create arc-length parameterization widget. -/
def arcLengthParameterizationWidget (env : DemoEnv) (state : ArcLengthParameterizationState)
    : Afferent.Arbor.WidgetBuilder := do
  Afferent.Arbor.custom (spec := {
    measure := fun _ _ => (0, 0)
    collect := fun _ => #[]
    draw := some (fun layout => do
      withContentRect layout fun w h => do
        resetTransform
        renderArcLengthParameterization state w h env.screenScale env.fontMedium env.fontSmall
    )
  }) (style := { flexItem := some (Trellis.FlexItem.growing 1) })

end Demos.Linalg
