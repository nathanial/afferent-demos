/-
  SmoothDamp Follower - target point with SmoothDamp following and velocity graph.
-/
import Afferent
import Afferent.Widget
import Afferent.Arbor
import Demos.Core.Demo
import Demos.Linalg.Shared
import Trellis
import Linalg.Core
import Linalg.Vec2
import Linalg.Easing

open Afferent CanvasM Linalg

namespace Demos.Linalg

/-- Slider types for SmoothDamp. -/
inductive SmoothDampSlider where
  | smoothTime
  | maxSpeed
  deriving BEq, Inhabited

/-- Drag state for SmoothDamp follower. -/
inductive SmoothDampDragMode where
  | none
  | target
  | slider (which : SmoothDampSlider)
  deriving BEq, Inhabited

/-- Slider layout parameters. -/
structure SmoothDampSliderLayout where
  x : Float
  y : Float
  width : Float
  height : Float

/-- Simple rectangle helper. -/
structure SmoothDampRect where
  x : Float
  y : Float
  w : Float
  h : Float

/-- Compute slider geometry for SmoothDamp sliders. -/
def smoothDampSliderLayout (w _h screenScale : Float) (idx : Nat) : SmoothDampSliderLayout :=
  let startX := w - 260.0 * screenScale
  let startY := 95.0 * screenScale
  let width := 190.0 * screenScale
  let height := 8.0 * screenScale
  let spacing := 32.0 * screenScale
  { x := startX, y := startY + idx.toFloat * spacing, width := width, height := height }

/-- Map slider t to smoothTime. -/
def smoothDampSmoothTimeFrom (t : Float) : Float :=
  0.08 + Linalg.Float.clamp t 0.0 1.0 * 1.92

/-- Map smoothTime to slider t. -/
def smoothDampSmoothTimeTo (value : Float) : Float :=
  Linalg.Float.clamp ((value - 0.08) / 1.92) 0.0 1.0

/-- Map slider t to maxSpeed. -/
def smoothDampMaxSpeedFrom (t : Float) : Float :=
  0.5 + Linalg.Float.clamp t 0.0 1.0 * 8.0

/-- Map maxSpeed to slider t. -/
def smoothDampMaxSpeedTo (value : Float) : Float :=
  Linalg.Float.clamp ((value - 0.5) / 8.0) 0.0 1.0

/-- State for SmoothDamp follower. -/
structure SmoothDampFollowerState where
  target : Vec2 := Vec2.mk 2.0 1.0
  dampState : SmoothDampState2 := SmoothDampState2.init (Vec2.mk (-2.0) (-1.0))
  smoothTime : Float := 0.6
  maxSpeed : Float := 4.0
  dragging : SmoothDampDragMode := .none
  history : Array Float := #[]
  animating : Bool := true
  deriving Inhabited


def smoothDampFollowerInitialState : SmoothDampFollowerState := {}

private def clamp01 (t : Float) : Float :=
  Linalg.Float.clamp t 0.0 1.0

private def renderSlider (label : String) (value : Float) (layout : SmoothDampSliderLayout)
    (fontSmall : Font) (active : Bool := false) : CanvasM Unit := do
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
  fillTextXY label (layout.x - 70.0) (layout.y + 6.0) fontSmall

private def drawVelocityGraph (history : Array Float) (rect : SmoothDampRect) (color : Color)
    (maxValue : Float) : CanvasM Unit := do
  if history.size < 2 then return
  let mut path := Afferent.Path.empty
  for i in [:history.size] do
    let t := i.toFloat / (history.size - 1).toFloat
    let v := history.getD i 0.0
    let y := rect.y + rect.h - (v / maxValue) * rect.h
    let x := rect.x + t * rect.w
    if i == 0 then
      path := path.moveTo (Point.mk x y)
    else
      path := path.lineTo (Point.mk x y)
  setStrokeColor color
  setLineWidth 1.6
  strokePath path

/-- Render SmoothDamp follower. -/
def renderSmoothDampFollower (state : SmoothDampFollowerState)
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

  let follower := state.dampState.current
  let target := state.target

  -- Connection line
  let (fx, fy) := worldToScreen follower origin scale
  let (tx, ty) := worldToScreen target origin scale
  setStrokeColor (Color.gray 0.4)
  setLineWidth 1.2
  strokePath (Afferent.Path.empty
    |>.moveTo (Point.mk fx fy)
    |>.lineTo (Point.mk tx ty))

  -- Target point
  setFillColor (Color.rgba 1.0 0.3 0.3 1.0)
  fillPath (Afferent.Path.circle (Point.mk tx ty) 7.0)

  -- Follower point
  setFillColor (Color.rgba 0.3 0.9 0.5 1.0)
  fillPath (Afferent.Path.circle (Point.mk fx fy) 7.0)

  -- Velocity vector
  let vel := state.dampState.velocity
  let velTip := follower.add (vel.scale 0.2)
  let (vx, vy) := worldToScreen velTip origin scale
  drawArrow2D (fx, fy) (vx, vy) { color := Color.yellow, lineWidth := 2.0 }

  -- Velocity graph panel
  let graphRect : SmoothDampRect := SmoothDampRect.mk (40.0 * screenScale) (h - 150.0 * screenScale)
    (w - 80.0 * screenScale) (90.0 * screenScale)
  setStrokeColor (Color.gray 0.35)
  setLineWidth 1.0
  strokePath (Afferent.Path.rectangleXYWH graphRect.x graphRect.y graphRect.w graphRect.h)
  let maxV := if state.maxSpeed > 0.5 then state.maxSpeed else 0.5
  drawVelocityGraph state.history graphRect (Color.rgba 0.2 0.9 1.0 0.9) maxV

  -- Sliders
  let layoutSmooth := smoothDampSliderLayout w h screenScale 0
  let layoutMax := smoothDampSliderLayout w h screenScale 1
  let smoothT := smoothDampSmoothTimeTo state.smoothTime
  let maxT := smoothDampMaxSpeedTo state.maxSpeed
  let activeSmooth := match state.dragging with | .slider .smoothTime => true | _ => false
  let activeMax := match state.dragging with | .slider .maxSpeed => true | _ => false
  renderSlider "smoothTime" smoothT layoutSmooth fontSmall activeSmooth
  renderSlider "maxSpeed" maxT layoutMax fontSmall activeMax

  -- Labels
  setFillColor VecColor.label
  fillTextXY "SMOOTH DAMP FOLLOWER" (20 * screenScale) (30 * screenScale) fontMedium
  setFillColor (Color.gray 0.6)
  fillTextXY "Drag target | Space: pause | R: reset" (20 * screenScale) (55 * screenScale) fontSmall
  setFillColor VecColor.label
  fillTextXY s!"smoothTime={formatFloat state.smoothTime}  maxSpeed={formatFloat state.maxSpeed}"
    (20 * screenScale) (h - 40 * screenScale) fontSmall

/-- Create SmoothDamp follower widget. -/
def smoothDampFollowerWidget (env : DemoEnv) (state : SmoothDampFollowerState)
    : Afferent.Arbor.WidgetBuilder := do
  Afferent.Arbor.custom (spec := {
    measure := fun _ _ => (0, 0)
    collect := fun _ => #[]
    draw := some (fun layout => do
      withContentRect layout fun w h => do
        resetTransform
        renderSmoothDampFollower state w h env.screenScale env.fontMedium env.fontSmall
    )
  }) (style := { flexItem := some (Trellis.FlexItem.growing 1) })

end Demos.Linalg
