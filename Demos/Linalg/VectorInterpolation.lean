/-
  Vector Interpolation Demo - Shows lerp between two draggable vectors.
  Press space to toggle animation.
-/
import Afferent
import Afferent.Widget
import Afferent.Arbor
import Demos.Core.Demo
import Demos.Linalg.Shared
import Trellis
import Linalg.Core
import Linalg.Vec2
import AfferentMath.Widget.MathView2D

open Afferent CanvasM Linalg
open Afferent.Widget
open AfferentMath.Widget

namespace Demos.Linalg

/-- Drag target for interpolation demo -/
inductive InterpDragTarget where
  | vectorA
  | vectorB
  deriving BEq, Inhabited

/-- State for vector interpolation demo -/
structure VectorInterpolationState where
  vectorA : Vec2 := Vec2.mk (-3.0) (-1.5)
  vectorB : Vec2 := Vec2.mk 3.0 2.0
  t : Float := 0.0
  animating : Bool := true
  dragging : Option InterpDragTarget := none
  deriving Inhabited

def vectorInterpolationInitialState : VectorInterpolationState := {}

def vectorInterpolationMathViewConfig (screenScale : Float) : MathView2D.Config := {
  style := { flexItem := some (Trellis.FlexItem.growing 1) }
  scale := 50.0 * screenScale
  minorStep := 1.0
  majorStep := 2.0
  gridMinorColor := Color.gray 0.2
  gridMajorColor := Color.gray 0.4
  axisColor := Color.gray 0.6
  labelColor := VecColor.label
  labelPrecision := 0
}

/-- Render the vector interpolation visualization -/
def renderVectorInterpolation (state : VectorInterpolationState)
    (view : MathView2D.View) (screenScale : Float) (fontMedium fontSmall : Font) : CanvasM Unit := do
  let w := view.width
  let h := view.height
  let origin : Float × Float := (view.origin.x, view.origin.y)
  let scale := view.scale

  -- Calculate interpolated point
  let interpPoint := Vec2.lerp state.vectorA state.vectorB state.t

  -- Draw the line from A to B
  let screenA := worldToScreen state.vectorA origin scale
  let screenB := worldToScreen state.vectorB origin scale
  drawDashedLine screenA screenB (Color.gray 0.6) 10.0 5.0 2.0

  -- Draw vector A marker
  drawMarker state.vectorA origin scale VecColor.vectorA 10.0
  -- Draw vector B marker
  drawMarker state.vectorB origin scale VecColor.vectorB 10.0
  -- Draw interpolated point
  drawMarker interpPoint origin scale VecColor.interpolated 8.0

  -- Draw vectors from origin to each point
  drawVectorArrow Vec2.zero state.vectorA origin scale { color := VecColor.vectorA, lineWidth := 2.5 }
  drawVectorArrow Vec2.zero state.vectorB origin scale { color := VecColor.vectorB, lineWidth := 2.5 }
  drawVectorArrow Vec2.zero interpPoint origin scale { color := VecColor.interpolated, lineWidth := 2.5 }

  -- Labels
  let (ax, ay) := worldToScreen state.vectorA origin scale
  let (bx, byy) := worldToScreen state.vectorB origin scale
  let (ix, iy) := worldToScreen interpPoint origin scale

  setFillColor VecColor.vectorA
  fillTextXY "A" (ax + 12) (ay - 8) fontSmall

  setFillColor VecColor.vectorB
  fillTextXY "B" (bx + 12) (byy - 8) fontSmall

  setFillColor VecColor.interpolated
  fillTextXY "lerp" (ix + 12) (iy - 8) fontSmall

  -- Info panel at bottom
  let infoY := h - 100 * screenScale
  setFillColor VecColor.label
  fillTextXY s!"A = {formatVec2 state.vectorA}" (20 * screenScale) infoY fontSmall
  fillTextXY s!"B = {formatVec2 state.vectorB}" (20 * screenScale) (infoY + 20 * screenScale) fontSmall
  fillTextXY s!"t = {formatFloat state.t}" (20 * screenScale) (infoY + 40 * screenScale) fontSmall
  fillTextXY s!"lerp(A, B, t) = {formatVec2 interpPoint}" (20 * screenScale) (infoY + 60 * screenScale) fontSmall

  -- Title and instructions
  fillTextXY "VECTOR INTERPOLATION" (20 * screenScale) (30 * screenScale) fontMedium
  setFillColor (Color.gray 0.6)
  let animText := if state.animating then "animating" else "paused"
  fillTextXY s!"Space: toggle animation ({animText}) | Drag: move A/B endpoints" (20 * screenScale) (55 * screenScale) fontSmall

/-- Create the interpolation widget -/
def vectorInterpolationWidget (env : DemoEnv) (state : VectorInterpolationState)
    : Afferent.Arbor.WidgetBuilder := do
  let config := vectorInterpolationMathViewConfig env.screenScale
  MathView2D.mathView2D config env.fontSmall (fun view => do
    renderVectorInterpolation state view env.screenScale env.fontMedium env.fontSmall
  )

end Demos.Linalg
