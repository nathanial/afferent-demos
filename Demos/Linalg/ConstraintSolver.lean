/-
  Constraint Solver Demo - simple distance constraints between particles.
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

/-- State for constraint solver demo. -/
structure ConstraintSolverState where
  positions : Array Vec2
  velocities : Array Vec2
  constraints : Array (Nat × Nat × Float)
  pinned : Array Nat
  dragging : Option Nat := none
  animating : Bool := true
  iterations : Nat := 6
  time : Float := 0.0
  deriving Inhabited

private def defaultPositions : Array Vec2 := #[(Vec2.mk (-3.0) 2.0), (Vec2.mk (-1.0) 1.0),
  (Vec2.mk (1.0) 0.2), (Vec2.mk (3.0) (-0.6))]

private def defaultConstraints : Array (Nat × Nat × Float) := #[(0, 1, 2.2), (1, 2, 2.2), (2, 3, 2.2)]

/-- Initial state. -/
def constraintSolverInitialState : ConstraintSolverState := {
  positions := defaultPositions
  velocities := Array.replicate defaultPositions.size Vec2.zero
  constraints := defaultConstraints
  pinned := #[0]
}

def constraintSolverMathViewConfig (screenScale : Float) : MathView2D.Config := {
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

private def isPinned (state : ConstraintSolverState) (idx : Nat) : Bool :=
  state.pinned.any (· == idx)

private def applyConstraints (state : ConstraintSolverState) : Array Vec2 := Id.run do
  let mut positions := state.positions
  for _iter in [:state.iterations] do
    for (aIdx, bIdx, restLen) in state.constraints do
      let a := positions.getD aIdx Vec2.zero
      let b := positions.getD bIdx Vec2.zero
      let delta := b.sub a
      let dist := delta.length
      if dist > 0.0001 then
        let diff := (dist - restLen) / dist
        let correction := delta.scale (0.5 * diff)
        let aPinned := isPinned state aIdx
        let bPinned := isPinned state bIdx
        if !aPinned && !bPinned then
          positions := positions.set! aIdx (a.add correction) |>.set! bIdx (b.sub correction)
        else if aPinned && !bPinned then
          positions := positions.set! bIdx (b.sub (correction.scale 2.0))
        else if !aPinned && bPinned then
          positions := positions.set! aIdx (a.add (correction.scale 2.0))
  return positions

def stepConstraintSolver (state : ConstraintSolverState) (dt : Float)
    : ConstraintSolverState :=
  let dt' := Float.min dt 0.033
  if dt' <= 0.0 then state
  else if !state.animating then { state with time := state.time + dt' }
  else
    Id.run do
      let gravity := Vec2.mk 0.0 (-3.2)
      let mut positions := state.positions
      let mut velocities := state.velocities
      for i in [:positions.size] do
        if !isPinned state i then
          let v := velocities[i]!.add (gravity.scale dt')
          let p := positions[i]!.add (v.scale dt')
          velocities := velocities.set! i v
          positions := positions.set! i p
      let corrected := applyConstraints { state with positions := positions }
      let mut newVelocities := velocities
      for i in [:corrected.size] do
        let newV := (corrected[i]!.sub positions[i]!).scale (1.0 / dt')
        newVelocities := newVelocities.set! i newV
      return { state with positions := corrected, velocities := newVelocities, time := state.time + dt' }

/-- Render constraint solver demo. -/
def renderConstraintSolver (state : ConstraintSolverState)
    (view : MathView2D.View) (screenScale : Float) (fontMedium fontSmall : Font) : CanvasM Unit := do
  let w := view.width
  let h := view.height
  let origin : Float × Float := (view.origin.x, view.origin.y)
  let scale := view.scale

  for (aIdx, bIdx, restLen) in state.constraints do
    let a := state.positions.getD aIdx Vec2.zero
    let b := state.positions.getD bIdx Vec2.zero
    let dist := a.distance b
    let error := dist - restLen
    let color := if error > 0.0 then Color.rgba 1.0 0.5 0.2 0.9 else Color.rgba 0.3 0.9 0.5 0.9
    drawDashedLine (worldToScreen a origin scale) (worldToScreen b origin scale) color 6.0 4.0 2.0
    let mid := a.lerp b 0.5
    let endVec := mid.add ((b.sub a).normalize.scale (Float.min (Float.abs error) 0.6))
    drawArrow2D (worldToScreen mid origin scale) (worldToScreen endVec origin scale)
      { color := color, lineWidth := 1.8 }

  for i in [:state.positions.size] do
    let p := state.positions[i]!
    let color := if isPinned state i then Color.rgba 1.0 0.9 0.2 1.0 else Color.white
    drawMarker p origin scale color 8.0

  let infoY := h - 120 * screenScale
  setFillColor VecColor.label
  fillTextXY s!"particles: {state.positions.size} | constraints: {state.constraints.size}" (20 * screenScale)
    infoY fontSmall
  fillTextXY s!"iterations: {state.iterations}" (20 * screenScale) (infoY + 20 * screenScale) fontSmall

  fillTextXY "CONSTRAINT SOLVER" (20 * screenScale) (30 * screenScale) fontMedium
  setFillColor (Color.gray 0.6)
  let animText := if state.animating then "animating" else "paused"
  fillTextXY s!"Space: {animText} | Drag points | +/- iterations" (20 * screenScale) (55 * screenScale)
    fontSmall

/-- Create constraint solver widget. -/
def constraintSolverWidget (env : DemoEnv) (state : ConstraintSolverState)
    : Afferent.Arbor.WidgetBuilder := do
  let config := constraintSolverMathViewConfig env.screenScale
  MathView2D.mathView2D config env.fontSmall (fun view => do
    renderConstraintSolver state view env.screenScale env.fontMedium env.fontSmall
  )

end Demos.Linalg
