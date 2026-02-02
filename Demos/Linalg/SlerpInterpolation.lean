/-
  SLERP vs LERP Interpolation - compares quaternion interpolation paths.
  Shows constant angular velocity of SLERP vs non-uniform LERP.
-/
import Afferent
import Afferent.Widget
import Afferent.Arbor
import Demos.Core.Demo
import Demos.Linalg.Shared
import Demos.Linalg.RotationShared
import Trellis
import Linalg.Core
import Linalg.Vec2
import Linalg.Vec3
import Linalg.Quat

open Afferent CanvasM Linalg
open Afferent.Widget

namespace Demos.Linalg

/-- State for slerp vs lerp demo. -/
structure SlerpInterpolationState where
  quatA : Quat := Quat.fromAxisAngle Vec3.unitY (Float.pi / 4)
  quatB : Quat := Quat.fromAxisAngle Vec3.unitX (Float.pi * 0.75)
  t : Float := 0.0
  animating : Bool := true
  cameraYaw : Float := 0.5
  cameraPitch : Float := 0.3
  dragging : Bool := false
  lastMouseX : Float := 0.0
  lastMouseY : Float := 0.0
  deriving Inhabited

def slerpInterpolationInitialState : SlerpInterpolationState := {}

def slerpInterpolationMathViewConfig (state : SlerpInterpolationState) (screenScale : Float)
    : MathView3D.Config := {
  style := { flexItem := some (Trellis.FlexItem.growing 1) }
  camera := { yaw := state.cameraYaw, pitch := state.cameraPitch, distance := 8.0 }
  showGrid := false
  showAxes := false
  axisLineWidth := 2.0 * screenScale
}

/-- Draw a polyline from sampled 3D points. -/
private def drawPolyline3D (view : MathView3D.View) (points : Array Vec3)
    (color : Color) (lineWidth : Float := 2.0) : CanvasM Unit := do
  MathView3D.drawPolyline3D view points color lineWidth

/-- Render the slerp vs lerp visualization. -/
def renderSlerpInterpolation (state : SlerpInterpolationState)
    (view : MathView3D.View) (screenScale : Float) (fontMedium fontSmall : Font) : CanvasM Unit := do
  let h := view.height

  -- Draw sphere rings
  rotDrawSphereRings view 1.0

  -- Endpoints (rotate forward vector)
  let forward := Vec3.unitZ
  let aDir := state.quatA.rotateVec3 forward
  let bDir := state.quatB.rotateVec3 forward

  -- Sample interpolation paths
  let sampleCount := 48
  let mut lerpPts : Array Vec3 := #[]
  let mut slerpPts : Array Vec3 := #[]
  for i in [:sampleCount + 1] do
    let t := i.toFloat / sampleCount.toFloat
    let qL := Quat.lerp state.quatA state.quatB t
    let qS := Quat.slerp state.quatA state.quatB t
    lerpPts := lerpPts.push (qL.rotateVec3 forward)
    slerpPts := slerpPts.push (qS.rotateVec3 forward)

  drawPolyline3D view lerpPts (Color.rgba 0.9 0.4 0.3 0.8) (2.0 * screenScale)
  drawPolyline3D view slerpPts (Color.rgba 0.3 0.9 0.5 0.9) (2.5 * screenScale)

  -- Current interpolation points
  let qL := Quat.lerp state.quatA state.quatB state.t
  let qS := Quat.slerp state.quatA state.quatB state.t
  let pL := qL.rotateVec3 forward
  let pS := qS.rotateVec3 forward

  match rotProject3Dto2D view pL with
  | some (lx, ly) =>
      setFillColor (Color.rgba 0.9 0.4 0.3 1.0)
      fillPath (Afferent.Path.circle (Point.mk lx ly) (6.0 * screenScale))
  | none => pure ()
  match rotProject3Dto2D view pS with
  | some (sx, sy) =>
      setFillColor (Color.rgba 0.3 0.9 0.5 1.0)
      fillPath (Afferent.Path.circle (Point.mk sx sy) (6.0 * screenScale))
  | none => pure ()

  -- Draw endpoints
  match rotProject3Dto2D view aDir with
  | some (ax, ay) =>
      setFillColor VecColor.vectorA
      fillPath (Afferent.Path.circle (Point.mk ax ay) (6.0 * screenScale))
  | none => pure ()
  match rotProject3Dto2D view bDir with
  | some (bx, byy) =>
      setFillColor VecColor.vectorB
      fillPath (Afferent.Path.circle (Point.mk bx byy) (6.0 * screenScale))
  | none => pure ()

  -- Angle metrics
  let angleL := Float.acos (Float.clamp (Vec3.dot aDir pL) (-1.0) 1.0)
  let angleS := Float.acos (Float.clamp (Vec3.dot aDir pS) (-1.0) 1.0)
  let angleLDeg := angleL * 180.0 / Float.pi
  let angleSDeg := angleS * 180.0 / Float.pi

  let infoY := h - 140 * screenScale
  setFillColor VecColor.label
  fillTextXY s!"t = {formatFloat state.t}" (20 * screenScale) infoY fontSmall
  fillTextXY s!"LERP angle from A: {formatFloat angleLDeg} deg" (20 * screenScale) (infoY + 22 * screenScale) fontSmall
  fillTextXY s!"SLERP angle from A: {formatFloat angleSDeg} deg" (20 * screenScale) (infoY + 44 * screenScale) fontSmall

  -- Title and instructions
  fillTextXY "SLERP vs LERP (Quaternion)" (20 * screenScale) (30 * screenScale) fontMedium
  setFillColor (Color.gray 0.6)
  fillTextXY "Space: toggle animation | Drag: rotate view" (20 * screenScale) (55 * screenScale) fontSmall

/-- Create the slerp interpolation widget. -/
def slerpInterpolationWidget (env : DemoEnv) (state : SlerpInterpolationState)
    : Afferent.Arbor.WidgetBuilder := do
  let config := slerpInterpolationMathViewConfig state env.screenScale
  MathView3D.mathView3D config env.fontSmall (fun view => do
    renderSlerpInterpolation state view env.screenScale env.fontMedium env.fontSmall
  )

end Demos.Linalg
