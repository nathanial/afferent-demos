/-
  Euler Gimbal Lock - Nested gimbal visualization showing loss of DOF at 90°.
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
import Linalg.Mat3
import Linalg.Euler
import Linalg.Quat
import AfferentMath.Widget.MathView3D

open Afferent CanvasM Linalg
open Afferent.Widget
open AfferentMath.Widget

namespace Demos.Linalg

/-- Axis enumeration for gimbal rings. -/
inductive Axis where
  | x | y | z
  deriving BEq, Inhabited

/-- Axis to vector. -/
def axisVec : Axis → Vec3
  | .x => Vec3.unitX
  | .y => Vec3.unitY
  | .z => Vec3.unitZ

/-- Axis to color. -/
def axisColor : Axis → Color
  | .x => VecColor.xAxis
  | .y => VecColor.yAxis
  | .z => VecColor.zAxis

/-- Rotation matrix for a given axis. -/
def axisRotation (axis : Axis) (angle : Float) : Mat3 :=
  let c := Float.cos angle
  let s := Float.sin angle
  match axis with
  | .x => Mat3.fromColumns
      (Vec3.mk 1 0 0)
      (Vec3.mk 0 c s)
      (Vec3.mk 0 (-s) c)
  | .y => Mat3.fromColumns
      (Vec3.mk c 0 (-s))
      (Vec3.mk 0 1 0)
      (Vec3.mk s 0 c)
  | .z => Mat3.fromColumns
      (Vec3.mk c s 0)
      (Vec3.mk (-s) c 0)
      (Vec3.mk 0 0 1)

/-- Map Euler order to axis sequence. -/
def orderAxes : EulerOrder → Axis × Axis × Axis
  | .XYZ => (.x, .y, .z)
  | .XZY => (.x, .z, .y)
  | .YXZ => (.y, .x, .z)
  | .YZX => (.y, .z, .x)
  | .ZXY => (.z, .x, .y)
  | .ZYX => (.z, .y, .x)

/-- State for gimbal lock demo. -/
structure EulerGimbalLockState where
  euler : Euler := Euler.fromDegrees 30 85 20 .XYZ
  selectedAxis : Nat := 1
  cameraYaw : Float := 0.4
  cameraPitch : Float := 0.3
  dragging : Bool := false
  lastMouseX : Float := 0.0
  lastMouseY : Float := 0.0
  deriving Inhabited

/-- Initial state. -/
def eulerGimbalLockInitialState : EulerGimbalLockState := {}

def eulerGimbalLockMathViewConfig (state : EulerGimbalLockState) (screenScale : Float)
    : MathView3D.Config := {
  style := { flexItem := some (Trellis.FlexItem.growing 1) }
  camera := { yaw := state.cameraYaw, pitch := state.cameraPitch, distance := 7.5 }
  showGrid := false
  showAxes := false
  axisLineWidth := 2.0 * screenScale
}

/-- Render the gimbal lock visualization. -/
def renderEulerGimbalLock (state : EulerGimbalLockState)
    (view : MathView3D.View) (screenScale : Float) (fontMedium fontSmall : Font) : CanvasM Unit := do
  let h := view.height

  -- Draw base axes
  rotDraw3DAxes view 2.5 fontSmall

  let (a1, a2, a3) := (state.euler.a1, state.euler.a2, state.euler.a3)
  let (axis1, axis2, axis3) := orderAxes state.euler.order

  let R1 := axisRotation axis1 a1
  let R2 := axisRotation axis2 a2
  let R3 := axisRotation axis3 a3
  let R12 := R1 * R2
  let _R := R12 * R3

  let g1 := axisVec axis1
  let g2 := R1.transformVec3 (axisVec axis2)
  let g3 := R12.transformVec3 (axisVec axis3)

  -- Draw gimbal rings
  rotDrawCircle3D Vec3.zero g1 1.2 view 64 (axisColor axis1) 2.0
  rotDrawCircle3D Vec3.zero g2 1.0 view 64 (axisColor axis2) 2.0
  rotDrawCircle3D Vec3.zero g3 0.8 view 64 (axisColor axis3) 2.0

  -- Draw rotated airplane axes using quaternion
  let q := state.euler.toQuat
  let forward := q.rotateVec3 Vec3.unitZ
  let up := q.rotateVec3 Vec3.unitY
  rotDraw3DArrow view (forward.scale 2.0)
    { color := Color.rgba 1.0 0.6 0.2 0.9, lineWidth := 2.5 }
  rotDraw3DArrow view (up.scale 1.5)
    { color := Color.rgba 0.7 0.7 1.0 0.9, lineWidth := 2.0 }

  -- Gimbal lock detection (middle axis near 90 degrees)
  let lock := Float.abs (Float.abs a2 - Float.halfPi) < 0.05
  let infoY := h - 160 * screenScale
  setFillColor VecColor.label
  fillTextXY s!"Order: {state.euler.order}" (20 * screenScale) infoY fontSmall
  let deg1 := a1 * 180.0 / Float.pi
  let deg2 := a2 * 180.0 / Float.pi
  let deg3 := a3 * 180.0 / Float.pi
  fillTextXY s!"Angles (deg): {formatFloat deg1}, {formatFloat deg2}, {formatFloat deg3}"
    (20 * screenScale) (infoY + 22 * screenScale) fontSmall
  if lock then
    setFillColor (Color.rgba 1.0 0.3 0.3 1.0)
    fillTextXY "GIMBAL LOCK: middle axis at ±90°" (20 * screenScale) (infoY + 44 * screenScale) fontSmall
  else
    setFillColor (Color.gray 0.7)
    fillTextXY "No gimbal lock" (20 * screenScale) (infoY + 44 * screenScale) fontSmall

  -- Title and instructions
  setFillColor VecColor.label
  fillTextXY "EULER GIMBAL LOCK" (20 * screenScale) (30 * screenScale) fontMedium
  setFillColor (Color.gray 0.6)
  fillTextXY "1/2/3 select axis | ←/→ adjust | O: cycle order | Drag: rotate view" (20 * screenScale) (55 * screenScale) fontSmall

/-- Create the gimbal lock widget. -/
def eulerGimbalLockWidget (env : DemoEnv) (state : EulerGimbalLockState)
    : Afferent.Arbor.WidgetBuilder := do
  let config := eulerGimbalLockMathViewConfig state env.screenScale
  MathView3D.mathView3D config env.fontSmall (fun view => do
    renderEulerGimbalLock state view env.screenScale env.fontMedium env.fontSmall
  )

end Demos.Linalg
