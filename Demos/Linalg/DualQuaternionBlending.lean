/-
  Dual Quaternion Blending - compare LBS vs DLB skinning on a two-bone rig.
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
import Linalg.Mat4
import Linalg.Quat
import Linalg.DualQuat
import AfferentMath.Widget.MathView3D

open Afferent CanvasM Linalg
open Afferent.Widget
open AfferentMath.Widget

namespace Demos.Linalg

/-- State for dual quaternion blending demo. -/
structure DualQuaternionBlendingState where
  twist : Float := Float.pi / 2
  bend : Float := Float.pi / 6
  cameraYaw : Float := 0.4
  cameraPitch : Float := 0.3
  dragging : Bool := false
  lastMouseX : Float := 0.0
  lastMouseY : Float := 0.0
  deriving Inhabited

def dualQuaternionBlendingInitialState : DualQuaternionBlendingState := {}

def dualQuaternionBlendingMathViewConfig (state : DualQuaternionBlendingState) (screenScale : Float)
    : MathView3D.Config := {
  style := { flexItem := some (Trellis.FlexItem.growing 1) }
  camera := { yaw := state.cameraYaw, pitch := state.cameraPitch, distance := 9.0 }
  showGrid := false
  showAxes := false
  axisLineWidth := 2.0 * screenScale
}

/-- Draw a polyline from sampled 3D points. -/
private def drawPolyline3D (view : MathView3D.View) (points : Array Vec3)
    (color : Color) (lineWidth : Float := 2.0) : CanvasM Unit := do
  MathView3D.drawPolyline3D view points color lineWidth

/-- Render the dual quaternion blending visualization. -/
def renderDualQuaternionBlending (state : DualQuaternionBlendingState)
    (view : MathView3D.View) (screenScale : Float) (fontMedium fontSmall : Font) : CanvasM Unit := do
  let h := view.height

  -- Bone transforms
  let qTwist0 := Quat.fromAxisAngle Vec3.unitX (-state.twist) |> Quat.normalize
  let qBend := Quat.fromAxisAngle Vec3.unitZ state.bend |> Quat.normalize
  let q0 := Quat.multiply qTwist0 qBend |> Quat.normalize
  let q1Local := Quat.fromAxisAngle Vec3.unitX state.twist |> Quat.normalize

  let mat0 := Mat4.rotationX (-state.twist) * Mat4.rotationZ state.bend
  let mat1Local := Mat4.translation 1 0 0 * Mat4.rotationX state.twist
  let mat1 := mat0 * mat1Local

  let q1 := Quat.multiply q0 q1Local |> Quat.normalize
  let t0 := Vec3.zero
  let t1 := mat1.getTranslation
  let dq0 := DualQuat.fromRotationTranslation q0 t0
  let dq1 := DualQuat.fromRotationTranslation q1 t1

  -- Draw skeleton
  let joint := mat0.transformPoint (Vec3.mk 1 0 0)
  let endPt := mat1.transformPoint (Vec3.mk 1 0 0)
  drawPolyline3D view #[Vec3.zero, joint, endPt] (Color.gray 0.7) (2.0 * screenScale)

  -- Build cross-sections along the bone
  let sections := 9
  let ringPts := 10
  let radius := 0.18

  for s in [:sections] do
    let t := s.toFloat / (sections - 1).toFloat
    let x := t * 2.0
    let weight1 := Float.clamp t 0.0 1.0
    let weight0 := 1.0 - weight1

    let dqBlend := DualQuat.blend #[dq0, dq1] #[weight0, weight1]

    let mut lbsRing : Array Vec3 := #[]
    let mut dlbRing : Array Vec3 := #[]

    for i in [:ringPts + 1] do
      let angle := (i.toFloat / ringPts.toFloat) * (2.0 * Float.pi)
      let localPos := Vec3.mk x (Float.cos angle * radius) (Float.sin angle * radius)

      let p0 := mat0.transformPoint localPos
      let p1 := mat1.transformPoint localPos
      let lbs := p0.scale weight0 + p1.scale weight1
      let dlb := dqBlend.transformPoint localPos

      lbsRing := lbsRing.push lbs
      dlbRing := dlbRing.push dlb

    drawPolyline3D view lbsRing (Color.rgba 1.0 0.6 0.2 0.8) (1.5 * screenScale)
    drawPolyline3D view dlbRing (Color.rgba 0.2 0.9 0.5 0.9) (1.5 * screenScale)

  -- Title and info
  let infoY := h - 140 * screenScale
  setFillColor VecColor.label
  fillTextXY s!"Twist: {formatFloat (state.twist * 180.0 / Float.pi)} deg" (20 * screenScale) infoY fontSmall
  fillTextXY s!"Bend: {formatFloat (state.bend * 180.0 / Float.pi)} deg" (20 * screenScale) (infoY + 22 * screenScale) fontSmall

  fillTextXY "DUAL QUATERNION BLENDING" (20 * screenScale) (30 * screenScale) fontMedium
  setFillColor (Color.gray 0.6)
  fillTextXY "T/G: twist ± | B/V: bend ± | Drag: rotate view" (20 * screenScale) (55 * screenScale) fontSmall

  -- Legend
  setFillColor (Color.rgba 1.0 0.6 0.2 0.9)
  fillTextXY "LBS" (20 * screenScale) (infoY + 44 * screenScale) fontSmall
  setFillColor (Color.rgba 0.2 0.9 0.5 0.9)
  fillTextXY "DLB" (80 * screenScale) (infoY + 44 * screenScale) fontSmall

/-- Create the dual quaternion blending widget. -/
def dualQuaternionBlendingWidget (env : DemoEnv) (state : DualQuaternionBlendingState)
    : Afferent.Arbor.WidgetBuilder := do
  let config := dualQuaternionBlendingMathViewConfig state env.screenScale
  MathView3D.mathView3D config env.fontSmall (fun view => do
    renderDualQuaternionBlending state view env.screenScale env.fontMedium env.fontSmall
  )

end Demos.Linalg
