/-
  Rigid Body Simulator Demo - apply forces and torques to a rigid body.
-/
import Afferent
import Afferent.Widget
import Afferent.Arbor
import Demos.Core.Demo
import Demos.Linalg.Shared
import Trellis
import Linalg.Core
import Linalg.Vec2
import Linalg.Vec3
import Linalg.Mat3
import Linalg.Quat
import Linalg.Physics

open Afferent CanvasM Linalg
open Afferent.Widget

namespace Demos.Linalg

/-- Shape options for rigid body demo. -/
inductive RigidShape where
  | box
  | sphere
  | cylinder
  deriving BEq, Inhabited

/-- State for rigid body simulator. -/
structure RigidBodySimulatorState where
  shape : RigidShape := .box
  body : RigidBody
  halfExtents : Vec3 := Vec3.mk 1.1 0.6 0.4
  radius : Float := 0.9
  height : Float := 1.6
  mass : Float := 2.0
  pendingForce : Option (Vec3 × Vec3 × Float) := none
  lastForce : Vec3 := Vec3.zero
  lastPoint : Vec3 := Vec3.zero
  lastTorque : Vec3 := Vec3.zero
  animating : Bool := true
  showAxes : Bool := true
  time : Float := 0.0

instance : Inhabited RigidBodySimulatorState where
  default := {
    body := RigidBody.create Vec3.zero 2.0 (InertiaTensor.solidBox 2.0 (Vec3.mk 1.1 0.6 0.4))
  }

/-- Initial state. -/
def rigidBodySimulatorInitialState : RigidBodySimulatorState := {
  body := RigidBody.create Vec3.zero 2.0 (InertiaTensor.solidBox 2.0 (Vec3.mk 1.1 0.6 0.4))
}

def rigidBodyMathViewConfig (screenScale : Float) : MathView2D.Config := {
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

private def shapeName : RigidShape → String
  | .box => "Box"
  | .sphere => "Sphere"
  | .cylinder => "Cylinder"

private def inertiaFor (shape : RigidShape) (mass : Float)
    (halfExtents : Vec3) (radius height : Float) : Mat3 :=
  match shape with
  | .box => InertiaTensor.solidBox mass halfExtents
  | .sphere => InertiaTensor.solidSphere mass radius
  | .cylinder => InertiaTensor.solidCylinder mass radius height

private def withInertia (body : RigidBody) (inertia : Mat3) : RigidBody :=
  let inv := inertia.inverse.getD Mat3.zero
  { body with inertiaTensor := inertia, inverseInertiaTensor := inv }

private def drawLineWorld (a b : Vec2) (origin : Float × Float) (scale : Float)
    (color : Color) (lineWidth : Float := 2.0) : CanvasM Unit := do
  let (sx, sy) := worldToScreen a origin scale
  let (ex, ey) := worldToScreen b origin scale
  setStrokeColor color
  setLineWidth lineWidth
  let path := Afferent.Path.empty
    |>.moveTo (Point.mk sx sy)
    |>.lineTo (Point.mk ex ey)
  strokePath path

private def drawOrientedBox (center : Vec3) (orientation : Quat)
    (halfExtents : Vec3) (origin : Float × Float) (scale : Float) : CanvasM Unit := do
  let rot : Mat3 := orientation.toMat3
  let corners : Array Vec3 := #[
    Vec3.mk (-halfExtents.x) (-halfExtents.y) 0.0,
    Vec3.mk halfExtents.x (-halfExtents.y) 0.0,
    Vec3.mk halfExtents.x halfExtents.y 0.0,
    Vec3.mk (-halfExtents.x) halfExtents.y 0.0
  ]
  let mut worldCorners : Array Vec2 := #[]
  for c in corners do
    let p := rot.transformVec3 c |>.add center
    worldCorners := worldCorners.push (Vec2.mk p.x p.y)

  if worldCorners.size > 0 then
    let first := worldCorners[0]!
    let (sx, sy) := worldToScreen first origin scale
    let mut path := Afferent.Path.empty
      |>.moveTo (Point.mk sx sy)
    for i in [1:worldCorners.size] do
      let p := worldCorners[i]!
      let (px, py) := worldToScreen p origin scale
      path := path.lineTo (Point.mk px py)
    path := path.closePath
    setFillColor (Color.rgba 0.2 0.8 1.0 0.12)
    fillPath path
    setStrokeColor (Color.rgba 0.2 0.8 1.0 0.8)
    setLineWidth 2.4
    strokePath path

private def drawAxes (center : Vec3) (orientation : Quat)
    (origin : Float × Float) (scale : Float) : CanvasM Unit := do
  let rot : Mat3 := orientation.toMat3
  let xAxis := rot.transformVec3 (Vec3.mk 1.0 0.0 0.0)
  let yAxis := rot.transformVec3 (Vec3.mk 0.0 1.0 0.0)
  let start := Vec2.mk center.x center.y
  drawLineWorld start (start.add (Vec2.mk xAxis.x xAxis.y)) origin scale VecColor.xAxis 2.0
  drawLineWorld start (start.add (Vec2.mk yAxis.x yAxis.y)) origin scale VecColor.yAxis 2.0

def stepRigidBodySimulator (state : RigidBodySimulatorState) (dt : Float)
    : RigidBodySimulatorState :=
  Id.run do
    let dt' := Float.min dt 0.033
    let inertia := inertiaFor state.shape state.mass state.halfExtents state.radius state.height
    let mut body := withInertia state.body inertia
    let mut pending := state.pendingForce
    let mut lastTorque := state.lastTorque

    match pending with
    | some (force, point, t) =>
        if t > 0.0 then
          body := RigidBody.applyForceAtPoint body force point
          lastTorque := (point.sub body.position).cross force
          pending := some (force, point, t - dt')
        else
          pending := none
    | none => ()

    if state.animating then
      body := Integration.integrateRigidBody body dt'

    body := RigidBody.clearAccelerations body

    return { state with
      body := body
      pendingForce := pending
      lastTorque := lastTorque
      time := state.time + dt' }

/-- Render rigid body simulator. -/
def renderRigidBodySimulator (state : RigidBodySimulatorState)
    (view : MathView2D.View) (screenScale : Float) (fontMedium fontSmall : Font) : CanvasM Unit := do
  let w := view.width
  let h := view.height
  let origin : Float × Float := (view.origin.x, view.origin.y)
  let scale := view.scale

  match state.shape with
  | .box =>
      drawOrientedBox state.body.position state.body.orientation state.halfExtents origin scale
  | .sphere =>
      let pos2 := Vec2.mk state.body.position.x state.body.position.y
      drawMarker pos2 origin scale (Color.rgba 0.2 0.8 1.0 1.0) 8.0
      let (sx, sy) := worldToScreen pos2 origin scale
      setStrokeColor (Color.rgba 0.2 0.8 1.0 0.8)
      setLineWidth 2.4
      strokePath (Afferent.Path.circle (Point.mk sx sy) (state.radius * scale))
  | .cylinder =>
      let pos2 := Vec2.mk state.body.position.x state.body.position.y
      let (sx, sy) := worldToScreen pos2 origin scale
      setStrokeColor (Color.rgba 0.2 0.8 1.0 0.8)
      setLineWidth 2.4
      strokePath (Afferent.Path.ellipse (Point.mk sx sy) (state.radius * scale) (state.height * 0.5 * scale))

  if state.showAxes then
    drawAxes state.body.position state.body.orientation origin scale

  if state.lastForce.length > 0.001 then
    let start2 := Vec2.mk state.lastPoint.x state.lastPoint.y
    let end2 := start2.add ((Vec2.mk state.lastForce.x state.lastForce.y).scale 0.2)
    drawArrow2D (worldToScreen start2 origin scale) (worldToScreen end2 origin scale)
      { color := Color.rgba 1.0 0.7 0.2 1.0, lineWidth := 2.0 }

  fillTextXY "RIGID BODY SIMULATOR" (20 * screenScale) (30 * screenScale) fontMedium
  setFillColor (Color.gray 0.6)
  let animText := if state.animating then "animating" else "paused"
  fillTextXY s!"Space: {animText} | Click: apply force | T: torque | S: shape | A: axes" (20 * screenScale)
    (55 * screenScale) fontSmall

  let infoY := h - 120 * screenScale
  setFillColor VecColor.label
  fillTextXY s!"Shape: {shapeName state.shape}" (20 * screenScale) infoY fontSmall
  fillTextXY s!"Vel: {formatVec3 state.body.velocity}" (20 * screenScale) (infoY + 20 * screenScale) fontSmall
  fillTextXY s!"Ang Vel: {formatVec3 state.body.angularVelocity}" (20 * screenScale)
    (infoY + 40 * screenScale) fontSmall
  fillTextXY s!"Torque: {formatVec3 state.lastTorque}" (20 * screenScale) (infoY + 60 * screenScale) fontSmall

/-- Create rigid body simulator widget. -/
def rigidBodySimulatorWidget (env : DemoEnv) (state : RigidBodySimulatorState)
    : Afferent.Arbor.WidgetBuilder := do
  let config := rigidBodyMathViewConfig env.screenScale
  MathView2D.mathView2D config env.fontSmall (fun view => do
    renderRigidBodySimulator state view env.screenScale env.fontMedium env.fontSmall
  )

end Demos.Linalg
