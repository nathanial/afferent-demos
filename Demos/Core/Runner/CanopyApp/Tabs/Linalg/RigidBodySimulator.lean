/-
  Demo Runner - Canopy app linalg RigidBodySimulator tab content.
-/
import Reactive
import Afferent
import Afferent.Canopy
import Afferent.Canopy.Reactive
import Demos.Core.Demo
import Demos.Linalg.RigidBodySimulator
import Trellis

open Reactive Reactive.Host
open Afferent
open Afferent.Arbor
open Afferent.Canopy
open Afferent.Canopy.Reactive
open Trellis

namespace Demos

private def nextShape (shape : Demos.Linalg.RigidShape) : Demos.Linalg.RigidShape :=
  match shape with
  | .box => .sphere
  | .sphere => .cylinder
  | .cylinder => .box

private def inertiaFor (shape : Demos.Linalg.RigidShape) (mass : Float)
    (halfExtents : Linalg.Vec3) (radius height : Float) : Linalg.Mat3 :=
  match shape with
  | .box => Linalg.InertiaTensor.solidBox mass halfExtents
  | .sphere => Linalg.InertiaTensor.solidSphere mass radius
  | .cylinder => Linalg.InertiaTensor.solidCylinder mass radius height

private def buildBody (state : Demos.Linalg.RigidBodySimulatorState) : Linalg.RigidBody :=
  let inertia := inertiaFor state.shape state.mass state.halfExtents state.radius state.height
  Linalg.RigidBody.create Linalg.Vec3.zero state.mass inertia

private def applyClickForce (state : Demos.Linalg.RigidBodySimulatorState) (world : Linalg.Vec2)
    : Demos.Linalg.RigidBodySimulatorState :=
  let point := Linalg.Vec3.mk world.x world.y 0.0
  let dir := point.sub state.body.position
  let base := if dir.length < 0.001 then Linalg.Vec3.unitX else dir.normalize
  let force := base.scale 8.0
  { state with
    pendingForce := some (force, point, 0.18)
    lastForce := force
    lastPoint := point }

def rigidBodySimulatorTabContent (env : DemoEnv) : WidgetM Unit := do
  let animFrame ← useAnimationFrame
  let demoName ← registerComponentW "rigid-body-simulator"

  let keyEvents ← useKeyboard
  let keyUpdates ← Event.mapM (fun data =>
    fun (s : Demos.Linalg.RigidBodySimulatorState) =>
      if data.event.isPress then
        match data.event.key with
        | .char 'r' => Demos.Linalg.rigidBodySimulatorInitialState
        | .space => { s with animating := !s.animating }
        | .char 'a' => { s with showAxes := !s.showAxes }
        | .char 's' =>
            let newShape := nextShape s.shape
            let newState := { s with shape := newShape }
            { newState with
              body := buildBody newState
              pendingForce := none
              lastForce := Linalg.Vec3.zero
              lastPoint := Linalg.Vec3.zero
              lastTorque := Linalg.Vec3.zero }
        | .char 't' =>
            let torque := Linalg.Vec3.mk 0.0 0.0 6.0
            { s with body := Linalg.RigidBody.applyTorque s.body torque, lastTorque := torque }
        | _ => s
      else s
    ) keyEvents

  let clickEvents ← useClickData demoName
  let clickUpdates ← Event.mapM (fun data =>
    if data.click.button != 0 then
      id
    else
      match data.nameMap.get? demoName with
      | some wid =>
          match data.layouts.get wid with
          | some layout =>
              let rect := layout.contentRect
              let localX := data.click.x - rect.x
              let localY := data.click.y - rect.y
              let origin := (rect.width / 2, rect.height / 2)
              let scale := 70.0 * env.screenScale
              let world := Demos.Linalg.screenToWorld (localX, localY) origin scale
              fun (state : Demos.Linalg.RigidBodySimulatorState) =>
                applyClickForce state world
          | none => id
      | none => id
    ) clickEvents

  let animUpdates ← Event.mapM (fun dt =>
    fun (s : Demos.Linalg.RigidBodySimulatorState) =>
      Demos.Linalg.stepRigidBodySimulator s dt
    ) animFrame

  let allUpdates ← Event.mergeAllListM [keyUpdates, clickUpdates, animUpdates]
  let state ← foldDyn (fun f s => f s) Demos.Linalg.rigidBodySimulatorInitialState allUpdates

  let _ ← dynWidget state fun s => do
    let containerStyle : BoxStyle := {
      flexItem := some (FlexItem.growing 1)
      width := .percent 1.0
      height := .percent 1.0
    }
    emit (pure (namedColumn demoName 0 containerStyle #[
      Demos.Linalg.rigidBodySimulatorWidget env s
    ]))
  pure ()

end Demos
