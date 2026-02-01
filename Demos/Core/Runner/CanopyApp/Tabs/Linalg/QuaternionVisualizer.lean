/-
  Demo Runner - Canopy app linalg QuaternionVisualizer tab content.
-/
import Reactive
import Afferent
import Afferent.Canopy
import Afferent.Canopy.Reactive
import Demos.Core.Demo
import Demos.Linalg.QuaternionVisualizer
import Trellis

open Reactive Reactive.Host
open Afferent
open Afferent.Arbor
open Afferent.Canopy
open Afferent.Canopy.Reactive
open Trellis

namespace Demos
def quaternionVisualizerTabContent (env : DemoEnv) (elapsedTime : Dynamic Spider Float)
    (stateRef : IO.Ref Demos.Linalg.QuaternionVisualizerState) : WidgetM Unit := do
  let quatName ← registerComponentW "quaternion-visualizer"

  let clickEvents ← useClickData quatName
  let clickAction ← Event.mapM (fun data => do
    if data.click.button != 0 && data.click.button != 1 then
      pure ()
    else
      match data.nameMap.get? quatName with
      | some wid =>
          match data.layouts.get wid with
          | some layout =>
              let rect := layout.contentRect
              let localX := data.click.x - rect.x
              let localY := data.click.y - rect.y
              let labels : Array (Demos.Linalg.QuatComponent × Nat) := #[(.x, 0), (.y, 1), (.z, 2), (.w, 3)]
              let mut hitSlider : Option Demos.Linalg.QuatComponent := none
              for (comp, idx) in labels do
                let layout := Demos.Linalg.sliderLayoutFor rect.width rect.height env.screenScale idx
                let hit := localX >= layout.x && localX <= layout.x + layout.width
                  && localY >= layout.y - 8.0 && localY <= layout.y + layout.height + 8.0
                if hit then
                  hitSlider := some comp
              let state ← stateRef.get
              match hitSlider with
              | some comp =>
                  let idx := match comp with | .x => 0 | .y => 1 | .z => 2 | .w => 3
                  let layout := Demos.Linalg.sliderLayoutFor rect.width rect.height env.screenScale idx
                  let t := (localX - layout.x) / layout.width
                  let value := Demos.Linalg.clampUnit (t * 2.0 - 1.0)
                  let q := (Demos.Linalg.setQuatComponent state.quat comp value).normalize
                  stateRef.set { state with
                    quat := q
                    dragging := .slider comp
                    lastMouseX := data.click.x
                    lastMouseY := data.click.y
                  }
              | none =>
                  if data.click.button == 1 then
                    stateRef.set { state with dragging := .camera, lastMouseX := data.click.x, lastMouseY := data.click.y }
                  else
                    stateRef.set { state with dragging := .arcball, lastMouseX := data.click.x, lastMouseY := data.click.y }
          | none => pure ()
      | none => pure ()
    ) clickEvents
  performEvent_ clickAction

  let hoverEvents ← useAllHovers
  let hoverAction ← Event.mapM (fun data => do
    let state ← stateRef.get
    match state.dragging with
    | .none => pure ()
    | .camera =>
        let dx := data.x - state.lastMouseX
        let dy := data.y - state.lastMouseY
        let yaw := state.cameraYaw + dx * 0.005
        let pitch := state.cameraPitch + dy * 0.005
        stateRef.set { state with cameraYaw := yaw, cameraPitch := pitch, lastMouseX := data.x, lastMouseY := data.y }
    | .arcball =>
        let dx := data.x - state.lastMouseX
        let dy := data.y - state.lastMouseY
        let rotY := Linalg.Quat.fromAxisAngle Linalg.Vec3.unitY (dx * 0.008)
        let rotX := Linalg.Quat.fromAxisAngle Linalg.Vec3.unitX (dy * 0.008)
        let q := Linalg.Quat.multiply rotY (Linalg.Quat.multiply rotX state.quat) |>.normalize
        stateRef.set { state with quat := q, lastMouseX := data.x, lastMouseY := data.y }
    | .slider comp =>
        match data.nameMap.get? quatName with
        | some wid =>
            match data.layouts.get wid with
            | some layout =>
                let rect := layout.contentRect
                let localX := data.x - rect.x
                let idx := match comp with | .x => 0 | .y => 1 | .z => 2 | .w => 3
                let layout := Demos.Linalg.sliderLayoutFor rect.width rect.height env.screenScale idx
                let t := (localX - layout.x) / layout.width
                let value := Demos.Linalg.clampUnit (t * 2.0 - 1.0)
                let q := (Demos.Linalg.setQuatComponent state.quat comp value).normalize
                stateRef.set { state with quat := q, lastMouseX := data.x, lastMouseY := data.y }
            | none => pure ()
        | none => pure ()
    ) hoverEvents
  performEvent_ hoverAction

  let mouseUpEvents ← useAllMouseUp
  let mouseUpAction ← Event.mapM (fun _ => do
    stateRef.modify fun s => { s with dragging := .none }
    ) mouseUpEvents
  performEvent_ mouseUpAction

  let keyEvents ← useKeyboard
  let keyAction ← Event.mapM (fun data => do
    if data.event.isPress then
      match data.event.key with
      | .char 'r' =>
          stateRef.set Demos.Linalg.quaternionVisualizerInitialState
      | .char 'e' =>
          stateRef.modify fun s =>
            let e := s.eulerAngles
            let q := Linalg.Quat.fromEuler e.x e.y e.z
            { s with quat := q.normalize }
      | .char '1' =>
          stateRef.modify fun s => { s with selectedEuler := 0 }
      | .char '2' =>
          stateRef.modify fun s => { s with selectedEuler := 1 }
      | .char '3' =>
          stateRef.modify fun s => { s with selectedEuler := 2 }
      | .left =>
          stateRef.modify fun s =>
            let delta := -5.0 * Linalg.Float.pi / 180.0
            let e := s.eulerAngles
            let e' := match s.selectedEuler with
              | 0 => Linalg.Vec3.mk (e.x + delta) e.y e.z
              | 1 => Linalg.Vec3.mk e.x (e.y + delta) e.z
              | _ => Linalg.Vec3.mk e.x e.y (e.z + delta)
            { s with eulerAngles := e' }
      | .right =>
          stateRef.modify fun s =>
            let delta := 5.0 * Linalg.Float.pi / 180.0
            let e := s.eulerAngles
            let e' := match s.selectedEuler with
              | 0 => Linalg.Vec3.mk (e.x + delta) e.y e.z
              | 1 => Linalg.Vec3.mk e.x (e.y + delta) e.z
              | _ => Linalg.Vec3.mk e.x e.y (e.z + delta)
            { s with eulerAngles := e' }
      | _ => pure ()
    ) keyEvents
  performEvent_ keyAction

  let _ ← dynWidget elapsedTime fun _ => do
    let state ← SpiderM.liftIO stateRef.get
    let containerStyle : BoxStyle := {
      flexItem := some (FlexItem.growing 1)
      width := .percent 1.0
      height := .percent 1.0
    }
    emit (pure (namedColumn quatName 0 containerStyle #[
      Demos.Linalg.quaternionVisualizerWidget env state
    ]))
  pure ()

end Demos
