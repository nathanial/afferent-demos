/-
  Demo Runner - Canopy app linalg RayCastingPlayground tab content.
-/
import Reactive
import Afferent
import Afferent.Canopy
import Afferent.Canopy.Reactive
import Demos.Core.Demo
import Demos.Linalg.RayCastingPlayground
import Trellis

open Reactive Reactive.Host
open Afferent
open Afferent.Arbor
open Afferent.Canopy
open Afferent.Canopy.Reactive
open Trellis

namespace Demos
def rayCastingPlaygroundTabContent (env : DemoEnv) : WidgetM Unit := do
  let elapsedTime ← useElapsedTime
  let stateRef ← SpiderM.liftIO (IO.mkRef Demos.Linalg.rayCastingPlaygroundInitialState)
  let rayName ← registerComponentW "ray-casting-playground"

  let keyEvents ← useKeyboard
  let keyAction ← Event.mapM (fun data => do
    if data.event.key == .char 'r' && data.event.isPress then
      stateRef.set Demos.Linalg.rayCastingPlaygroundInitialState
    ) keyEvents
  performEvent_ keyAction

  let clickEvents ← useClickData rayName
  let clickAction ← Event.mapM (fun data => do
    if data.click.button != 0 && data.click.button != 1 then
      pure ()
    else
      match data.nameMap.get? rayName with
      | some wid =>
          match data.layouts.get wid with
          | some layout =>
              let rect := layout.contentRect
              let localX := data.click.x - rect.x
              let localY := data.click.y - rect.y
              let origin := (rect.width / 2, rect.height / 2)
              let scale := 70.0 * env.screenScale
              let worldPos := Demos.Linalg.screenToWorld (localX, localY) origin scale
              let state ← stateRef.get
              let origin2 := Linalg.Vec2.mk state.rayOrigin.x state.rayOrigin.z
              let target2 := Linalg.Vec2.mk state.rayTarget.x state.rayTarget.z
              if data.click.button == 1 then
                stateRef.set { state with dragging := .camera, lastMouseX := data.click.x, lastMouseY := data.click.y }
              else if Demos.Linalg.nearPoint worldPos origin2 0.5 then
                stateRef.set { state with dragging := .origin, lastMouseX := data.click.x, lastMouseY := data.click.y }
              else if Demos.Linalg.nearPoint worldPos target2 0.5 then
                stateRef.set { state with dragging := .direction, lastMouseX := data.click.x, lastMouseY := data.click.y }
              else
                pure ()
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
        let newYaw := state.cameraYaw + dx * 0.005
        let newPitch := state.cameraPitch + dy * 0.005
        stateRef.set { state with
          cameraYaw := newYaw
          cameraPitch := newPitch
          lastMouseX := data.x
          lastMouseY := data.y
        }
    | .origin =>
        match data.nameMap.get? rayName with
        | some wid =>
            match data.layouts.get wid with
            | some layout =>
                let rect := layout.contentRect
                let localX := data.x - rect.x
                let localY := data.y - rect.y
                let origin := (rect.width / 2, rect.height / 2)
                let scale := 70.0 * env.screenScale
                let worldPos := Demos.Linalg.screenToWorld (localX, localY) origin scale
                let newOrigin := Linalg.Vec3.mk worldPos.x 0.0 worldPos.y
                stateRef.set { state with rayOrigin := newOrigin, lastMouseX := data.x, lastMouseY := data.y }
            | none => pure ()
        | none => pure ()
    | .direction =>
        match data.nameMap.get? rayName with
        | some wid =>
            match data.layouts.get wid with
            | some layout =>
                let rect := layout.contentRect
                let localX := data.x - rect.x
                let localY := data.y - rect.y
                let origin := (rect.width / 2, rect.height / 2)
                let scale := 70.0 * env.screenScale
                let worldPos := Demos.Linalg.screenToWorld (localX, localY) origin scale
                let newTarget := Linalg.Vec3.mk worldPos.x 0.0 worldPos.y
                stateRef.set { state with rayTarget := newTarget, lastMouseX := data.x, lastMouseY := data.y }
            | none => pure ()
        | none => pure ()
    ) hoverEvents
  performEvent_ hoverAction

  let mouseUpEvents ← useAllMouseUp
  let mouseUpAction ← Event.mapM (fun _ => do
    stateRef.modify fun s => { s with dragging := .none }
    ) mouseUpEvents
  performEvent_ mouseUpAction

  let _ ← dynWidget elapsedTime fun _ => do
    let state ← SpiderM.liftIO stateRef.get
    let containerStyle : BoxStyle := {
      flexItem := some (FlexItem.growing 1)
      width := .percent 1.0
      height := .percent 1.0
    }
    emit (pure (namedColumn rayName 0 containerStyle #[
      Demos.Linalg.rayCastingPlaygroundWidget env state
    ]))
  pure ()

end Demos
