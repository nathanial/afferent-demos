/-
  Demo Runner - Canopy app linalg CrossProduct3D tab content.
-/
import Reactive
import Afferent
import Afferent.Canopy
import Afferent.Canopy.Reactive
import Demos.Core.Demo
import Demos.Linalg.CrossProduct3D
import Trellis

open Reactive Reactive.Host
open Afferent
open Afferent.Arbor
open Afferent.Canopy
open Afferent.Canopy.Reactive
open Trellis

namespace Demos
def crossProduct3DTabContent (env : DemoEnv) : WidgetM Unit := do
  let elapsedTime ← useElapsedTime
  let crossName ← registerComponentW "cross-product-3d"

  let clickEvents ← useClickData crossName
  let clickUpdates ← Event.mapM (fun data =>
    if data.click.button != 0 then
      id
    else
      fun (s : Demos.Linalg.CrossProduct3DState) =>
        { s with dragging := .camera, lastMouseX := data.click.x, lastMouseY := data.click.y }
    ) clickEvents

  let hoverEvents ← useAllHovers
  let hoverUpdates ← Event.mapM (fun data =>
    fun (state : Demos.Linalg.CrossProduct3DState) =>
      match state.dragging with
      | .camera =>
          let dx := data.x - state.lastMouseX
          let dy := data.y - state.lastMouseY
          let newYaw := state.cameraYaw + dx * 0.01
          let rawPitch := state.cameraPitch + dy * 0.01
          let newPitch := if rawPitch < -1.5 then -1.5 else if rawPitch > 1.5 then 1.5 else rawPitch
          { state with
            cameraYaw := newYaw
            cameraPitch := newPitch
            lastMouseX := data.x
            lastMouseY := data.y
          }
      | _ => state
    ) hoverEvents

  let mouseUpEvents ← useAllMouseUp
  let mouseUpUpdates ← Event.mapM (fun data =>
    fun (s : Demos.Linalg.CrossProduct3DState) =>
      if data.button == 0 then
        { s with dragging := .none }
      else s
    ) mouseUpEvents

  let keyEvents ← useKeyboard
  let keyUpdates ← Event.mapM (fun data =>
    fun (s : Demos.Linalg.CrossProduct3DState) =>
      if data.event.isPress then
        match data.event.key with
        | .char 'p' => { s with showParallelogram := !s.showParallelogram }
        | .char 'r' => { s with cameraYaw := 0.6, cameraPitch := 0.4 }
        | _ => s
      else s
    ) keyEvents

  let allUpdates ← Event.mergeAllListM [clickUpdates, hoverUpdates, mouseUpUpdates, keyUpdates]
  let state ← foldDyn (fun f s => f s) Demos.Linalg.crossProduct3DInitialState allUpdates

  let _ ← dynWidget state fun s => do
    let containerStyle : BoxStyle := {
      flexItem := some (FlexItem.growing 1)
      width := .percent 1.0
      height := .percent 1.0
    }
    emit (pure (namedColumn crossName 0 containerStyle #[
      Demos.Linalg.crossProduct3DWidget env s
    ]))
  pure ()

end Demos
