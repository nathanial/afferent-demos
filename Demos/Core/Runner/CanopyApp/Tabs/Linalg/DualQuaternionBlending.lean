/-
  Demo Runner - Canopy app linalg DualQuaternionBlending tab content.
-/
import Reactive
import Afferent
import Afferent.Canopy
import Afferent.Canopy.Reactive
import Demos.Core.Demo
import Demos.Linalg.DualQuaternionBlending
import Trellis

open Reactive Reactive.Host
open Afferent
open Afferent.Arbor
open Afferent.Canopy
open Afferent.Canopy.Reactive
open Trellis

namespace Demos
def dualQuaternionBlendingTabContent (env : DemoEnv) : WidgetM Unit := do
  let elapsedTime ← useElapsedTime
  let dualName ← registerComponentW "dual-quaternion-blending"

  let keyEvents ← useKeyboard
  let keyUpdates ← Event.mapM (fun data =>
    fun (s : Demos.Linalg.DualQuaternionBlendingState) =>
      if data.event.isPress then
        match data.event.key with
        | .char 'r' => Demos.Linalg.dualQuaternionBlendingInitialState
        | .char 't' => { s with twist := s.twist + 0.1 }
        | .char 'g' => { s with twist := s.twist - 0.1 }
        | .char 'b' => { s with bend := s.bend + 0.1 }
        | .char 'v' => { s with bend := s.bend - 0.1 }
        | _ => s
      else s
    ) keyEvents

  let clickEvents ← useClickData dualName
  let clickUpdates ← Event.mapM (fun data =>
    if data.click.button != 0 then
      id
    else
      fun (s : Demos.Linalg.DualQuaternionBlendingState) =>
        { s with dragging := true, lastMouseX := data.click.x, lastMouseY := data.click.y }
    ) clickEvents

  let hoverEvents ← useAllHovers
  let hoverUpdates ← Event.mapM (fun data =>
    fun (state : Demos.Linalg.DualQuaternionBlendingState) =>
      if state.dragging then
        let dx := data.x - state.lastMouseX
        let dy := data.y - state.lastMouseY
        let newYaw := state.cameraYaw + dx * 0.005
        let newPitch := state.cameraPitch + dy * 0.005
        { state with
          cameraYaw := newYaw
          cameraPitch := newPitch
          lastMouseX := data.x
          lastMouseY := data.y
        }
      else
        state
    ) hoverEvents

  let mouseUpEvents ← useAllMouseUp
  let mouseUpUpdates ← Event.mapM (fun data =>
    fun (s : Demos.Linalg.DualQuaternionBlendingState) =>
      if data.button == 0 then
        { s with dragging := false }
      else s
    ) mouseUpEvents

  let allUpdates ← Event.mergeAllListM [keyUpdates, clickUpdates, hoverUpdates, mouseUpUpdates]
  let state ← foldDyn (fun f s => f s) Demos.Linalg.dualQuaternionBlendingInitialState allUpdates

  let _ ← dynWidget state fun s => do
    let containerStyle : BoxStyle := {
      flexItem := some (FlexItem.growing 1)
      width := .percent 1.0
      height := .percent 1.0
    }
    emit (pure (namedColumn dualName 0 containerStyle #[
      Demos.Linalg.dualQuaternionBlendingWidget env s
    ]))
  pure ()

end Demos
