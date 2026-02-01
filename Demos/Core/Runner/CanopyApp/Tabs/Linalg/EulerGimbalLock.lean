/-
  Demo Runner - Canopy app linalg EulerGimbalLock tab content.
-/
import Reactive
import Afferent
import Afferent.Canopy
import Afferent.Canopy.Reactive
import Demos.Core.Demo
import Demos.Linalg.EulerGimbalLock
import Trellis

open Reactive Reactive.Host
open Afferent
open Afferent.Arbor
open Afferent.Canopy
open Afferent.Canopy.Reactive
open Trellis

namespace Demos
def eulerGimbalLockTabContent (env : DemoEnv) : WidgetM Unit := do
  let elapsedTime ← useElapsedTime
  let gimbalName ← registerComponentW "euler-gimbal-lock"

  let keyEvents ← useKeyboard
  let keyUpdates ← Event.mapM (fun data =>
    fun (s : Demos.Linalg.EulerGimbalLockState) =>
      if data.event.isPress then
        match data.event.key with
        | .char 'r' => Demos.Linalg.eulerGimbalLockInitialState
        | .char '1' => { s with selectedAxis := 0 }
        | .char '2' => { s with selectedAxis := 1 }
        | .char '3' => { s with selectedAxis := 2 }
        | .char 'o' =>
            let nextOrder := match s.euler.order with
              | .XYZ => .XZY
              | .XZY => .YXZ
              | .YXZ => .YZX
              | .YZX => .ZXY
              | .ZXY => .ZYX
              | .ZYX => .XYZ
            { s with euler := { s.euler with order := nextOrder } }
        | .left | .right =>
            let delta := if data.event.key == .left then -5.0 else 5.0
            let e := s.euler
            let radDelta := delta * Linalg.Float.pi / 180.0
            let e' := match s.selectedAxis with
              | 0 => { e with a1 := e.a1 + radDelta }
              | 1 => { e with a2 := e.a2 + radDelta }
              | _ => { e with a3 := e.a3 + radDelta }
            { s with euler := e' }
        | _ => s
      else s
    ) keyEvents

  let clickEvents ← useClickData gimbalName
  let clickUpdates ← Event.mapM (fun data =>
    if data.click.button != 0 then
      id
    else
      fun (s : Demos.Linalg.EulerGimbalLockState) =>
        { s with dragging := true, lastMouseX := data.click.x, lastMouseY := data.click.y }
    ) clickEvents

  let hoverEvents ← useAllHovers
  let hoverUpdates ← Event.mapM (fun data =>
    fun (state : Demos.Linalg.EulerGimbalLockState) =>
      if !state.dragging then
        state
      else
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
    ) hoverEvents

  let mouseUpEvents ← useAllMouseUp
  let mouseUpUpdates ← Event.mapM (fun data =>
    fun (s : Demos.Linalg.EulerGimbalLockState) =>
      if data.button == 0 then
        { s with dragging := false }
      else s
    ) mouseUpEvents

  let allUpdates ← Event.mergeAllListM [keyUpdates, clickUpdates, hoverUpdates, mouseUpUpdates]
  let state ← foldDyn (fun f s => f s) Demos.Linalg.eulerGimbalLockInitialState allUpdates

  let _ ← dynWidget state fun s => do
    let containerStyle : BoxStyle := {
      flexItem := some (FlexItem.growing 1)
      width := .percent 1.0
      height := .percent 1.0
    }
    emit (pure (namedColumn gimbalName 0 containerStyle #[
      Demos.Linalg.eulerGimbalLockWidget env s
    ]))
  pure ()

end Demos
