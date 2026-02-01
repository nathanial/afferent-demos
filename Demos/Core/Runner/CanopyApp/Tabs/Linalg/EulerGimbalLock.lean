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
def eulerGimbalLockTabContent (env : DemoEnv) (elapsedTime : Dynamic Spider Float)
    (stateRef : IO.Ref Demos.Linalg.EulerGimbalLockState) : WidgetM Unit := do
  let gimbalName ← registerComponentW "euler-gimbal-lock"

  let keyEvents ← useKeyboard
  let keyAction ← Event.mapM (fun data => do
    if data.event.isPress then
      match data.event.key with
      | .char 'r' =>
          stateRef.set Demos.Linalg.eulerGimbalLockInitialState
      | .char '1' =>
          stateRef.modify fun s => { s with selectedAxis := 0 }
      | .char '2' =>
          stateRef.modify fun s => { s with selectedAxis := 1 }
      | .char '3' =>
          stateRef.modify fun s => { s with selectedAxis := 2 }
      | .char 'o' =>
          stateRef.modify fun s =>
            let nextOrder := match s.euler.order with
              | .XYZ => .XZY
              | .XZY => .YXZ
              | .YXZ => .YZX
              | .YZX => .ZXY
              | .ZXY => .ZYX
              | .ZYX => .XYZ
            { s with euler := { s.euler with order := nextOrder } }
      | .left | .right =>
          stateRef.modify fun s =>
            let delta := if data.event.key == .left then -5.0 else 5.0
            let e := s.euler
            let radDelta := delta * Linalg.Float.pi / 180.0
            let e' := match s.selectedAxis with
              | 0 => { e with a1 := e.a1 + radDelta }
              | 1 => { e with a2 := e.a2 + radDelta }
              | _ => { e with a3 := e.a3 + radDelta }
            { s with euler := e' }
      | _ => pure ()
    ) keyEvents
  performEvent_ keyAction

  let clickEvents ← useClickData gimbalName
  let clickAction ← Event.mapM (fun data => do
    if data.click.button != 0 then
      pure ()
    else
      stateRef.modify fun s => { s with dragging := true, lastMouseX := data.click.x, lastMouseY := data.click.y }
    ) clickEvents
  performEvent_ clickAction

  let hoverEvents ← useAllHovers
  let hoverAction ← Event.mapM (fun data => do
    let state ← stateRef.get
    if !state.dragging then
      pure ()
    else
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
    ) hoverEvents
  performEvent_ hoverAction

  let mouseUpEvents ← useAllMouseUp
  let mouseUpAction ← Event.mapM (fun data => do
    if data.button == 0 then
      stateRef.modify fun s => { s with dragging := false }
    ) mouseUpEvents
  performEvent_ mouseUpAction

  let _ ← dynWidget elapsedTime fun _ => do
    let state ← SpiderM.liftIO stateRef.get
    let containerStyle : BoxStyle := {
      flexItem := some (FlexItem.growing 1)
      width := .percent 1.0
      height := .percent 1.0
    }
    emit (pure (namedColumn gimbalName 0 containerStyle #[
      Demos.Linalg.eulerGimbalLockWidget env state
    ]))
  pure ()

end Demos
