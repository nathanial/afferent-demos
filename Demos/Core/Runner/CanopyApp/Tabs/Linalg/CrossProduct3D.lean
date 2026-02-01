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
  let stateRef ← SpiderM.liftIO (IO.mkRef Demos.Linalg.crossProduct3DInitialState)
  let crossName ← registerComponentW "cross-product-3d"

  let clickEvents ← useClickData crossName
  let clickAction ← Event.mapM (fun data => do
    if data.click.button != 0 then
      pure ()
    else
      stateRef.modify fun s =>
        { s with dragging := .camera, lastMouseX := data.click.x, lastMouseY := data.click.y }
    ) clickEvents
  performEvent_ clickAction

  let hoverEvents ← useAllHovers
  let hoverAction ← Event.mapM (fun data => do
    let state ← stateRef.get
    match state.dragging with
    | .camera =>
        let dx := data.x - state.lastMouseX
        let dy := data.y - state.lastMouseY
        let newYaw := state.cameraYaw + dx * 0.01
        let rawPitch := state.cameraPitch + dy * 0.01
        let newPitch := if rawPitch < -1.5 then -1.5 else if rawPitch > 1.5 then 1.5 else rawPitch
        stateRef.set { state with
          cameraYaw := newYaw
          cameraPitch := newPitch
          lastMouseX := data.x
          lastMouseY := data.y
        }
    | _ => pure ()
    ) hoverEvents
  performEvent_ hoverAction

  let mouseUpEvents ← useAllMouseUp
  let mouseUpAction ← Event.mapM (fun data => do
    if data.button == 0 then
      stateRef.modify fun s => { s with dragging := .none }
    ) mouseUpEvents
  performEvent_ mouseUpAction

  let keyEvents ← useKeyboard
  let keyAction ← Event.mapM (fun data => do
    if data.event.isPress then
      match data.event.key with
      | .char 'p' =>
          stateRef.modify fun s => { s with showParallelogram := !s.showParallelogram }
      | .char 'r' =>
          stateRef.modify fun s => { s with cameraYaw := 0.6, cameraPitch := 0.4 }
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
    emit (pure (namedColumn crossName 0 containerStyle #[
      Demos.Linalg.crossProduct3DWidget env state
    ]))
  pure ()

end Demos
