/-
  Demo Runner - Canopy app linalg ProjectionExplorer tab content.
-/
import Reactive
import Afferent
import Afferent.Canopy
import Afferent.Canopy.Reactive
import Demos.Core.Demo
import Demos.Linalg.ProjectionExplorer
import Trellis

open Reactive Reactive.Host
open Afferent
open Afferent.Arbor
open Afferent.Canopy
open Afferent.Canopy.Reactive
open Trellis

namespace Demos
def projectionExplorerTabContent (env : DemoEnv) : WidgetM Unit := do
  let elapsedTime ← useElapsedTime
  let stateRef ← SpiderM.liftIO (IO.mkRef Demos.Linalg.projectionExplorerInitialState)
  let projName ← registerComponentW "projection-explorer"

  let keyEvents ← useKeyboard
  let keyAction ← Event.mapM (fun data => do
    if data.event.isPress then
      match data.event.key with
      | .tab =>
          stateRef.modify fun s =>
            { s with projType := match s.projType with
              | .perspective => .orthographic
              | .orthographic => .perspective
            }
      | .char 'f' =>
          stateRef.modify fun s =>
            let newFar := s.far + 0.5
            { s with far := if newFar > 10.0 then 10.0 else newFar }
      | .char 'n' =>
          stateRef.modify fun s =>
            let newNear := s.near + 0.1
            let maxNear := s.far - 0.5
            let clampedNear := if newNear > maxNear then maxNear else newNear
            { s with near := if clampedNear < 0.1 then 0.1 else clampedNear }
      | .char '=' | .char '+' =>
          stateRef.modify fun s =>
            match s.projType with
            | .perspective =>
                let newFov := s.fov + 0.1
                let maxFov := 2.513
                { s with fov := if newFov > maxFov then maxFov else newFov }
            | .orthographic =>
                let newSize := s.orthoSize + 0.2
                { s with orthoSize := if newSize > 5.0 then 5.0 else newSize }
      | .char '-' =>
          stateRef.modify fun s =>
            match s.projType with
            | .perspective =>
                let newFov := s.fov - 0.1
                { s with fov := if newFov < 0.3 then 0.3 else newFov }
            | .orthographic =>
                let newSize := s.orthoSize - 0.2
                { s with orthoSize := if newSize < 0.5 then 0.5 else newSize }
      | .char 'c' =>
          stateRef.modify fun s => { s with showClipSpace := !s.showClipSpace }
      | .char 'o' =>
          stateRef.modify fun s => { s with showTestObjects := !s.showTestObjects }
      | _ => pure ()
    ) keyEvents
  performEvent_ keyAction

  let clickEvents ← useClickData projName
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
    if state.dragging then
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
    else
      pure ()
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
    emit (pure (namedColumn projName 0 containerStyle #[
      Demos.Linalg.projectionExplorerWidget env state
    ]))
  pure ()

end Demos
