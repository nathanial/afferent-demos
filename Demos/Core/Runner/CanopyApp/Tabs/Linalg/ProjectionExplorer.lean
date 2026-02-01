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
  let projName ← registerComponentW "projection-explorer"

  let keyEvents ← useKeyboard
  let keyUpdates ← Event.mapM (fun data =>
    fun (s : Demos.Linalg.ProjectionExplorerState) =>
      if data.event.isPress then
        match data.event.key with
        | .tab =>
            { s with projType := match s.projType with
              | .perspective => .orthographic
              | .orthographic => .perspective
            }
        | .char 'f' =>
            let newFar := s.far + 0.5
            { s with far := if newFar > 10.0 then 10.0 else newFar }
        | .char 'n' =>
            let newNear := s.near + 0.1
            let maxNear := s.far - 0.5
            let clampedNear := if newNear > maxNear then maxNear else newNear
            { s with near := if clampedNear < 0.1 then 0.1 else clampedNear }
        | .char '=' | .char '+' =>
            match s.projType with
            | .perspective =>
                let newFov := s.fov + 0.1
                let maxFov := 2.513
                { s with fov := if newFov > maxFov then maxFov else newFov }
            | .orthographic =>
                let newSize := s.orthoSize + 0.2
                { s with orthoSize := if newSize > 5.0 then 5.0 else newSize }
        | .char '-' =>
            match s.projType with
            | .perspective =>
                let newFov := s.fov - 0.1
                { s with fov := if newFov < 0.3 then 0.3 else newFov }
            | .orthographic =>
                let newSize := s.orthoSize - 0.2
                { s with orthoSize := if newSize < 0.5 then 0.5 else newSize }
        | .char 'c' => { s with showClipSpace := !s.showClipSpace }
        | .char 'o' => { s with showTestObjects := !s.showTestObjects }
        | _ => s
      else s
    ) keyEvents

  let clickEvents ← useClickData projName
  let clickUpdates ← Event.mapM (fun data =>
    if data.click.button != 0 then
      id
    else
      fun (s : Demos.Linalg.ProjectionExplorerState) =>
        { s with dragging := true, lastMouseX := data.click.x, lastMouseY := data.click.y }
    ) clickEvents

  let hoverEvents ← useAllHovers
  let hoverUpdates ← Event.mapM (fun data =>
    fun (state : Demos.Linalg.ProjectionExplorerState) =>
      if state.dragging then
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
      else
        state
    ) hoverEvents

  let mouseUpEvents ← useAllMouseUp
  let mouseUpUpdates ← Event.mapM (fun data =>
    fun (s : Demos.Linalg.ProjectionExplorerState) =>
      if data.button == 0 then { s with dragging := false } else s
    ) mouseUpEvents

  let allUpdates ← Event.mergeAllListM [keyUpdates, clickUpdates, hoverUpdates, mouseUpUpdates]
  let state ← foldDyn (fun f s => f s) Demos.Linalg.projectionExplorerInitialState allUpdates

  let _ ← dynWidget state fun s => do
    let containerStyle : BoxStyle := {
      flexItem := some (FlexItem.growing 1)
      width := .percent 1.0
      height := .percent 1.0
    }
    emit (pure (namedColumn projName 0 containerStyle #[
      Demos.Linalg.projectionExplorerWidget env s
    ]))
  pure ()

end Demos
