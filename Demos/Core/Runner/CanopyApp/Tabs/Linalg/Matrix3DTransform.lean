/-
  Demo Runner - Canopy app linalg Matrix3DTransform tab content.
-/
import Reactive
import Afferent
import Afferent.Canopy
import Afferent.Canopy.Reactive
import Demos.Core.Demo
import Demos.Linalg.Matrix3DTransform
import Trellis

open Reactive Reactive.Host
open Afferent
open Afferent.Arbor
open Afferent.Canopy
open Afferent.Canopy.Reactive
open Trellis

namespace Demos
def matrix3DTransformTabContent (env : DemoEnv) : WidgetM Unit := do
  let elapsedTime ← useElapsedTime
  let mat3dName ← registerComponentW "matrix-3d-transform"

  let keyEvents ← useKeyboard
  let keyUpdates ← Event.mapM (fun data =>
    fun (s : Demos.Linalg.Matrix3DTransformState) =>
      if data.event.isPress then
        match data.event.key with
        | .char '1' => { s with selectedIndex := some 0 }
        | .char '2' => { s with selectedIndex := some 1 }
        | .char '3' => { s with selectedIndex := some 2 }
        | .up =>
            match s.selectedIndex with
            | some idx =>
                if idx > 0 then
                  let arr := s.transforms
                  let temp := arr.getD idx (.rotateX 0)
                  let arr := arr.set! idx (arr.getD (idx - 1) (.rotateX 0))
                  let arr := arr.set! (idx - 1) temp
                  { s with transforms := arr, selectedIndex := some (idx - 1) }
                else s
            | none => s
        | .down =>
            match s.selectedIndex with
            | some idx =>
                if idx + 1 < s.transforms.size then
                  let arr := s.transforms
                  let temp := arr.getD idx (.rotateX 0)
                  let arr := arr.set! idx (arr.getD (idx + 1) (.rotateX 0))
                  let arr := arr.set! (idx + 1) temp
                  { s with transforms := arr, selectedIndex := some (idx + 1) }
                else s
            | none => s
        | .char 'a' => { s with showAxes := !s.showAxes }
        | .char 'i' => { s with showIntermediateSteps := !s.showIntermediateSteps }
        | .char 'r' => { s with cameraYaw := 0.5, cameraPitch := 0.3 }
        | _ => s
      else s
    ) keyEvents

  let clickEvents ← useClickData mat3dName
  let clickUpdates ← Event.mapM (fun data =>
    if data.click.button != 0 then
      id
    else
      fun (s : Demos.Linalg.Matrix3DTransformState) =>
        { s with dragging := true, lastMouseX := data.click.x, lastMouseY := data.click.y }
    ) clickEvents

  let hoverEvents ← useAllHovers
  let hoverUpdates ← Event.mapM (fun data =>
    fun (state : Demos.Linalg.Matrix3DTransformState) =>
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
    fun (s : Demos.Linalg.Matrix3DTransformState) =>
      if data.button == 0 then
        { s with dragging := false }
      else s
    ) mouseUpEvents

  let allUpdates ← Event.mergeAllListM [keyUpdates, clickUpdates, hoverUpdates, mouseUpUpdates]
  let state ← foldDyn (fun f s => f s) Demos.Linalg.matrix3DTransformInitialState allUpdates

  let _ ← dynWidget state fun s => do
    let containerStyle : BoxStyle := {
      flexItem := some (FlexItem.growing 1)
      width := .percent 1.0
      height := .percent 1.0
    }
    emit (pure (namedColumn mat3dName 0 containerStyle #[
      Demos.Linalg.matrix3DTransformWidget env s
    ]))
  pure ()

end Demos
