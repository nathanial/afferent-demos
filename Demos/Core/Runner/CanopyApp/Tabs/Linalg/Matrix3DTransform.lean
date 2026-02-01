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
  let stateRef ← SpiderM.liftIO (IO.mkRef Demos.Linalg.matrix3DTransformInitialState)
  let mat3dName ← registerComponentW "matrix-3d-transform"

  let keyEvents ← useKeyboard
  let keyAction ← Event.mapM (fun data => do
    if data.event.isPress then
      match data.event.key with
      | .char '1' =>
          stateRef.modify fun s => { s with selectedIndex := some 0 }
      | .char '2' =>
          stateRef.modify fun s => { s with selectedIndex := some 1 }
      | .char '3' =>
          stateRef.modify fun s => { s with selectedIndex := some 2 }
      | .up =>
          stateRef.modify fun s =>
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
          stateRef.modify fun s =>
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
      | .char 'a' =>
          stateRef.modify fun s => { s with showAxes := !s.showAxes }
      | .char 'i' =>
          stateRef.modify fun s => { s with showIntermediateSteps := !s.showIntermediateSteps }
      | .char 'r' =>
          stateRef.modify fun s => { s with cameraYaw := 0.5, cameraPitch := 0.3 }
      | _ => pure ()
    ) keyEvents
  performEvent_ keyAction

  let clickEvents ← useClickData mat3dName
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
    emit (pure (namedColumn mat3dName 0 containerStyle #[
      Demos.Linalg.matrix3DTransformWidget env state
    ]))
  pure ()

end Demos
