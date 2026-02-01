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
def dualQuaternionBlendingTabContent (env : DemoEnv) (elapsedTime : Dynamic Spider Float)
    (stateRef : IO.Ref Demos.Linalg.DualQuaternionBlendingState) : WidgetM Unit := do
  let dualName ← registerComponentW "dual-quaternion-blending"

  let keyEvents ← useKeyboard
  let keyAction ← Event.mapM (fun data => do
    if data.event.isPress then
      match data.event.key with
      | .char 'r' =>
          stateRef.set Demos.Linalg.dualQuaternionBlendingInitialState
      | .char 't' =>
          stateRef.modify fun s => { s with twist := s.twist + 0.1 }
      | .char 'g' =>
          stateRef.modify fun s => { s with twist := s.twist - 0.1 }
      | .char 'b' =>
          stateRef.modify fun s => { s with bend := s.bend + 0.1 }
      | .char 'v' =>
          stateRef.modify fun s => { s with bend := s.bend - 0.1 }
      | _ => pure ()
    ) keyEvents
  performEvent_ keyAction

  let clickEvents ← useClickData dualName
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
    emit (pure (namedColumn dualName 0 containerStyle #[
      Demos.Linalg.dualQuaternionBlendingWidget env state
    ]))
  pure ()

end Demos
