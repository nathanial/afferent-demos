/-
  Demo Runner - Canopy app linalg FrustumCullingDemo tab content.
-/
import Reactive
import Afferent
import Afferent.Canopy
import Afferent.Canopy.Reactive
import Demos.Core.Demo
import Demos.Linalg.FrustumCullingDemo
import Trellis

open Reactive Reactive.Host
open Afferent
open Afferent.Arbor
open Afferent.Canopy
open Afferent.Canopy.Reactive
open Trellis

namespace Demos
def frustumCullingDemoTabContent (env : DemoEnv) : WidgetM Unit := do
  let elapsedTime ← useElapsedTime
  let stateRef ← SpiderM.liftIO (IO.mkRef Demos.Linalg.frustumCullingDemoInitialState)
  let frustumName ← registerComponentW "frustum-culling-demo"

  let keyEvents ← useKeyboard
  let keyAction ← Event.mapM (fun data => do
    if data.event.isPress then
      match data.event.key with
      | .char 'r' =>
          stateRef.set Demos.Linalg.frustumCullingDemoInitialState
      | .char 'j' =>
          stateRef.modify fun s => { s with camYaw := s.camYaw - 0.08 }
      | .char 'l' =>
          stateRef.modify fun s => { s with camYaw := s.camYaw + 0.08 }
      | .char 'i' =>
          stateRef.modify fun s =>
            let newPitch := s.camPitch + 0.08
            { s with camPitch := if newPitch > 1.2 then 1.2 else newPitch }
      | .char 'k' =>
          stateRef.modify fun s =>
            let newPitch := s.camPitch - 0.08
            { s with camPitch := if newPitch < -1.2 then -1.2 else newPitch }
      | .char '=' | .char '+' =>
          stateRef.modify fun s =>
            let newDist := s.camDist - 0.3
            { s with camDist := if newDist < 2.0 then 2.0 else newDist }
      | .char '-' =>
          stateRef.modify fun s => { s with camDist := s.camDist + 0.3 }
      | _ => pure ()
    ) keyEvents
  performEvent_ keyAction

  let clickEvents ← useClickData frustumName
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
      let newYaw := state.viewYaw + dx * 0.005
      let newPitch := state.viewPitch + dy * 0.005
      stateRef.set { state with
        viewYaw := newYaw
        viewPitch := newPitch
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
    emit (pure (namedColumn frustumName 0 containerStyle #[
      Demos.Linalg.frustumCullingDemoWidget env state
    ]))
  pure ()

end Demos
