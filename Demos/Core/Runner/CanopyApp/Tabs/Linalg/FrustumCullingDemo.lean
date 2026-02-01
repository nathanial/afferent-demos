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
  let frustumName ← registerComponentW "frustum-culling-demo"

  let keyEvents ← useKeyboard
  let keyUpdates ← Event.mapM (fun data =>
    fun (s : Demos.Linalg.FrustumCullingDemoState) =>
      if data.event.isPress then
        match data.event.key with
        | .char 'r' => Demos.Linalg.frustumCullingDemoInitialState
        | .char 'j' => { s with camYaw := s.camYaw - 0.08 }
        | .char 'l' => { s with camYaw := s.camYaw + 0.08 }
        | .char 'i' =>
            let newPitch := s.camPitch + 0.08
            { s with camPitch := if newPitch > 1.2 then 1.2 else newPitch }
        | .char 'k' =>
            let newPitch := s.camPitch - 0.08
            { s with camPitch := if newPitch < -1.2 then -1.2 else newPitch }
        | .char '=' | .char '+' =>
            let newDist := s.camDist - 0.3
            { s with camDist := if newDist < 2.0 then 2.0 else newDist }
        | .char '-' => { s with camDist := s.camDist + 0.3 }
        | _ => s
      else s
    ) keyEvents

  let clickEvents ← useClickData frustumName
  let clickUpdates ← Event.mapM (fun data =>
    if data.click.button != 0 then
      id
    else
      fun (s : Demos.Linalg.FrustumCullingDemoState) =>
        { s with dragging := true, lastMouseX := data.click.x, lastMouseY := data.click.y }
    ) clickEvents

  let hoverEvents ← useAllHovers
  let hoverUpdates ← Event.mapM (fun data =>
    fun (state : Demos.Linalg.FrustumCullingDemoState) =>
      if !state.dragging then
        state
      else
        let dx := data.x - state.lastMouseX
        let dy := data.y - state.lastMouseY
        let newYaw := state.viewYaw + dx * 0.005
        let newPitch := state.viewPitch + dy * 0.005
        { state with
          viewYaw := newYaw
          viewPitch := newPitch
          lastMouseX := data.x
          lastMouseY := data.y
        }
    ) hoverEvents

  let mouseUpEvents ← useAllMouseUp
  let mouseUpUpdates ← Event.mapM (fun data =>
    fun (s : Demos.Linalg.FrustumCullingDemoState) =>
      if data.button == 0 then
        { s with dragging := false }
      else s
    ) mouseUpEvents

  let allUpdates ← Event.mergeAllListM [keyUpdates, clickUpdates, hoverUpdates, mouseUpUpdates]
  let state ← foldDyn (fun f s => f s) Demos.Linalg.frustumCullingDemoInitialState allUpdates

  let _ ← dynWidget state fun s => do
    let containerStyle : BoxStyle := {
      flexItem := some (FlexItem.growing 1)
      width := .percent 1.0
      height := .percent 1.0
    }
    emit (pure (namedColumn frustumName 0 containerStyle #[
      Demos.Linalg.frustumCullingDemoWidget env s
    ]))
  pure ()

end Demos
