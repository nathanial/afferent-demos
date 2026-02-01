/-
  Demo Runner - Canopy app linalg SlerpInterpolation tab content.
-/
import Reactive
import Afferent
import Afferent.Canopy
import Afferent.Canopy.Reactive
import Demos.Core.Demo
import Demos.Linalg.SlerpInterpolation
import Trellis

open Reactive Reactive.Host
open Afferent
open Afferent.Arbor
open Afferent.Canopy
open Afferent.Canopy.Reactive
open Trellis

namespace Demos
def slerpInterpolationTabContent (env : DemoEnv) : WidgetM Unit := do
  let animFrame ← useAnimationFrame
  let slerpName ← registerComponentW "slerp-interpolation"

  let clickEvents ← useClickData slerpName
  let clickUpdates ← Event.mapM (fun data =>
    if data.click.button != 0 then
      id
    else
      fun (s : Demos.Linalg.SlerpInterpolationState) =>
        { s with dragging := true, lastMouseX := data.click.x, lastMouseY := data.click.y }
    ) clickEvents

  let hoverEvents ← useAllHovers
  let hoverUpdates ← Event.mapM (fun data =>
    fun (state : Demos.Linalg.SlerpInterpolationState) =>
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
    fun (s : Demos.Linalg.SlerpInterpolationState) =>
      if data.button == 0 then { s with dragging := false } else s
    ) mouseUpEvents

  let keyEvents ← useKeyboard
  let keyUpdates ← Event.mapM (fun data =>
    fun (s : Demos.Linalg.SlerpInterpolationState) =>
      if data.event.key == .space && data.event.isPress then
        { s with animating := !s.animating }
      else s
    ) keyEvents

  let dtUpdates ← Event.mapM (fun dt =>
    fun (s : Demos.Linalg.SlerpInterpolationState) =>
      if s.animating then
        let newT := s.t + dt * 0.35
        { s with t := if newT > 1.0 then newT - 1.0 else newT }
      else s
    ) animFrame

  let allUpdates ← Event.mergeAllListM [clickUpdates, hoverUpdates, mouseUpUpdates, keyUpdates, dtUpdates]
  let state ← foldDyn (fun f s => f s) Demos.Linalg.slerpInterpolationInitialState allUpdates

  let _ ← dynWidget state fun s => do
    let containerStyle : BoxStyle := {
      flexItem := some (FlexItem.growing 1)
      width := .percent 1.0
      height := .percent 1.0
    }
    emit (pure (namedColumn slerpName 0 containerStyle #[
      Demos.Linalg.slerpInterpolationWidget env s
    ]))
  pure ()

end Demos
