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
  let elapsedTime ← useElapsedTime
  let stateRef ← SpiderM.liftIO (IO.mkRef Demos.Linalg.slerpInterpolationInitialState)
  let lastTimeRef ← SpiderM.liftIO (IO.mkRef 0.0)
  let slerpName ← registerComponentW "slerp-interpolation"

  let clickEvents ← useClickData slerpName
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

  let keyEvents ← useKeyboard
  let keyAction ← Event.mapM (fun data => do
    if data.event.key == .space && data.event.isPress then
      stateRef.modify fun s => { s with animating := !s.animating }
    ) keyEvents
  performEvent_ keyAction

  let _ ← dynWidget elapsedTime fun t => do
    let state ← SpiderM.liftIO do
      let lastT ← lastTimeRef.get
      let dt := if lastT == 0.0 then 0.0 else max 0.0 (t - lastT)
      let mut current ← stateRef.get
      if current.animating then
        let newT := current.t + dt * 0.35
        current := { current with t := if newT > 1.0 then newT - 1.0 else newT }
        stateRef.set current
      lastTimeRef.set t
      pure current
    let containerStyle : BoxStyle := {
      flexItem := some (FlexItem.growing 1)
      width := .percent 1.0
      height := .percent 1.0
    }
    emit (pure (namedColumn slerpName 0 containerStyle #[
      Demos.Linalg.slerpInterpolationWidget env state
    ]))
  pure ()

end Demos
