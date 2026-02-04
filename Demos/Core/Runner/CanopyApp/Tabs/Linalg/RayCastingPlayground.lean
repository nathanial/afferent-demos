/-
  Demo Runner - Canopy app linalg RayCastingPlayground tab content.
-/
import Reactive
import Afferent
import Afferent.Canopy
import Afferent.Canopy.Reactive
import Demos.Core.Demo
import Demos.Linalg.RayCastingPlayground
import Trellis
import AfferentMath.Widget.MathView3D

open Reactive Reactive.Host
open Afferent
open Afferent.Arbor
open Afferent.Canopy
open Afferent.Canopy.Reactive
open Trellis
open AfferentMath.Widget

namespace Demos
def rayCastingPlaygroundTabContent (env : DemoEnv) : WidgetM Unit := do
  let rayName ← registerComponentW "ray-casting-playground"

  let keyEvents ← useKeyboard
  let keyUpdates ← Event.mapM (fun data =>
    fun (s : Demos.Linalg.RayCastingPlaygroundState) =>
      if data.event.key == .char 'r' && data.event.isPress then
        Demos.Linalg.rayCastingPlaygroundInitialState
      else s
    ) keyEvents

  let clickEvents ← useClickData rayName
  let clickUpdates ← Event.mapM (fun data =>
    if data.click.button != 0 && data.click.button != 1 then
      id
    else
      match data.nameMap.get? rayName with
      | some wid =>
          match data.layouts.get wid with
          | some layout =>
              let rect := layout.contentRect
              let localX := data.click.x - rect.x
              let localY := data.click.y - rect.y
              let button := data.click.button
              fun (state : Demos.Linalg.RayCastingPlaygroundState) =>
                let config := Demos.Linalg.rayCastingPlaygroundMathViewConfig state env.screenScale
                let view := AfferentMath.Widget.MathView3D.viewForSize config rect.width rect.height
                let worldOpt := AfferentMath.Widget.MathView3D.screenToWorldOnPlane view (localX, localY)
                  Linalg.Vec3.zero Linalg.Vec3.unitY
                let origin2 := Linalg.Vec2.mk state.rayOrigin.x state.rayOrigin.z
                let target2 := Linalg.Vec2.mk state.rayTarget.x state.rayTarget.z
                if button == 1 then
                  { state with dragging := .camera, lastMouseX := data.click.x, lastMouseY := data.click.y }
                else
                  match worldOpt with
                  | some worldPos =>
                      let world2 := Linalg.Vec2.mk worldPos.x worldPos.z
                      if Demos.Linalg.nearPoint world2 origin2 0.5 then
                        { state with dragging := .origin, lastMouseX := data.click.x, lastMouseY := data.click.y }
                      else if Demos.Linalg.nearPoint world2 target2 0.5 then
                        { state with dragging := .direction, lastMouseX := data.click.x, lastMouseY := data.click.y }
                      else
                        state
                  | none => state
          | none => id
      | none => id
    ) clickEvents

  let hoverEvents ← useAllHovers
  let hoverUpdates ← Event.mapM (fun data =>
    match data.nameMap.get? rayName with
    | some wid =>
        match data.layouts.get wid with
        | some layout =>
            let rect := layout.contentRect
            let localX := data.x - rect.x
            let localY := data.y - rect.y
            fun (state : Demos.Linalg.RayCastingPlaygroundState) =>
              match state.dragging with
              | .none => state
              | .camera =>
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
              | .origin =>
                  let config := Demos.Linalg.rayCastingPlaygroundMathViewConfig state env.screenScale
                  let view := AfferentMath.Widget.MathView3D.viewForSize config rect.width rect.height
                  match AfferentMath.Widget.MathView3D.screenToWorldOnPlane view (localX, localY)
                    Linalg.Vec3.zero Linalg.Vec3.unitY with
                  | some worldPos =>
                      let newOrigin := Linalg.Vec3.mk worldPos.x 0.0 worldPos.z
                      { state with rayOrigin := newOrigin, lastMouseX := data.x, lastMouseY := data.y }
                  | none => state
              | .direction =>
                  let config := Demos.Linalg.rayCastingPlaygroundMathViewConfig state env.screenScale
                  let view := AfferentMath.Widget.MathView3D.viewForSize config rect.width rect.height
                  match AfferentMath.Widget.MathView3D.screenToWorldOnPlane view (localX, localY)
                    Linalg.Vec3.zero Linalg.Vec3.unitY with
                  | some worldPos =>
                      let newTarget := Linalg.Vec3.mk worldPos.x 0.0 worldPos.z
                      { state with rayTarget := newTarget, lastMouseX := data.x, lastMouseY := data.y }
                  | none => state
        | none => id
    | none => id
    ) hoverEvents

  let mouseUpEvents ← useAllMouseUp
  let mouseUpUpdates ← Event.mapM (fun _ =>
    fun (s : Demos.Linalg.RayCastingPlaygroundState) => { s with dragging := .none }
    ) mouseUpEvents

  let allUpdates ← Event.mergeAllListM [keyUpdates, clickUpdates, hoverUpdates, mouseUpUpdates]
  let state ← foldDyn (fun f s => f s) Demos.Linalg.rayCastingPlaygroundInitialState allUpdates

  let _ ← dynWidget state fun s => do
    let containerStyle : BoxStyle := {
      flexItem := some (FlexItem.growing 1)
      width := .percent 1.0
      height := .percent 1.0
    }
    emit (pure (namedColumn rayName 0 containerStyle #[
      Demos.Linalg.rayCastingPlaygroundWidget env s
    ]))
  pure ()

end Demos
