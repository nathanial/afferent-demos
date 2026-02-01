/-
  Demo Runner - Canopy app linalg BezierPatchSurface tab content.
-/
import Reactive
import Afferent
import Afferent.Canopy
import Afferent.Canopy.Reactive
import Demos.Core.Demo
import Demos.Linalg.BezierPatchSurface
import Trellis

open Reactive Reactive.Host
open Afferent
open Afferent.Arbor
open Afferent.Canopy
open Afferent.Canopy.Reactive
open Trellis

namespace Demos
def bezierPatchSurfaceTabContent (env : DemoEnv) : WidgetM Unit := do
  let elapsedTime ← useElapsedTime
  let patchName ← registerComponentW "bezier-patch-surface"

  let clickEvents ← useClickData patchName
  let clickUpdates ← Event.mapM (fun data =>
    match data.click.button with
    | 1 =>
        fun (s : Demos.Linalg.BezierPatchSurfaceState) =>
          { s with dragging := .camera, lastMouseX := data.click.x, lastMouseY := data.click.y }
    | 0 =>
        match data.nameMap.get? patchName with
        | some wid =>
            match data.layouts.get wid with
            | some layout =>
                let rect := layout.contentRect
                let localX := data.click.x - rect.x
                let localY := data.click.y - rect.y
                let rectX := rect.width - 260.0 * env.screenScale
                let rectY := 110.0 * env.screenScale
                let rectW := 220.0 * env.screenScale
                let rectH := 220.0 * env.screenScale
                let withinMini := localX >= rectX && localX <= rectX + rectW
                  && localY >= rectY && localY <= rectY + rectH
                if withinMini then
                  let origin := (rectX + rectW / 2, rectY + rectH / 2)
                  let scale := rectW / 5.8
                  let worldPos := Demos.Linalg.screenToWorld (localX, localY) origin scale
                  fun (state : Demos.Linalg.BezierPatchSurfaceState) =>
                    let hit := (Array.range 16).findSome? fun idx =>
                      let row := idx / 4
                      let col := idx % 4
                      let p := state.patch.getPoint row col
                      let p2 := Linalg.Vec2.mk p.x p.y
                      if Demos.Linalg.nearPoint worldPos p2 0.35 then some idx else none
                    match hit with
                    | some idx => { state with selected := some idx, dragging := .point idx }
                    | none => state
                else
                  id
            | none => id
        | none => id
    | _ => id
    ) clickEvents

  let hoverEvents ← useAllHovers
  let hoverUpdates ← Event.mapM (fun data =>
    match data.nameMap.get? patchName with
    | some wid =>
        match data.layouts.get wid with
        | some layout =>
            let rect := layout.contentRect
            let localX := data.x - rect.x
            let localY := data.y - rect.y
            fun (state : Demos.Linalg.BezierPatchSurfaceState) =>
              match state.dragging with
              | .none => state
              | .camera =>
                  let dx := data.x - state.lastMouseX
                  let dy := data.y - state.lastMouseY
                  let yaw := state.cameraYaw + dx * 0.005
                  let pitch := state.cameraPitch + dy * 0.005
                  { state with cameraYaw := yaw, cameraPitch := pitch, lastMouseX := data.x, lastMouseY := data.y }
              | .point idx =>
                  let rectX := rect.width - 260.0 * env.screenScale
                  let rectY := 110.0 * env.screenScale
                  let rectW := 220.0 * env.screenScale
                  let rectH := 220.0 * env.screenScale
                  let origin := (rectX + rectW / 2, rectY + rectH / 2)
                  let scale := rectW / 5.8
                  let worldPos := Demos.Linalg.screenToWorld (localX, localY) origin scale
                  let row := idx / 4
                  let col := idx % 4
                  let p := state.patch.getPoint row col
                  let patch := state.patch.setPoint row col (Linalg.Vec3.mk worldPos.x worldPos.y p.z)
                  { state with patch := patch }
        | none => id
    | none => id
    ) hoverEvents

  let mouseUpEvents ← useAllMouseUp
  let mouseUpUpdates ← Event.mapM (fun _ =>
    fun (s : Demos.Linalg.BezierPatchSurfaceState) => { s with dragging := .none }
    ) mouseUpEvents

  let keyEvents ← useKeyboard
  let keyUpdates ← Event.mapM (fun data =>
    fun (s : Demos.Linalg.BezierPatchSurfaceState) =>
      if data.event.isPress then
        match data.event.key with
        | .char 'r' => Demos.Linalg.bezierPatchSurfaceInitialState
        | .char 'n' => { s with showNormals := !s.showNormals }
        | .left =>
            let newTess := if s.tessellation > 2 then s.tessellation - 1 else 2
            { s with tessellation := newTess }
        | .right =>
            let newTess := if s.tessellation < 18 then s.tessellation + 1 else 18
            { s with tessellation := newTess }
        | .up | .down =>
            let delta := if data.event.key == .up then 0.2 else -0.2
            match s.selected with
            | some idx =>
                let row := idx / 4
                let col := idx % 4
                let p := s.patch.getPoint row col
                let patch := s.patch.setPoint row col (Linalg.Vec3.mk p.x p.y (p.z + delta))
                { s with patch := patch }
            | none => s
        | _ => s
      else s
    ) keyEvents

  let allUpdates ← Event.mergeAllListM [clickUpdates, hoverUpdates, mouseUpUpdates, keyUpdates]
  let state ← foldDyn (fun f s => f s) Demos.Linalg.bezierPatchSurfaceInitialState allUpdates

  let _ ← dynWidget state fun s => do
    let containerStyle : BoxStyle := {
      flexItem := some (FlexItem.growing 1)
      width := .percent 1.0
      height := .percent 1.0
    }
    emit (pure (namedColumn patchName 0 containerStyle #[
      Demos.Linalg.bezierPatchSurfaceWidget env s
    ]))
  pure ()

end Demos
