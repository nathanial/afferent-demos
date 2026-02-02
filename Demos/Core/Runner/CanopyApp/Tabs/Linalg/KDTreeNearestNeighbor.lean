/-
  Demo Runner - Canopy app linalg KDTreeNearestNeighbor tab content.
-/
import Reactive
import Afferent
import Afferent.Canopy
import Afferent.Canopy.Reactive
import Demos.Core.Demo
import Demos.Linalg.KDTreeNearestNeighbor
import Trellis

open Reactive Reactive.Host
open Afferent
open Afferent.Arbor
open Afferent.Canopy
open Afferent.Canopy.Reactive
open Trellis

namespace Demos

private def updateRadius (scale : Float) (state : Demos.Linalg.KDTreeNearestNeighborState)
    : Demos.Linalg.KDTreeNearestNeighborState :=
  let minR := 0.3
  let maxR := 4.5
  { state with radius := Linalg.Float.clamp (state.radius * scale) minR maxR }

def kdTreeNearestNeighborTabContent (env : DemoEnv) : WidgetM Unit := do
  let demoName ← registerComponentW "kd-tree-nearest-neighbor"

  let keyEvents ← useKeyboard
  let keyUpdates ← Event.mapM (fun data =>
    fun (s : Demos.Linalg.KDTreeNearestNeighborState) =>
      if data.event.isPress then
        match data.event.key with
        | .char 'r' => Demos.Linalg.kdTreeNearestNeighborInitialState
        | .char '-' => updateRadius 0.9 s
        | .char '+' => updateRadius 1.1 s
        | .char '=' => updateRadius 1.1 s
        | .char '[' =>
            let k := if s.kCount > 1 then s.kCount - 1 else 1
            { s with kCount := k }
        | .char ']' =>
            let k := if s.kCount < 8 then s.kCount + 1 else s.kCount
            { s with kCount := k }
        | _ => s
      else s
    ) keyEvents

  let clickEvents ← useClickData demoName
  let clickUpdates ← Event.mapM (fun data =>
    if data.click.button != 0 then
      id
    else
      match data.nameMap.get? demoName with
      | some wid =>
          match data.layouts.get wid with
          | some layout =>
              let rect := layout.contentRect
              let localX := data.click.x - rect.x
              let localY := data.click.y - rect.y
              let config := Demos.Linalg.kdTreeMathViewConfig env.screenScale
              let view := Afferent.Widget.MathView2D.viewForSize config rect.width rect.height
              let worldPos := Afferent.Widget.MathView2D.screenToWorld view (localX, localY)
              fun (state : Demos.Linalg.KDTreeNearestNeighborState) =>
                if Demos.Linalg.nearPoint worldPos state.queryPoint 0.5 then
                  { state with dragging := true }
                else
                  { state with queryPoint := worldPos, dragging := true }
          | none => id
      | none => id
    ) clickEvents

  let hoverEvents ← useAllHovers
  let hoverUpdates ← Event.mapM (fun data =>
    match data.nameMap.get? demoName with
    | some wid =>
        match data.layouts.get wid with
        | some layout =>
            let rect := layout.contentRect
            let localX := data.x - rect.x
            let localY := data.y - rect.y
            let config := Demos.Linalg.kdTreeMathViewConfig env.screenScale
            let view := Afferent.Widget.MathView2D.viewForSize config rect.width rect.height
            let worldPos := Afferent.Widget.MathView2D.screenToWorld view (localX, localY)
            fun (state : Demos.Linalg.KDTreeNearestNeighborState) =>
              if state.dragging then
                { state with queryPoint := worldPos }
              else state
        | none => id
    | none => id
    ) hoverEvents

  let mouseUpEvents ← useAllMouseUp
  let mouseUpUpdates ← Event.mapM (fun _ =>
    fun (s : Demos.Linalg.KDTreeNearestNeighborState) => { s with dragging := false }
    ) mouseUpEvents

  let allUpdates ← Event.mergeAllListM [keyUpdates, clickUpdates, hoverUpdates, mouseUpUpdates]
  let state ← foldDyn (fun f s => f s) Demos.Linalg.kdTreeNearestNeighborInitialState allUpdates

  let _ ← dynWidget state fun s => do
    let containerStyle : BoxStyle := {
      flexItem := some (FlexItem.growing 1)
      width := .percent 1.0
      height := .percent 1.0
    }
    emit (pure (namedColumn demoName 0 containerStyle #[
      Demos.Linalg.kdTreeNearestNeighborWidget env s
    ]))
  pure ()

end Demos
