/-
  Demo Runner - Canopy app linalg QuadtreeVisualizer tab content.
-/
import Reactive
import Afferent
import Afferent.Canopy
import Afferent.Canopy.Reactive
import Demos.Core.Demo
import Demos.Linalg.QuadtreeVisualizer
import Trellis

open Reactive Reactive.Host
open Afferent
open Afferent.Arbor
open Afferent.Canopy
open Afferent.Canopy.Reactive
open Trellis

namespace Demos

private def updateQuerySize (scale : Float) (state : Demos.Linalg.QuadtreeVisualizerState)
    : Demos.Linalg.QuadtreeVisualizerState :=
  let minSize := 0.3
  let maxSize := 4.5
  let newExtents := Linalg.Vec2.mk
    (Linalg.Float.clamp (state.queryExtents.x * scale) minSize maxSize)
    (Linalg.Float.clamp (state.queryExtents.y * scale) minSize maxSize)
  let newRadius := Linalg.Float.clamp (state.queryRadius * scale) minSize maxSize
  { state with queryExtents := newExtents, queryRadius := newRadius }

def quadtreeVisualizerTabContent (env : DemoEnv) : WidgetM Unit := do
  let demoName ← registerComponentW "quadtree-visualizer"

  let keyEvents ← useKeyboard
  let keyUpdates ← Event.mapM (fun data =>
    fun (s : Demos.Linalg.QuadtreeVisualizerState) =>
      if data.event.isPress then
        match data.event.key with
        | .char 'r' => Demos.Linalg.quadtreeVisualizerInitialState
        | .char 'q' =>
            let mode := if s.queryMode == .rect then .circle else .rect
            { s with queryMode := mode }
        | .char '1' => { s with config := Linalg.Spatial.TreeConfig.default }
        | .char '2' => { s with config := Linalg.Spatial.TreeConfig.dense }
        | .char '3' => { s with config := Linalg.Spatial.TreeConfig.sparse }
        | .char 'k' => { s with showNearest := !s.showNearest }
        | .char '[' =>
            let k := if s.kCount > 1 then s.kCount - 1 else 1
            { s with kCount := k }
        | .char ']' =>
            let k := if s.kCount < 8 then s.kCount + 1 else s.kCount
            { s with kCount := k }
        | .char '-' => updateQuerySize 0.9 s
        | .char '+' => updateQuerySize 1.1 s
        | .char '=' => updateQuerySize 1.1 s
        | .char 'w' => { s with queryCenter := s.queryCenter.add (Linalg.Vec2.mk 0.0 0.3) }
        | .char 's' => { s with queryCenter := s.queryCenter.add (Linalg.Vec2.mk 0.0 (-0.3)) }
        | .char 'a' => { s with queryCenter := s.queryCenter.add (Linalg.Vec2.mk (-0.3) 0.0) }
        | .char 'd' => { s with queryCenter := s.queryCenter.add (Linalg.Vec2.mk 0.3 0.0) }
        | _ => s
      else s
    ) keyEvents

  let clickEvents ← useClickData demoName
  let clickUpdates ← Event.mapM (fun data =>
    if data.click.button != 0 && data.click.button != 1 then
      id
    else
      match data.nameMap.get? demoName with
      | some wid =>
          match data.layouts.get wid with
          | some layout =>
              let rect := layout.contentRect
              let localX := data.click.x - rect.x
              let localY := data.click.y - rect.y
              let config := Demos.Linalg.quadtreeMathViewConfig env.screenScale
              let view := Afferent.Widget.MathView2D.viewForSize config rect.width rect.height
              let worldPos := Afferent.Widget.MathView2D.screenToWorld view (localX, localY)
              let button := data.click.button
              fun (state : Demos.Linalg.QuadtreeVisualizerState) =>
                if button == 1 then
                  { state with queryCenter := worldPos }
                else
                  { state with points := state.points.push worldPos }
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
            let config := Demos.Linalg.quadtreeMathViewConfig env.screenScale
            let view := Afferent.Widget.MathView2D.viewForSize config rect.width rect.height
            let worldPos := Afferent.Widget.MathView2D.screenToWorld view (localX, localY)
            fun (state : Demos.Linalg.QuadtreeVisualizerState) =>
              { state with hoverPos := some worldPos }
        | none => id
    | none => id
    ) hoverEvents

  let allUpdates ← Event.mergeAllListM [keyUpdates, clickUpdates, hoverUpdates]
  let state ← foldDyn (fun f s => f s) Demos.Linalg.quadtreeVisualizerInitialState allUpdates

  let _ ← dynWidget state fun s => do
    let containerStyle : BoxStyle := {
      flexItem := some (FlexItem.growing 1)
      width := .percent 1.0
      height := .percent 1.0
    }
    emit (pure (namedColumn demoName 0 containerStyle #[
      Demos.Linalg.quadtreeVisualizerWidget env s
    ]))
  pure ()

end Demos
