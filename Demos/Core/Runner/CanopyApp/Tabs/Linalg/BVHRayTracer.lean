/-
  Demo Runner - Canopy app linalg BVHRayTracer tab content.
-/
import Reactive
import Afferent
import Afferent.Canopy
import Afferent.Canopy.Reactive
import Demos.Core.Demo
import Demos.Linalg.BVHRayTracer
import Trellis

open Reactive Reactive.Host
open Afferent
open Afferent.Arbor
open Afferent.Canopy
open Afferent.Canopy.Reactive
open Trellis

namespace Demos

def bvhRayTracerTabContent (env : DemoEnv) : WidgetM Unit := do
  let demoName ← registerComponentW "bvh-ray-tracer"

  let keyEvents ← useKeyboard
  let keyUpdates ← Event.mapM (fun data =>
    fun (s : Demos.Linalg.BVHRayTracerState) =>
      if data.event.isPress then
        match data.event.key with
        | .char 'r' => Demos.Linalg.bvhRayTracerInitialState
        | .char 'v' => { s with showVisited := !s.showVisited }
        | .char 'b' => { s with showNodes := !s.showNodes }
        | _ => s
      else s
    ) keyEvents

  let hoverEvents ← useAllHovers
  let hoverUpdates ← Event.mapM (fun data =>
    match data.nameMap.get? demoName with
    | some wid =>
        match data.layouts.get wid with
        | some layout =>
            let rect := layout.contentRect
            let localX := data.x - rect.x
            let localY := data.y - rect.y
            let config := Demos.Linalg.bvhRayTracerMathViewConfig env.screenScale
            let view := Afferent.Widget.MathView2D.viewForSize config rect.width rect.height
            let worldPos := Afferent.Widget.MathView2D.screenToWorld view (localX, localY)
            fun (state : Demos.Linalg.BVHRayTracerState) =>
              { state with rayOrigin := worldPos }
        | none => id
    | none => id
    ) hoverEvents

  let allUpdates ← Event.mergeAllListM [keyUpdates, hoverUpdates]
  let state ← foldDyn (fun f s => f s) Demos.Linalg.bvhRayTracerInitialState allUpdates

  let _ ← dynWidget state fun s => do
    let containerStyle : BoxStyle := {
      flexItem := some (FlexItem.growing 1)
      width := .percent 1.0
      height := .percent 1.0
    }
    emit (pure (namedColumn demoName 0 containerStyle #[
      Demos.Linalg.bvhRayTracerWidget env s
    ]))
  pure ()

end Demos
