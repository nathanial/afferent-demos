/-
  Demo Runner - Canopy app linalg SweptCollisionDemo tab content.
-/
import Reactive
import Afferent
import Afferent.Canopy
import Afferent.Canopy.Reactive
import Demos.Core.Demo
import Demos.Linalg.SweptCollisionDemo
import Trellis
import AfferentMath.Widget.MathView2D

open Reactive Reactive.Host
open Afferent
open Afferent.Arbor
open Afferent.Canopy
open Afferent.Canopy.Reactive
open Trellis
open AfferentMath.Widget

namespace Demos

private def nextMode (mode : Demos.Linalg.SweptMode) : Demos.Linalg.SweptMode :=
  match mode with
  | .sphere => .aabb
  | .aabb => .sphere

private def pickDragTarget (state : Demos.Linalg.SweptCollisionDemoState)
    (pos : Linalg.Vec2) : Option Demos.Linalg.SweptDragTarget := Id.run do
  let threshold := 0.55
  let candidates : Array (Demos.Linalg.SweptDragTarget × Linalg.Vec2) := #[
    (.startPos, state.startPos),
    (.endPos, state.endPos),
    (.staticCenter, state.staticCenter)
  ]
  let mut best : Option (Demos.Linalg.SweptDragTarget × Float) := none
  for (target, p) in candidates do
    let d := p.distance pos
    if d < threshold then
      match best with
      | some (_, bestD) =>
          if d < bestD then
            best := some (target, d)
      | none =>
          best := some (target, d)
  return best.map (fun (t, _) => t)

def sweptCollisionDemoTabContent (env : DemoEnv) : WidgetM Unit := do
  let animFrame ← useAnimationFrame
  let demoName ← registerComponentW "swept-collision-demo"

  let keyEvents ← useKeyboard
  let keyUpdates ← Event.mapM (fun data =>
    fun (s : Demos.Linalg.SweptCollisionDemoState) =>
      if data.event.isPress then
        match data.event.key with
        | .char 'r' => Demos.Linalg.sweptCollisionDemoInitialState
        | .space => { s with animating := !s.animating }
        | .char 'm' => { s with mode := nextMode s.mode }
        | .char 'd' => { s with showDiscrete := !s.showDiscrete }
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
              let config := Demos.Linalg.sweptCollisionMathViewConfig env.screenScale
              let view := AfferentMath.Widget.MathView2D.viewForSize config rect.width rect.height
              let world := AfferentMath.Widget.MathView2D.screenToWorld view (localX, localY)
              fun (state : Demos.Linalg.SweptCollisionDemoState) =>
                match pickDragTarget state world with
                | some target => { state with dragging := some target }
                | none => state
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
            let config := Demos.Linalg.sweptCollisionMathViewConfig env.screenScale
            let view := AfferentMath.Widget.MathView2D.viewForSize config rect.width rect.height
            let world := AfferentMath.Widget.MathView2D.screenToWorld view (localX, localY)
            fun (state : Demos.Linalg.SweptCollisionDemoState) =>
              match state.dragging with
              | some .startPos => { state with startPos := world }
              | some .endPos => { state with endPos := world }
              | some .staticCenter => { state with staticCenter := world }
              | none => state
        | none => id
    | none => id
    ) hoverEvents

  let mouseUpEvents ← useAllMouseUp
  let mouseUpUpdates ← Event.mapM (fun _ =>
    fun (s : Demos.Linalg.SweptCollisionDemoState) => { s with dragging := none }
    ) mouseUpEvents

  let animUpdates ← Event.mapM (fun dt =>
    fun (s : Demos.Linalg.SweptCollisionDemoState) =>
      if s.animating then { s with time := s.time + dt } else s
    ) animFrame

  let allUpdates ← Event.mergeAllListM [keyUpdates, clickUpdates, hoverUpdates, mouseUpUpdates, animUpdates]
  let state ← foldDyn (fun f s => f s) Demos.Linalg.sweptCollisionDemoInitialState allUpdates

  let _ ← dynWidget state fun s => do
    let containerStyle : BoxStyle := {
      flexItem := some (FlexItem.growing 1)
      width := .percent 1.0
      height := .percent 1.0
    }
    emit (pure (namedColumn demoName 0 containerStyle #[
      Demos.Linalg.sweptCollisionDemoWidget env s
    ]))
  pure ()

end Demos
