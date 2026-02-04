/-
  Demo Runner - Canopy app linalg ConstraintSolver tab content.
-/
import Reactive
import Afferent
import Afferent.Canopy
import Afferent.Canopy.Reactive
import Demos.Core.Demo
import Demos.Linalg.ConstraintSolver
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

private def pickPoint (state : Demos.Linalg.ConstraintSolverState)
    (pos : Linalg.Vec2) : Option Nat :=
  (Array.range state.positions.size).findSome? fun i =>
    let p := state.positions.getD i Linalg.Vec2.zero
    if Demos.Linalg.nearPoint pos p 0.45 then some i else none

def constraintSolverTabContent (env : DemoEnv) : WidgetM Unit := do
  let animFrame ← useAnimationFrame
  let demoName ← registerComponentW "constraint-solver"

  let keyEvents ← useKeyboard
  let keyUpdates ← Event.mapM (fun data =>
    fun (s : Demos.Linalg.ConstraintSolverState) =>
      if data.event.isPress then
        match data.event.key with
        | .char 'r' => Demos.Linalg.constraintSolverInitialState
        | .space => { s with animating := !s.animating }
        | .char '-' =>
            let iter := if s.iterations > 1 then s.iterations - 1 else s.iterations
            { s with iterations := iter }
        | .char '+' =>
            let iter := if s.iterations < 12 then s.iterations + 1 else s.iterations
            { s with iterations := iter }
        | .char '=' =>
            let iter := if s.iterations < 12 then s.iterations + 1 else s.iterations
            { s with iterations := iter }
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
              let config := Demos.Linalg.constraintSolverMathViewConfig env.screenScale
              let view := AfferentMath.Widget.MathView2D.viewForSize config rect.width rect.height
              let world := AfferentMath.Widget.MathView2D.screenToWorld view (localX, localY)
              fun (state : Demos.Linalg.ConstraintSolverState) =>
                match pickPoint state world with
                | some idx => { state with dragging := some idx }
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
            let config := Demos.Linalg.constraintSolverMathViewConfig env.screenScale
            let view := AfferentMath.Widget.MathView2D.viewForSize config rect.width rect.height
            let world := AfferentMath.Widget.MathView2D.screenToWorld view (localX, localY)
            fun (state : Demos.Linalg.ConstraintSolverState) =>
              match state.dragging with
              | some idx =>
                  if idx < state.positions.size then
                    { state with
                      positions := state.positions.set! idx world
                      velocities := state.velocities.set! idx Linalg.Vec2.zero }
                  else
                    state
              | none => state
        | none => id
    | none => id
    ) hoverEvents

  let mouseUpEvents ← useAllMouseUp
  let mouseUpUpdates ← Event.mapM (fun _ =>
    fun (s : Demos.Linalg.ConstraintSolverState) => { s with dragging := none }
    ) mouseUpEvents

  let animUpdates ← Event.mapM (fun dt =>
    fun (s : Demos.Linalg.ConstraintSolverState) =>
      Demos.Linalg.stepConstraintSolver s dt
    ) animFrame

  let allUpdates ← Event.mergeAllListM [keyUpdates, clickUpdates, hoverUpdates, mouseUpUpdates, animUpdates]
  let state ← foldDyn (fun f s => f s) Demos.Linalg.constraintSolverInitialState allUpdates

  let _ ← dynWidget state fun s => do
    let containerStyle : BoxStyle := {
      flexItem := some (FlexItem.growing 1)
      width := .percent 1.0
      height := .percent 1.0
    }
    emit (pure (namedColumn demoName 0 containerStyle #[
      Demos.Linalg.constraintSolverWidget env s
    ]))
  pure ()

end Demos
