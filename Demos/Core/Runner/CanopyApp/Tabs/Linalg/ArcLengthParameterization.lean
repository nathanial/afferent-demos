/-
  Demo Runner - Canopy app linalg ArcLengthParameterization tab content.
-/
import Reactive
import Afferent
import Afferent.Canopy
import Afferent.Canopy.Reactive
import Demos.Core.Demo
import Demos.Linalg.ArcLengthParameterization
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
def arcLengthParameterizationTabContent (env : DemoEnv) : WidgetM Unit := do
  let elapsedTime ← useElapsedTime
  let arcName ← registerComponentW "arc-length-parameterization"

  let clickEvents ← useClickData arcName
  let clickUpdates ← Event.mapM (fun data =>
    if data.click.button != 0 then
      id
    else
      match data.nameMap.get? arcName with
      | some wid =>
          match data.layouts.get wid with
          | some layout =>
              let rect := layout.contentRect
              let localX := data.click.x - rect.x
              let localY := data.click.y - rect.y
              let sliderX := rect.width - 260.0 * env.screenScale
              let sliderY := 95.0 * env.screenScale
              let sliderW := 190.0 * env.screenScale
              let sliderH := 8.0 * env.screenScale
              let hitSlider := localX >= sliderX && localX <= sliderX + sliderW
                && localY >= sliderY - 8.0 && localY <= sliderY + sliderH + 8.0
              if hitSlider then
                let t := Linalg.Float.clamp ((localX - sliderX) / sliderW) 0.0 1.0
                let speed := 0.2 + t * 3.8
                fun (s : Demos.Linalg.ArcLengthParameterizationState) =>
                  { s with speed := speed, dragging := .slider }
              else
                let config := Demos.Linalg.arcLengthMathViewConfig env.screenScale
                let view := AfferentMath.Widget.MathView2D.viewForSize config rect.width rect.height
                let worldPos := AfferentMath.Widget.MathView2D.screenToWorld view (localX, localY)
                fun (state : Demos.Linalg.ArcLengthParameterizationState) =>
                  let hit := (Array.range state.controlPoints.size).findSome? fun i =>
                    let p := state.controlPoints.getD i Linalg.Vec2.zero
                    if Demos.Linalg.nearPoint worldPos p 0.45 then some i else none
                  match hit with
                  | some idx => { state with dragging := .point idx }
                  | none => state
          | none => id
      | none => id
    ) clickEvents

  let hoverEvents ← useAllHovers
  let hoverUpdates ← Event.mapM (fun data =>
    match data.nameMap.get? arcName with
    | some wid =>
        match data.layouts.get wid with
        | some layout =>
            let rect := layout.contentRect
            let localX := data.x - rect.x
            let localY := data.y - rect.y
            fun (state : Demos.Linalg.ArcLengthParameterizationState) =>
              match state.dragging with
              | .none => state
              | .slider =>
                  let sliderX := rect.width - 260.0 * env.screenScale
                  let sliderW := 190.0 * env.screenScale
                  let t := Linalg.Float.clamp ((localX - sliderX) / sliderW) 0.0 1.0
                  let speed := 0.2 + t * 3.8
                  { state with speed := speed }
              | .point idx =>
                  let config := Demos.Linalg.arcLengthMathViewConfig env.screenScale
                  let view := AfferentMath.Widget.MathView2D.viewForSize config rect.width rect.height
                  let worldPos := AfferentMath.Widget.MathView2D.screenToWorld view (localX, localY)
                  if idx < state.controlPoints.size then
                    { state with controlPoints := state.controlPoints.set! idx worldPos }
                  else
                    state
        | none => id
    | none => id
    ) hoverEvents

  let mouseUpEvents ← useAllMouseUp
  let mouseUpUpdates ← Event.mapM (fun _ =>
    fun (s : Demos.Linalg.ArcLengthParameterizationState) => { s with dragging := .none }
    ) mouseUpEvents

  let keyEvents ← useKeyboard
  let keyUpdates ← Event.mapM (fun data =>
    fun (s : Demos.Linalg.ArcLengthParameterizationState) =>
      if data.event.isPress then
        match data.event.key with
        | .char 'r' => Demos.Linalg.arcLengthParameterizationInitialState
        | .space => { s with animating := !s.animating }
        | _ => s
      else s
    ) keyEvents

  -- Time-based animation updates (track lastTime in state)
  let timeUpdates ← Event.mapM (fun t =>
    fun (state : Demos.Linalg.ArcLengthParameterizationState) =>
      let dt := if state.lastTime == 0.0 then 0.0 else max 0.0 (t - state.lastTime)
      if state.animating then
        let p0 := state.controlPoints.getD 0 Linalg.Vec2.zero
        let p1 := state.controlPoints.getD 1 Linalg.Vec2.zero
        let p2 := state.controlPoints.getD 2 Linalg.Vec2.zero
        let p3 := state.controlPoints.getD 3 Linalg.Vec2.zero
        let curve := Linalg.Bezier3.mk p0 p1 p2 p3
        let evalFn := fun t => Linalg.Bezier3.evalVec2 curve t
        let table := Linalg.ArcLengthTable.build evalFn 120
        let newT := state.t + dt * 0.2
        let newS := state.s + state.speed * dt
        let wrappedS := if table.totalLength > Linalg.Float.epsilon then
          if newS > table.totalLength then newS - table.totalLength else newS
        else 0.0
        { state with t := (if newT > 1.0 then newT - 1.0 else newT), s := wrappedS, lastTime := t }
      else
        { state with lastTime := t }
    ) elapsedTime.updated

  let allUpdates ← Event.mergeAllListM [clickUpdates, hoverUpdates, mouseUpUpdates, keyUpdates, timeUpdates]
  let state ← foldDyn (fun f s => f s) Demos.Linalg.arcLengthParameterizationInitialState allUpdates

  let _ ← dynWidget state fun s => do
    let containerStyle : BoxStyle := {
      flexItem := some (FlexItem.growing 1)
      width := .percent 1.0
      height := .percent 1.0
    }
    emit (pure (namedColumn arcName 0 containerStyle #[
      Demos.Linalg.arcLengthParameterizationWidget env s
    ]))
  pure ()

end Demos
