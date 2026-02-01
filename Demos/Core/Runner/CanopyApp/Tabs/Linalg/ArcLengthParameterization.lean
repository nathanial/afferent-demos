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

open Reactive Reactive.Host
open Afferent
open Afferent.Arbor
open Afferent.Canopy
open Afferent.Canopy.Reactive
open Trellis

namespace Demos
def arcLengthParameterizationTabContent (env : DemoEnv) (elapsedTime : Dynamic Spider Float)
    (stateRef : IO.Ref Demos.Linalg.ArcLengthParameterizationState)
    (lastTimeRef : IO.Ref Float) : WidgetM Unit := do
  let arcName ← registerComponentW "arc-length-parameterization"

  let clickEvents ← useClickData arcName
  let clickAction ← Event.mapM (fun data => do
    if data.click.button != 0 then
      pure ()
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
                stateRef.modify fun s => { s with speed := speed, dragging := .slider }
              else
                let origin := (rect.width / 2, rect.height / 2)
                let scale := 70.0 * env.screenScale
                let worldPos := Demos.Linalg.screenToWorld (localX, localY) origin scale
                let state ← stateRef.get
                let mut hit : Option Nat := none
                for i in [:state.controlPoints.size] do
                  let p := state.controlPoints.getD i Linalg.Vec2.zero
                  if Demos.Linalg.nearPoint worldPos p 0.45 then
                    hit := some i
                match hit with
                | some idx => stateRef.set { state with dragging := .point idx }
                | none => pure ()
          | none => pure ()
      | none => pure ()
    ) clickEvents
  performEvent_ clickAction

  let hoverEvents ← useAllHovers
  let hoverAction ← Event.mapM (fun data => do
    let state ← stateRef.get
    match state.dragging with
    | .none => pure ()
    | .slider =>
        match data.nameMap.get? arcName with
        | some wid =>
            match data.layouts.get wid with
            | some layout =>
                let rect := layout.contentRect
                let localX := data.x - rect.x
                let sliderX := rect.width - 260.0 * env.screenScale
                let sliderW := 190.0 * env.screenScale
                let t := Linalg.Float.clamp ((localX - sliderX) / sliderW) 0.0 1.0
                let speed := 0.2 + t * 3.8
                stateRef.set { state with speed := speed }
            | none => pure ()
        | none => pure ()
    | .point idx =>
        match data.nameMap.get? arcName with
        | some wid =>
            match data.layouts.get wid with
            | some layout =>
                let rect := layout.contentRect
                let localX := data.x - rect.x
                let localY := data.y - rect.y
                let origin := (rect.width / 2, rect.height / 2)
                let scale := 70.0 * env.screenScale
                let worldPos := Demos.Linalg.screenToWorld (localX, localY) origin scale
                if idx < state.controlPoints.size then
                  stateRef.set { state with controlPoints := state.controlPoints.set! idx worldPos }
                else
                  pure ()
            | none => pure ()
        | none => pure ()
    ) hoverEvents
  performEvent_ hoverAction

  let mouseUpEvents ← useAllMouseUp
  let mouseUpAction ← Event.mapM (fun _ => do
    stateRef.modify fun s => { s with dragging := .none }
    ) mouseUpEvents
  performEvent_ mouseUpAction

  let keyEvents ← useKeyboard
  let keyAction ← Event.mapM (fun data => do
    if data.event.isPress then
      match data.event.key with
      | .char 'r' =>
          stateRef.set Demos.Linalg.arcLengthParameterizationInitialState
      | .space =>
          stateRef.modify fun s => { s with animating := !s.animating }
      | _ => pure ()
    ) keyEvents
  performEvent_ keyAction

  let _ ← dynWidget elapsedTime fun t => do
    let state ← SpiderM.liftIO do
      let lastT ← lastTimeRef.get
      let dt := if lastT == 0.0 then 0.0 else max 0.0 (t - lastT)
      let mut current ← stateRef.get
      if current.animating then
        let p0 := current.controlPoints.getD 0 Linalg.Vec2.zero
        let p1 := current.controlPoints.getD 1 Linalg.Vec2.zero
        let p2 := current.controlPoints.getD 2 Linalg.Vec2.zero
        let p3 := current.controlPoints.getD 3 Linalg.Vec2.zero
        let curve := Linalg.Bezier3.mk p0 p1 p2 p3
        let evalFn := fun t => Linalg.Bezier3.evalVec2 curve t
        let table := Linalg.ArcLengthTable.build evalFn 120
        let newT := current.t + dt * 0.2
        let newS := current.s + current.speed * dt
        let wrappedS := if table.totalLength > Linalg.Float.epsilon then
          if newS > table.totalLength then newS - table.totalLength else newS
        else 0.0
        current := { current with t := (if newT > 1.0 then newT - 1.0 else newT), s := wrappedS }
        stateRef.set current
      lastTimeRef.set t
      pure current
    let containerStyle : BoxStyle := {
      flexItem := some (FlexItem.growing 1)
      width := .percent 1.0
      height := .percent 1.0
    }
    emit (pure (namedColumn arcName 0 containerStyle #[
      Demos.Linalg.arcLengthParameterizationWidget env state
    ]))
  pure ()

end Demos
