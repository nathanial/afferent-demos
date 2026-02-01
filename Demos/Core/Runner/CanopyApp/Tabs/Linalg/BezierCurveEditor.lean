/-
  Demo Runner - Canopy app linalg BezierCurveEditor tab content.
-/
import Reactive
import Afferent
import Afferent.Canopy
import Afferent.Canopy.Reactive
import Demos.Core.Demo
import Demos.Linalg.BezierCurveEditor
import Trellis

open Reactive Reactive.Host
open Afferent
open Afferent.Arbor
open Afferent.Canopy
open Afferent.Canopy.Reactive
open Trellis

namespace Demos
def bezierCurveEditorTabContent (env : DemoEnv) (elapsedTime : Dynamic Spider Float)
    (stateRef : IO.Ref Demos.Linalg.BezierCurveEditorState) : WidgetM Unit := do
  let bezierName ← registerComponentW "bezier-curve-editor"

  let clickEvents ← useClickData bezierName
  let clickAction ← Event.mapM (fun data => do
    if data.click.button != 0 then
      pure ()
    else
      match data.nameMap.get? bezierName with
      | some wid =>
          match data.layouts.get wid with
          | some layout =>
              let rect := layout.contentRect
              let localX := data.click.x - rect.x
              let localY := data.click.y - rect.y
              let sliderX := rect.width - 250.0 * env.screenScale
              let sliderY := 95.0 * env.screenScale
              let sliderW := 180.0 * env.screenScale
              let sliderH := 8.0 * env.screenScale
              let hitSlider := localX >= sliderX && localX <= sliderX + sliderW
                && localY >= sliderY - 8.0 && localY <= sliderY + sliderH + 8.0
              if hitSlider then
                let t := Linalg.Float.clamp ((localX - sliderX) / sliderW) 0.0 1.0
                stateRef.modify fun s => { s with t := t, dragging := .slider }
              else
                let origin := (rect.width / 2, rect.height / 2)
                let scale := 70.0 * env.screenScale
                let worldPos := Demos.Linalg.screenToWorld (localX, localY) origin scale
                let state ← stateRef.get
                let points := match state.mode with
                  | .quadratic => state.quadPoints
                  | .cubic => state.cubicPoints
                let mut hit : Option Nat := none
                for i in [:points.size] do
                  let p := points.getD i Linalg.Vec2.zero
                  if Demos.Linalg.nearPoint worldPos p 0.45 then
                    hit := some i
                match hit with
                | some idx => stateRef.set { state with dragging := .control idx }
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
        match data.nameMap.get? bezierName with
        | some wid =>
            match data.layouts.get wid with
            | some layout =>
                let rect := layout.contentRect
                let localX := data.x - rect.x
                let sliderX := rect.width - 250.0 * env.screenScale
                let sliderW := 180.0 * env.screenScale
                let t := Linalg.Float.clamp ((localX - sliderX) / sliderW) 0.0 1.0
                stateRef.set { state with t := t }
            | none => pure ()
        | none => pure ()
    | .control idx =>
        match data.nameMap.get? bezierName with
        | some wid =>
            match data.layouts.get wid with
            | some layout =>
                let rect := layout.contentRect
                let localX := data.x - rect.x
                let localY := data.y - rect.y
                let origin := (rect.width / 2, rect.height / 2)
                let scale := 70.0 * env.screenScale
                let worldPos := Demos.Linalg.screenToWorld (localX, localY) origin scale
                let next := match state.mode with
                  | .quadratic =>
                      if idx < state.quadPoints.size then
                        { state with quadPoints := state.quadPoints.set! idx worldPos }
                      else state
                  | .cubic =>
                      if idx < state.cubicPoints.size then
                        { state with cubicPoints := state.cubicPoints.set! idx worldPos }
                      else state
                stateRef.set next
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
          stateRef.set Demos.Linalg.bezierCurveEditorInitialState
      | .char 'q' =>
          stateRef.modify fun s => { s with mode := .quadratic }
      | .char 'c' =>
          stateRef.modify fun s => { s with mode := .cubic }
      | _ => pure ()
    ) keyEvents
  performEvent_ keyAction

  let _ ← dynWidget elapsedTime fun _ => do
    let state ← SpiderM.liftIO stateRef.get
    let containerStyle : BoxStyle := {
      flexItem := some (FlexItem.growing 1)
      width := .percent 1.0
      height := .percent 1.0
    }
    emit (pure (namedColumn bezierName 0 containerStyle #[
      Demos.Linalg.bezierCurveEditorWidget env state
    ]))
  pure ()

end Demos
