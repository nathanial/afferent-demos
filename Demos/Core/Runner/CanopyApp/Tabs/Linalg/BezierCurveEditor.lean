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
import AfferentMath.Widget.MathView2D

open Reactive Reactive.Host
open Afferent
open Afferent.Arbor
open Afferent.Canopy
open Afferent.Canopy.Reactive
open Trellis
open AfferentMath.Widget

namespace Demos
def bezierCurveEditorTabContent (env : DemoEnv) : WidgetM Unit := do
  let elapsedTime ← useElapsedTime
  let bezierName ← registerComponentW "bezier-curve-editor"

  let clickEvents ← useClickData bezierName
  let clickUpdates ← Event.mapM (fun data =>
    if data.click.button != 0 then
      id
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
                fun (s : Demos.Linalg.BezierCurveEditorState) =>
                  { s with t := t, dragging := .slider }
              else
                let config := Demos.Linalg.bezierCurveMathViewConfig env.screenScale
                let view := AfferentMath.Widget.MathView2D.viewForSize config rect.width rect.height
                let worldPos := AfferentMath.Widget.MathView2D.screenToWorld view (localX, localY)
                fun (state : Demos.Linalg.BezierCurveEditorState) =>
                  let points := match state.mode with
                    | .quadratic => state.quadPoints
                    | .cubic => state.cubicPoints
                  let hit := (Array.range points.size).findSome? fun i =>
                    let p := points.getD i Linalg.Vec2.zero
                    if Demos.Linalg.nearPoint worldPos p 0.45 then some i else none
                  match hit with
                  | some idx => { state with dragging := .control idx }
                  | none => state
          | none => id
      | none => id
    ) clickEvents

  let hoverEvents ← useAllHovers
  let hoverUpdates ← Event.mapM (fun data =>
    match data.nameMap.get? bezierName with
    | some wid =>
        match data.layouts.get wid with
        | some layout =>
            let rect := layout.contentRect
            let localX := data.x - rect.x
            let localY := data.y - rect.y
            fun (state : Demos.Linalg.BezierCurveEditorState) =>
              match state.dragging with
              | .none => state
              | .slider =>
                  let sliderX := rect.width - 250.0 * env.screenScale
                  let sliderW := 180.0 * env.screenScale
                  let t := Linalg.Float.clamp ((localX - sliderX) / sliderW) 0.0 1.0
                  { state with t := t }
              | .control idx =>
                  let config := Demos.Linalg.bezierCurveMathViewConfig env.screenScale
                  let view := AfferentMath.Widget.MathView2D.viewForSize config rect.width rect.height
                  let worldPos := AfferentMath.Widget.MathView2D.screenToWorld view (localX, localY)
                  match state.mode with
                  | .quadratic =>
                      if idx < state.quadPoints.size then
                        { state with quadPoints := state.quadPoints.set! idx worldPos }
                      else state
                  | .cubic =>
                      if idx < state.cubicPoints.size then
                        { state with cubicPoints := state.cubicPoints.set! idx worldPos }
                      else state
        | none => id
    | none => id
    ) hoverEvents

  let mouseUpEvents ← useAllMouseUp
  let mouseUpUpdates ← Event.mapM (fun _ =>
    fun (s : Demos.Linalg.BezierCurveEditorState) => { s with dragging := .none }
    ) mouseUpEvents

  let keyEvents ← useKeyboard
  let keyUpdates ← Event.mapM (fun data =>
    fun (s : Demos.Linalg.BezierCurveEditorState) =>
      if data.event.isPress then
        match data.event.key with
        | .char 'r' => Demos.Linalg.bezierCurveEditorInitialState
        | .char 'q' => { s with mode := .quadratic }
        | .char 'c' => { s with mode := .cubic }
        | _ => s
      else s
    ) keyEvents

  let allUpdates ← Event.mergeAllListM [clickUpdates, hoverUpdates, mouseUpUpdates, keyUpdates]
  let state ← foldDyn (fun f s => f s) Demos.Linalg.bezierCurveEditorInitialState allUpdates

  let _ ← dynWidget state fun s => do
    let containerStyle : BoxStyle := {
      flexItem := some (FlexItem.growing 1)
      width := .percent 1.0
      height := .percent 1.0
    }
    emit (pure (namedColumn bezierName 0 containerStyle #[
      Demos.Linalg.bezierCurveEditorWidget env s
    ]))
  pure ()

end Demos
