/-
  Demo Runner - Canopy app linalg CatmullRomSplineEditor tab content.
-/
import Reactive
import Afferent
import Afferent.Canopy
import Afferent.Canopy.Reactive
import Demos.Core.Demo
import Demos.Linalg.CatmullRomSplineEditor
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
def catmullRomSplineEditorTabContent (env : DemoEnv) : WidgetM Unit := do
  let elapsedTime ← useElapsedTime
  let catmullName ← registerComponentW "catmull-rom-spline-editor"

  let clickEvents ← useClickData catmullName
  let clickUpdates ← Event.mapM (fun data =>
    if data.click.button != 0 then
      id
    else
      match data.nameMap.get? catmullName with
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
                let alpha := Linalg.Float.clamp ((localX - sliderX) / sliderW) 0.0 1.0
                fun (s : Demos.Linalg.CatmullRomSplineEditorState) =>
                  { s with alpha := alpha, dragging := .slider }
              else
                let config := Demos.Linalg.catmullRomMathViewConfig env.screenScale
                let view := AfferentMath.Widget.MathView2D.viewForSize config rect.width rect.height
                let worldPos := AfferentMath.Widget.MathView2D.screenToWorld view (localX, localY)
                fun (state : Demos.Linalg.CatmullRomSplineEditorState) =>
                  let hit := (Array.range state.points.size).findSome? fun i =>
                    let p := state.points.getD i Linalg.Vec2.zero
                    if Demos.Linalg.nearPoint worldPos p 0.45 then some i else none
                  match hit with
                  | some idx => { state with dragging := .point idx }
                  | none =>
                      let newPoints := state.points.push worldPos
                      { state with points := newPoints, dragging := .point (newPoints.size - 1) }
          | none => id
      | none => id
    ) clickEvents

  let hoverEvents ← useAllHovers
  let hoverUpdates ← Event.mapM (fun data =>
    match data.nameMap.get? catmullName with
    | some wid =>
        match data.layouts.get wid with
        | some layout =>
            let rect := layout.contentRect
            let localX := data.x - rect.x
            let localY := data.y - rect.y
            fun (state : Demos.Linalg.CatmullRomSplineEditorState) =>
              match state.dragging with
              | .none => state
              | .slider =>
                  let sliderX := rect.width - 260.0 * env.screenScale
                  let sliderW := 190.0 * env.screenScale
                  let alpha := Linalg.Float.clamp ((localX - sliderX) / sliderW) 0.0 1.0
                  { state with alpha := alpha }
              | .point idx =>
                  let config := Demos.Linalg.catmullRomMathViewConfig env.screenScale
                  let view := AfferentMath.Widget.MathView2D.viewForSize config rect.width rect.height
                  let worldPos := AfferentMath.Widget.MathView2D.screenToWorld view (localX, localY)
                  if idx < state.points.size then
                    { state with points := state.points.set! idx worldPos }
                  else
                    state
        | none => id
    | none => id
    ) hoverEvents

  let mouseUpEvents ← useAllMouseUp
  let mouseUpUpdates ← Event.mapM (fun _ =>
    fun (s : Demos.Linalg.CatmullRomSplineEditorState) => { s with dragging := .none }
    ) mouseUpEvents

  let keyEvents ← useKeyboard
  let keyUpdates ← Event.mapM (fun data =>
    fun (s : Demos.Linalg.CatmullRomSplineEditorState) =>
      if data.event.isPress then
        match data.event.key with
        | .char 'r' => Demos.Linalg.catmullRomSplineEditorInitialState
        | .char 'c' => { s with closed := !s.closed }
        | .delete | .backspace =>
            if s.points.size > 0 then
              { s with points := s.points.pop }
            else s
        | .space => { s with animating := !s.animating }
        | _ => s
      else s
    ) keyEvents

  -- Time-based animation updates (track lastTime in state)
  let timeUpdates ← Event.mapM (fun t =>
    fun (state : Demos.Linalg.CatmullRomSplineEditorState) =>
      let dt := if state.lastTime == 0.0 then 0.0 else max 0.0 (t - state.lastTime)
      if state.animating then
        let newT := state.t + dt * 0.2
        { state with t := if newT > 1.0 then newT - 1.0 else newT, lastTime := t }
      else
        { state with lastTime := t }
    ) elapsedTime.updated

  let allUpdates ← Event.mergeAllListM [clickUpdates, hoverUpdates, mouseUpUpdates, keyUpdates, timeUpdates]
  let state ← foldDyn (fun f s => f s) Demos.Linalg.catmullRomSplineEditorInitialState allUpdates

  let _ ← dynWidget state fun s => do
    let containerStyle : BoxStyle := {
      flexItem := some (FlexItem.growing 1)
      width := .percent 1.0
      height := .percent 1.0
    }
    emit (pure (namedColumn catmullName 0 containerStyle #[
      Demos.Linalg.catmullRomSplineEditorWidget env s
    ]))
  pure ()

end Demos
