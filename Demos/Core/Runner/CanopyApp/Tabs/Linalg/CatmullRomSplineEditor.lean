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

open Reactive Reactive.Host
open Afferent
open Afferent.Arbor
open Afferent.Canopy
open Afferent.Canopy.Reactive
open Trellis

namespace Demos
def catmullRomSplineEditorTabContent (env : DemoEnv) (elapsedTime : Dynamic Spider Float)
    (stateRef : IO.Ref Demos.Linalg.CatmullRomSplineEditorState)
    (lastTimeRef : IO.Ref Float) : WidgetM Unit := do
  let catmullName ← registerComponentW "catmull-rom-spline-editor"

  let clickEvents ← useClickData catmullName
  let clickAction ← Event.mapM (fun data => do
    if data.click.button != 0 then
      pure ()
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
                stateRef.modify fun s => { s with alpha := alpha, dragging := .slider }
              else
                let origin := (rect.width / 2, rect.height / 2)
                let scale := 70.0 * env.screenScale
                let worldPos := Demos.Linalg.screenToWorld (localX, localY) origin scale
                let state ← stateRef.get
                let mut hit : Option Nat := none
                for i in [:state.points.size] do
                  let p := state.points.getD i Linalg.Vec2.zero
                  if Demos.Linalg.nearPoint worldPos p 0.45 then
                    hit := some i
                match hit with
                | some idx => stateRef.set { state with dragging := .point idx }
                | none =>
                    let newPoints := state.points.push worldPos
                    stateRef.set { state with points := newPoints, dragging := .point (newPoints.size - 1) }
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
        match data.nameMap.get? catmullName with
        | some wid =>
            match data.layouts.get wid with
            | some layout =>
                let rect := layout.contentRect
                let localX := data.x - rect.x
                let sliderX := rect.width - 260.0 * env.screenScale
                let sliderW := 190.0 * env.screenScale
                let alpha := Linalg.Float.clamp ((localX - sliderX) / sliderW) 0.0 1.0
                stateRef.set { state with alpha := alpha }
            | none => pure ()
        | none => pure ()
    | .point idx =>
        match data.nameMap.get? catmullName with
        | some wid =>
            match data.layouts.get wid with
            | some layout =>
                let rect := layout.contentRect
                let localX := data.x - rect.x
                let localY := data.y - rect.y
                let origin := (rect.width / 2, rect.height / 2)
                let scale := 70.0 * env.screenScale
                let worldPos := Demos.Linalg.screenToWorld (localX, localY) origin scale
                if idx < state.points.size then
                  stateRef.set { state with points := state.points.set! idx worldPos }
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
          stateRef.set Demos.Linalg.catmullRomSplineEditorInitialState
      | .char 'c' =>
          stateRef.modify fun s => { s with closed := !s.closed }
      | .delete | .backspace =>
          stateRef.modify fun s =>
            if s.points.size > 0 then
              { s with points := s.points.pop }
            else s
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
        let newT := current.t + dt * 0.2
        current := { current with t := if newT > 1.0 then newT - 1.0 else newT }
        stateRef.set current
      lastTimeRef.set t
      pure current
    let containerStyle : BoxStyle := {
      flexItem := some (FlexItem.growing 1)
      width := .percent 1.0
      height := .percent 1.0
    }
    emit (pure (namedColumn catmullName 0 containerStyle #[
      Demos.Linalg.catmullRomSplineEditorWidget env state
    ]))
  pure ()

end Demos
