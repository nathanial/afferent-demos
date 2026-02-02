/-
  Demo Runner - Canopy app linalg ConvexHull2D tab content.
-/
import Reactive
import Afferent
import Afferent.Canopy
import Afferent.Canopy.Reactive
import Demos.Core.Demo
import Demos.Linalg.ConvexHull2D
import Trellis

open Reactive Reactive.Host
open Afferent
open Afferent.Arbor
open Afferent.Canopy
open Afferent.Canopy.Reactive
open Trellis

namespace Demos

def convexHull2DTabContent (env : DemoEnv) : WidgetM Unit := do
  let animFrame ← useAnimationFrame
  let demoName ← registerComponentW "convex-hull-2d"

  let keyEvents ← useKeyboard
  let keyUpdates ← Event.mapM (fun data =>
    fun (s : Demos.Linalg.ConvexHull2DState) =>
      if data.event.isPress then
        match data.event.key with
        | .char 'r' => Demos.Linalg.convexHull2DInitialState
        | .space => { s with animating := !s.animating }
        | .char 'h' => { s with showHull := !s.showHull }
        | .char 'g' => { s with showGiftWrap := !s.showGiftWrap }
        | .char 'f' => { s with showHullFill := !s.showHullFill }
        | .char 'x' =>
            let hit := Id.run do
              let mut idx : Option Nat := none
              for i in [:s.points.size] do
                if idx.isNone then
                  let p := s.points[i]!
                  if Demos.Linalg.nearPoint s.lastMouse p 0.4 then
                    idx := some i
              return idx
            match hit with
            | some i =>
                let newPoints := s.points.eraseIdxIfInBounds i
                { s with points := newPoints, dragging := none }
            | none => s
        | _ => s
      else s
    ) keyEvents

  let clickEvents ← useClickData demoName
  let clickUpdates ← Event.mapM (fun data =>
    if data.click.button != 0 && data.click.button != 2 then
      id
    else
      match data.nameMap.get? demoName with
      | some wid =>
          match data.layouts.get wid with
          | some layout =>
              let rect := layout.contentRect
              let localX := data.click.x - rect.x
              let localY := data.click.y - rect.y
              let config := Demos.Linalg.convexHullMathViewConfig env.screenScale
              let view := Afferent.Widget.MathView2D.viewForSize config rect.width rect.height
              let worldPos := Afferent.Widget.MathView2D.screenToWorld view (localX, localY)
              fun (state : Demos.Linalg.ConvexHull2DState) =>
                let points := state.points
                let hit := Id.run do
                  let mut idx : Option Nat := none
                  for i in [:points.size] do
                    if idx.isNone then
                      let p := points[i]!
                      if Demos.Linalg.nearPoint worldPos p 0.4 then
                        idx := some i
                  return idx
                let hitQuery := Demos.Linalg.nearPoint worldPos state.queryPoint 0.4
                if data.click.button == 2 then
                  match hit with
                  | some i =>
                      let newPoints := points.eraseIdxIfInBounds i
                      { state with points := newPoints, dragging := none }
                  | none => state
                else if hitQuery then
                  { state with draggingQuery := true, queryPoint := worldPos }
                else
                  match hit with
                  | some i =>
                      { state with dragging := some i }
                  | none =>
                      let newPoints := points.push worldPos
                      { state with points := newPoints }
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
            let config := Demos.Linalg.convexHullMathViewConfig env.screenScale
            let view := Afferent.Widget.MathView2D.viewForSize config rect.width rect.height
            let worldPos := Afferent.Widget.MathView2D.screenToWorld view (localX, localY)
            fun (state : Demos.Linalg.ConvexHull2DState) =>
              let state := { state with lastMouse := worldPos }
              match state.dragging with
              | some i =>
                  let points := state.points.set! i worldPos
                  { state with points := points }
              | none =>
                  if state.draggingQuery then
                    { state with queryPoint := worldPos }
                  else
                    state
        | none => id
    | none => id
    ) hoverEvents

  let mouseUpEvents ← useAllMouseUp
  let mouseUpUpdates ← Event.mapM (fun _ =>
    fun (s : Demos.Linalg.ConvexHull2DState) =>
      { s with dragging := none, draggingQuery := false }
    ) mouseUpEvents

  let animUpdates ← Event.mapM (fun dt =>
    fun (s : Demos.Linalg.ConvexHull2DState) =>
      if s.animating then
        { s with time := s.time + dt }
      else s
    ) animFrame

  let allUpdates ← Event.mergeAllListM [keyUpdates, clickUpdates, hoverUpdates, mouseUpUpdates, animUpdates]
  let state ← foldDyn (fun f s => f s) Demos.Linalg.convexHull2DInitialState allUpdates

  let _ ← dynWidget state fun s => do
    let containerStyle : BoxStyle := {
      flexItem := some (FlexItem.growing 1)
      width := .percent 1.0
      height := .percent 1.0
    }
    emit (pure (namedColumn demoName 0 containerStyle #[
      Demos.Linalg.convexHull2DWidget env s
    ]))
  pure ()

end Demos
