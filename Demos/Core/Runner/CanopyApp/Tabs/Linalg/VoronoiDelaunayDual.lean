/-
  Demo Runner - Canopy app linalg VoronoiDelaunayDual tab content.
-/
import Reactive
import Afferent
import Afferent.Canopy
import Afferent.Canopy.Reactive
import Demos.Core.Demo
import Demos.Linalg.VoronoiDelaunayDual
import Trellis

open Reactive Reactive.Host
open Afferent
open Afferent.Arbor
open Afferent.Canopy
open Afferent.Canopy.Reactive
open Trellis

namespace Demos

def voronoiDelaunayDualTabContent (env : DemoEnv) : WidgetM Unit := do
  let animFrame ← useAnimationFrame
  let demoName ← registerComponentW "voronoi-delaunay-dual"

  let keyEvents ← useKeyboard
  let keyUpdates ← Event.mapM (fun data =>
    fun (s : Demos.Linalg.VoronoiDelaunayDualState) =>
      if data.event.isPress then
        match data.event.key with
        | .char 'r' => Demos.Linalg.voronoiDelaunayDualInitialState
        | .space => { s with animating := !s.animating }
        | .char 'd' => { s with showDelaunay := !s.showDelaunay }
        | .char 'v' => { s with showVoronoi := !s.showVoronoi }
        | .char 'u' => { s with showDual := !s.showDual }
        | .char 'c' => { s with showCircumcircle := !s.showCircumcircle }
        | .char 't' => { s with selectedTriangle := s.selectedTriangle + 1 }
        | .char 'n' =>
            if s.points.size == 0 then s
            else { s with selectedSite := (s.selectedSite + 1) % s.points.size }
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
              let origin := (rect.width / 2, rect.height / 2)
              let scale := 70.0 * env.screenScale
              let worldPos := Demos.Linalg.screenToWorld (localX, localY) origin scale
              fun (state : Demos.Linalg.VoronoiDelaunayDualState) =>
                let points := state.points
                let hit := Id.run do
                  let mut idx : Option Nat := none
                  for i in [:points.size] do
                    if idx.isNone then
                      let p := points[i]!
                      if Demos.Linalg.nearPoint worldPos p 0.4 then
                        idx := some i
                  return idx
                if data.click.button == 2 then
                  match hit with
                  | some i =>
                      let newPoints := points.eraseIdxIfInBounds i
                      let newSelected :=
                        if newPoints.size == 0 then 0
                        else if state.selectedSite >= newPoints.size then
                          newPoints.size - 1
                        else
                          state.selectedSite
                      { state with
                        points := newPoints
                        dragging := none
                        selectedSite := newSelected
                        activeCount := Nat.min state.activeCount newPoints.size }
                  | none => state
                else
                  match hit with
                  | some i =>
                      { state with dragging := some i, selectedSite := i }
                  | none =>
                      let newPoints := points.push worldPos
                      let newIndex := newPoints.size - 1
                      { state with
                        points := newPoints
                        dragging := some newIndex
                        selectedSite := newIndex
                        activeCount := Nat.min (state.activeCount + 1) newPoints.size }
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
            let origin := (rect.width / 2, rect.height / 2)
            let scale := 70.0 * env.screenScale
            let worldPos := Demos.Linalg.screenToWorld (localX, localY) origin scale
            fun (state : Demos.Linalg.VoronoiDelaunayDualState) =>
              match state.dragging with
              | some i =>
                  let points := state.points.set! i worldPos
                  { state with points := points }
              | none => state
        | none => id
    | none => id
    ) hoverEvents

  let mouseUpEvents ← useAllMouseUp
  let mouseUpUpdates ← Event.mapM (fun _ =>
    fun (s : Demos.Linalg.VoronoiDelaunayDualState) => { s with dragging := none }
    ) mouseUpEvents

  let animUpdates ← Event.mapM (fun dt =>
    fun (s : Demos.Linalg.VoronoiDelaunayDualState) =>
      if s.animating then
        let newTime := s.time + dt
        let newCount := Demos.Linalg.computeActiveCount s.points.size newTime s.speed
        { s with time := newTime, activeCount := newCount }
      else s
    ) animFrame

  let allUpdates ← Event.mergeAllListM [keyUpdates, clickUpdates, hoverUpdates, mouseUpUpdates, animUpdates]
  let state ← foldDyn (fun f s => f s) Demos.Linalg.voronoiDelaunayDualInitialState allUpdates

  let _ ← dynWidget state fun s => do
    let containerStyle : BoxStyle := {
      flexItem := some (FlexItem.growing 1)
      width := .percent 1.0
      height := .percent 1.0
    }
    emit (pure (namedColumn demoName 0 containerStyle #[
      Demos.Linalg.voronoiDelaunayDualWidget env s
    ]))
  pure ()

end Demos
