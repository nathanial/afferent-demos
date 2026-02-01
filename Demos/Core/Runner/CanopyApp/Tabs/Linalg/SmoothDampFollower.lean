/-
  Demo Runner - Canopy app linalg SmoothDampFollower tab content.
-/
import Reactive
import Afferent
import Afferent.Canopy
import Afferent.Canopy.Reactive
import Demos.Core.Demo
import Demos.Linalg.SmoothDampFollower
import Trellis

open Reactive Reactive.Host
open Afferent
open Afferent.Arbor
open Afferent.Canopy
open Afferent.Canopy.Reactive
open Trellis

namespace Demos
def smoothDampFollowerTabContent (env : DemoEnv) : WidgetM Unit := do
  let elapsedTime ← useElapsedTime
  let lastTimeRef ← SpiderM.liftIO (IO.mkRef 0.0)
  let smoothName ← registerComponentW "smooth-damp-follower"

  let clickEvents ← useClickData smoothName
  let clickUpdates ← Event.mapM (fun data =>
    if data.click.button != 0 then
      id
    else
      match data.nameMap.get? smoothName with
      | some wid =>
          match data.layouts.get wid with
          | some layout =>
              let rect := layout.contentRect
              let localX := data.click.x - rect.x
              let localY := data.click.y - rect.y
              let layoutSmooth := Demos.Linalg.smoothDampSliderLayout rect.width rect.height env.screenScale 0
              let layoutMax := Demos.Linalg.smoothDampSliderLayout rect.width rect.height env.screenScale 1
              let hitSmooth := localX >= layoutSmooth.x && localX <= layoutSmooth.x + layoutSmooth.width
                && localY >= layoutSmooth.y - 8.0 && localY <= layoutSmooth.y + layoutSmooth.height + 8.0
              let hitMax := localX >= layoutMax.x && localX <= layoutMax.x + layoutMax.width
                && localY >= layoutMax.y - 8.0 && localY <= layoutMax.y + layoutMax.height + 8.0
              let origin := (rect.width / 2, rect.height / 2)
              let scale := 70.0 * env.screenScale
              let worldPos := Demos.Linalg.screenToWorld (localX, localY) origin scale
              fun (state : Demos.Linalg.SmoothDampFollowerState) =>
                if hitSmooth then
                  let t := Linalg.Float.clamp ((localX - layoutSmooth.x) / layoutSmooth.width) 0.0 1.0
                  let value := Demos.Linalg.smoothDampSmoothTimeFrom t
                  { state with smoothTime := value, dragging := .slider .smoothTime }
                else if hitMax then
                  let t := Linalg.Float.clamp ((localX - layoutMax.x) / layoutMax.width) 0.0 1.0
                  let value := Demos.Linalg.smoothDampMaxSpeedFrom t
                  { state with maxSpeed := value, dragging := .slider .maxSpeed }
                else if Demos.Linalg.nearPoint worldPos state.target 0.45 then
                  { state with dragging := .target }
                else
                  { state with target := worldPos, dragging := .target }
          | none => id
      | none => id
    ) clickEvents

  let hoverEvents ← useAllHovers
  let hoverUpdates ← Event.mapM (fun data =>
    match data.nameMap.get? smoothName with
    | some wid =>
        match data.layouts.get wid with
        | some layout =>
            let rect := layout.contentRect
            let localX := data.x - rect.x
            let localY := data.y - rect.y
            let origin := (rect.width / 2, rect.height / 2)
            let scale := 70.0 * env.screenScale
            let worldPos := Demos.Linalg.screenToWorld (localX, localY) origin scale
            fun (state : Demos.Linalg.SmoothDampFollowerState) =>
              match state.dragging with
              | .none => state
              | .target => { state with target := worldPos }
              | .slider which =>
                  let lx := data.x - rect.x
                  match which with
                  | .smoothTime =>
                      let layout := Demos.Linalg.smoothDampSliderLayout rect.width rect.height env.screenScale 0
                      let t := Linalg.Float.clamp ((lx - layout.x) / layout.width) 0.0 1.0
                      { state with smoothTime := Demos.Linalg.smoothDampSmoothTimeFrom t }
                  | .maxSpeed =>
                      let layout := Demos.Linalg.smoothDampSliderLayout rect.width rect.height env.screenScale 1
                      let t := Linalg.Float.clamp ((lx - layout.x) / layout.width) 0.0 1.0
                      { state with maxSpeed := Demos.Linalg.smoothDampMaxSpeedFrom t }
        | none => id
    | none => id
    ) hoverEvents

  let mouseUpEvents ← useAllMouseUp
  let mouseUpUpdates ← Event.mapM (fun _ =>
    fun (s : Demos.Linalg.SmoothDampFollowerState) => { s with dragging := .none }
    ) mouseUpEvents

  let keyEvents ← useKeyboard
  let keyUpdates ← Event.mapM (fun data =>
    fun (s : Demos.Linalg.SmoothDampFollowerState) =>
      if data.event.isPress then
        match data.event.key with
        | .char 'r' => Demos.Linalg.smoothDampFollowerInitialState
        | .space => { s with animating := !s.animating }
        | _ => s
      else s
    ) keyEvents

  let allUpdates ← Event.mergeAllListM [clickUpdates, hoverUpdates, mouseUpUpdates, keyUpdates]
  let state ← foldDyn (fun f s => f s) Demos.Linalg.smoothDampFollowerInitialState allUpdates

  let _ ← dynWidget elapsedTime fun t => do
    let currentState ← SpiderM.liftIO do
      let lastT ← lastTimeRef.get
      let dt := if lastT == 0.0 then 0.0 else max 0.0 (t - lastT)
      lastTimeRef.set t
      pure dt
    let dt := currentState
    let s ← SpiderM.liftIO state.sample
    let finalState :=
      if s.animating then
        let (_newPos, newState) := Linalg.SmoothDampState2.step
          s.dampState s.target s.smoothTime dt s.maxSpeed
        let speed := newState.velocity.length
        let history := s.history.push speed
        let history := if history.size > 120 then history.eraseIdxIfInBounds 0 else history
        { s with dampState := newState, history := history }
      else s
    let containerStyle : BoxStyle := {
      flexItem := some (FlexItem.growing 1)
      width := .percent 1.0
      height := .percent 1.0
    }
    emit (pure (namedColumn smoothName 0 containerStyle #[
      Demos.Linalg.smoothDampFollowerWidget env finalState
    ]))
  pure ()

end Demos
