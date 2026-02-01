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
  let stateRef ← SpiderM.liftIO (IO.mkRef Demos.Linalg.smoothDampFollowerInitialState)
  let lastTimeRef ← SpiderM.liftIO (IO.mkRef 0.0)
  let smoothName ← registerComponentW "smooth-damp-follower"

  let clickEvents ← useClickData smoothName
  let clickAction ← Event.mapM (fun data => do
    if data.click.button != 0 then
      pure ()
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
              if hitSmooth then
                let t := Linalg.Float.clamp ((localX - layoutSmooth.x) / layoutSmooth.width) 0.0 1.0
                let value := Demos.Linalg.smoothDampSmoothTimeFrom t
                stateRef.modify fun s => { s with smoothTime := value, dragging := .slider .smoothTime }
              else if hitMax then
                let t := Linalg.Float.clamp ((localX - layoutMax.x) / layoutMax.width) 0.0 1.0
                let value := Demos.Linalg.smoothDampMaxSpeedFrom t
                stateRef.modify fun s => { s with maxSpeed := value, dragging := .slider .maxSpeed }
              else
                let origin := (rect.width / 2, rect.height / 2)
                let scale := 70.0 * env.screenScale
                let worldPos := Demos.Linalg.screenToWorld (localX, localY) origin scale
                let state ← stateRef.get
                if Demos.Linalg.nearPoint worldPos state.target 0.45 then
                  stateRef.set { state with dragging := .target }
                else
                  stateRef.set { state with target := worldPos, dragging := .target }
          | none => pure ()
      | none => pure ()
    ) clickEvents
  performEvent_ clickAction

  let hoverEvents ← useAllHovers
  let hoverAction ← Event.mapM (fun data => do
    let state ← stateRef.get
    match state.dragging with
    | .none => pure ()
    | .target =>
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
                stateRef.set { state with target := worldPos }
            | none => pure ()
        | none => pure ()
    | .slider which =>
        let localX := data.x
        match data.nameMap.get? smoothName with
        | some wid =>
            match data.layouts.get wid with
            | some layout =>
                let rect := layout.contentRect
                let lx := localX - rect.x
                match which with
                | .smoothTime =>
                    let layout := Demos.Linalg.smoothDampSliderLayout rect.width rect.height env.screenScale 0
                    let t := Linalg.Float.clamp ((lx - layout.x) / layout.width) 0.0 1.0
                    stateRef.set { state with smoothTime := Demos.Linalg.smoothDampSmoothTimeFrom t }
                | .maxSpeed =>
                    let layout := Demos.Linalg.smoothDampSliderLayout rect.width rect.height env.screenScale 1
                    let t := Linalg.Float.clamp ((lx - layout.x) / layout.width) 0.0 1.0
                    stateRef.set { state with maxSpeed := Demos.Linalg.smoothDampMaxSpeedFrom t }
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
          stateRef.set Demos.Linalg.smoothDampFollowerInitialState
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
        let (_newPos, newState) := Linalg.SmoothDampState2.step
          current.dampState current.target current.smoothTime dt current.maxSpeed
        let speed := newState.velocity.length
        let mut history := current.history.push speed
        if history.size > 120 then
          history := history.eraseIdxIfInBounds 0
        current := { current with dampState := newState, history := history }
        stateRef.set current
      lastTimeRef.set t
      pure current
    let containerStyle : BoxStyle := {
      flexItem := some (FlexItem.growing 1)
      width := .percent 1.0
      height := .percent 1.0
    }
    emit (pure (namedColumn smoothName 0 containerStyle #[
      Demos.Linalg.smoothDampFollowerWidget env state
    ]))
  pure ()

end Demos
