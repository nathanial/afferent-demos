/-
  Demo Runner - Canopy app linalg SpringAnimationPlayground tab content.
-/
import Reactive
import Afferent
import Afferent.Canopy
import Afferent.Canopy.Reactive
import Demos.Core.Demo
import Demos.Linalg.SpringAnimationPlayground
import Trellis

open Reactive Reactive.Host
open Afferent
open Afferent.Arbor
open Afferent.Canopy
open Afferent.Canopy.Reactive
open Trellis

namespace Demos
def springAnimationPlaygroundTabContent (env : DemoEnv) : WidgetM Unit := do
  let elapsedTime ← useElapsedTime
  let stateRef ← SpiderM.liftIO (IO.mkRef Demos.Linalg.springAnimationPlaygroundInitialState)
  let lastTimeRef ← SpiderM.liftIO (IO.mkRef 0.0)
  let springName ← registerComponentW "spring-animation-playground"

  let clickEvents ← useClickData springName
  let clickAction ← Event.mapM (fun data => do
    if data.click.button != 0 then
      pure ()
    else
      match data.nameMap.get? springName with
      | some wid =>
          match data.layouts.get wid with
          | some layout =>
              let rect := layout.contentRect
              let localX := data.click.x - rect.x
              let localY := data.click.y - rect.y
              let layoutDamp := Demos.Linalg.springSliderLayout rect.width rect.height env.screenScale 0
              let layoutFreq := Demos.Linalg.springSliderLayout rect.width rect.height env.screenScale 1
              let hitDamp := localX >= layoutDamp.x && localX <= layoutDamp.x + layoutDamp.width
                && localY >= layoutDamp.y - 8.0 && localY <= layoutDamp.y + layoutDamp.height + 8.0
              let hitFreq := localX >= layoutFreq.x && localX <= layoutFreq.x + layoutFreq.width
                && localY >= layoutFreq.y - 8.0 && localY <= layoutFreq.y + layoutFreq.height + 8.0
              if hitDamp then
                let t := Linalg.Float.clamp ((localX - layoutDamp.x) / layoutDamp.width) 0.0 1.0
                stateRef.modify fun s => { s with dampingRatio := Demos.Linalg.springDampingFrom t, dragging := .sliderDamping }
              else if hitFreq then
                let t := Linalg.Float.clamp ((localX - layoutFreq.x) / layoutFreq.width) 0.0 1.0
                stateRef.modify fun s => { s with frequency := Demos.Linalg.springFrequencyFrom t, dragging := .sliderFrequency }
              else
                pure ()
          | none => pure ()
      | none => pure ()
    ) clickEvents
  performEvent_ clickAction

  let hoverEvents ← useAllHovers
  let hoverAction ← Event.mapM (fun data => do
    let state ← stateRef.get
    let localX := data.x
    match state.dragging with
    | .sliderDamping =>
        match data.nameMap.get? springName with
        | some wid =>
            match data.layouts.get wid with
            | some layout =>
                let rect := layout.contentRect
                let lx := localX - rect.x
                let layout := Demos.Linalg.springSliderLayout rect.width rect.height env.screenScale 0
                let t := Linalg.Float.clamp ((lx - layout.x) / layout.width) 0.0 1.0
                stateRef.set { state with dampingRatio := Demos.Linalg.springDampingFrom t }
            | none => pure ()
        | none => pure ()
    | .sliderFrequency =>
        match data.nameMap.get? springName with
        | some wid =>
            match data.layouts.get wid with
            | some layout =>
                let rect := layout.contentRect
                let lx := localX - rect.x
                let layout := Demos.Linalg.springSliderLayout rect.width rect.height env.screenScale 1
                let t := Linalg.Float.clamp ((lx - layout.x) / layout.width) 0.0 1.0
                stateRef.set { state with frequency := Demos.Linalg.springFrequencyFrom t }
            | none => pure ()
        | none => pure ()
    | .none => pure ()
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
          stateRef.set Demos.Linalg.springAnimationPlaygroundInitialState
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
        let newTime := current.time + dt
        let time := if newTime > 4.0 then newTime - 4.0 else newTime
        let ω := 2.0 * Linalg.Float.pi * current.frequency
        let x := Demos.Linalg.springResponse time current.dampingRatio ω
        let v := Demos.Linalg.springVelocity time current.dampingRatio ω
        let energy := 0.5 * (x * x + (v / ω) * (v / ω))
        let mut history := current.energyHistory.push energy
        if history.size > 140 then
          history := history.eraseIdxIfInBounds 0
        current := { current with time := time, energyHistory := history }
        stateRef.set current
      lastTimeRef.set t
      pure current
    let containerStyle : BoxStyle := {
      flexItem := some (FlexItem.growing 1)
      width := .percent 1.0
      height := .percent 1.0
    }
    emit (pure (namedColumn springName 0 containerStyle #[
      Demos.Linalg.springAnimationPlaygroundWidget env state
    ]))
  pure ()

end Demos
