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
  let lastTimeRef ← SpiderM.liftIO (IO.mkRef 0.0)
  let springName ← registerComponentW "spring-animation-playground"

  let clickEvents ← useClickData springName
  let clickUpdates ← Event.mapM (fun data =>
    if data.click.button != 0 then
      id
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
              fun (state : Demos.Linalg.SpringAnimationPlaygroundState) =>
                if hitDamp then
                  let t := Linalg.Float.clamp ((localX - layoutDamp.x) / layoutDamp.width) 0.0 1.0
                  { state with dampingRatio := Demos.Linalg.springDampingFrom t, dragging := .sliderDamping }
                else if hitFreq then
                  let t := Linalg.Float.clamp ((localX - layoutFreq.x) / layoutFreq.width) 0.0 1.0
                  { state with frequency := Demos.Linalg.springFrequencyFrom t, dragging := .sliderFrequency }
                else
                  state
          | none => id
      | none => id
    ) clickEvents

  let hoverEvents ← useAllHovers
  let hoverUpdates ← Event.mapM (fun data =>
    match data.nameMap.get? springName with
    | some wid =>
        match data.layouts.get wid with
        | some layout =>
            let rect := layout.contentRect
            let lx := data.x - rect.x
            fun (state : Demos.Linalg.SpringAnimationPlaygroundState) =>
              match state.dragging with
              | .sliderDamping =>
                  let layout := Demos.Linalg.springSliderLayout rect.width rect.height env.screenScale 0
                  let t := Linalg.Float.clamp ((lx - layout.x) / layout.width) 0.0 1.0
                  { state with dampingRatio := Demos.Linalg.springDampingFrom t }
              | .sliderFrequency =>
                  let layout := Demos.Linalg.springSliderLayout rect.width rect.height env.screenScale 1
                  let t := Linalg.Float.clamp ((lx - layout.x) / layout.width) 0.0 1.0
                  { state with frequency := Demos.Linalg.springFrequencyFrom t }
              | .none => state
        | none => id
    | none => id
    ) hoverEvents

  let mouseUpEvents ← useAllMouseUp
  let mouseUpUpdates ← Event.mapM (fun _ =>
    fun (s : Demos.Linalg.SpringAnimationPlaygroundState) => { s with dragging := .none }
    ) mouseUpEvents

  let keyEvents ← useKeyboard
  let keyUpdates ← Event.mapM (fun data =>
    fun (s : Demos.Linalg.SpringAnimationPlaygroundState) =>
      if data.event.isPress then
        match data.event.key with
        | .char 'r' => Demos.Linalg.springAnimationPlaygroundInitialState
        | .space => { s with animating := !s.animating }
        | _ => s
      else s
    ) keyEvents

  let allUpdates ← Event.mergeAllListM [clickUpdates, hoverUpdates, mouseUpUpdates, keyUpdates]
  let state ← foldDyn (fun f s => f s) Demos.Linalg.springAnimationPlaygroundInitialState allUpdates

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
        let newTime := s.time + dt
        let time := if newTime > 4.0 then newTime - 4.0 else newTime
        let ω := 2.0 * Linalg.Float.pi * s.frequency
        let x := Demos.Linalg.springResponse time s.dampingRatio ω
        let v := Demos.Linalg.springVelocity time s.dampingRatio ω
        let energy := 0.5 * (x * x + (v / ω) * (v / ω))
        let history := s.energyHistory.push energy
        let history := if history.size > 140 then history.eraseIdxIfInBounds 0 else history
        { s with time := time, energyHistory := history }
      else s
    let containerStyle : BoxStyle := {
      flexItem := some (FlexItem.growing 1)
      width := .percent 1.0
      height := .percent 1.0
    }
    emit (pure (namedColumn springName 0 containerStyle #[
      Demos.Linalg.springAnimationPlaygroundWidget env finalState
    ]))
  pure ()

end Demos
