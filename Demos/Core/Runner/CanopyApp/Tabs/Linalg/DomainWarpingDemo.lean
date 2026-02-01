/-
  Demo Runner - Canopy app linalg DomainWarpingDemo tab content.
-/
import Reactive
import Afferent
import Afferent.Canopy
import Afferent.Canopy.Reactive
import Demos.Core.Demo
import Demos.Linalg.DomainWarpingDemo
import Trellis

open Reactive Reactive.Host
open Afferent
open Afferent.Arbor
open Afferent.Canopy
open Afferent.Canopy.Reactive
open Trellis

namespace Demos
def domainWarpingDemoTabContent (env : DemoEnv) : WidgetM Unit := do
  let elapsedTime ← useElapsedTime
  let warpName ← registerComponentW "domain-warping-demo"

  let clickEvents ← useClickData warpName
  let clickUpdates ← Event.mapM (fun data =>
    if data.click.button != 0 then
      id
    else
      match data.nameMap.get? warpName with
      | some wid =>
          match data.layouts.get wid with
          | some layout =>
              let rect := layout.contentRect
              let localX := data.click.x - rect.x
              let localY := data.click.y - rect.y
              let toggleA := Demos.Linalg.domainWarpingToggleLayout rect.width rect.height env.screenScale 0
              let toggleB := Demos.Linalg.domainWarpingToggleLayout rect.width rect.height env.screenScale 1
              let toggleC := Demos.Linalg.domainWarpingToggleLayout rect.width rect.height env.screenScale 2
              let hitToggle (t : Demos.Linalg.DomainWarpingToggleLayout) : Bool :=
                localX >= t.x && localX <= t.x + t.size && localY >= t.y && localY <= t.y + t.size
              fun (state : Demos.Linalg.DomainWarpingState) =>
                if hitToggle toggleA then
                  { state with useAdvanced := !state.useAdvanced }
                else if hitToggle toggleB then
                  { state with animate := !state.animate }
                else if hitToggle toggleC then
                  { state with showVectors := !state.showVectors }
                else
                  let sliders : Array Demos.Linalg.WarpingSlider := #[.strength1, .strength2, .scale, .speed]
                  let hit := (Array.range sliders.size).findSome? fun i =>
                    let layout := Demos.Linalg.domainWarpingSliderLayout rect.width rect.height env.screenScale i
                    let within := localX >= layout.x && localX <= layout.x + layout.width
                      && localY >= layout.y - 10.0 && localY <= layout.y + layout.height + 10.0
                    if within then some (i, sliders.getD i .strength1) else none
                  match hit with
                  | some (idx, which) =>
                      let layout := Demos.Linalg.domainWarpingSliderLayout rect.width rect.height env.screenScale idx
                      let t := Linalg.Float.clamp ((localX - layout.x) / layout.width) 0.0 1.0
                      let newState := Demos.Linalg.domainWarpingApplySlider state which t
                      { newState with dragging := .slider which }
                  | none => state
          | none => id
      | none => id
    ) clickEvents

  let hoverEvents ← useAllHovers
  let hoverUpdates ← Event.mapM (fun data =>
    match data.nameMap.get? warpName with
    | some wid =>
        match data.layouts.get wid with
        | some layout =>
            let rect := layout.contentRect
            let localX := data.x - rect.x
            fun (state : Demos.Linalg.DomainWarpingState) =>
              match state.dragging with
              | .slider which =>
                  let sliders : Array Demos.Linalg.WarpingSlider := #[.strength1, .strength2, .scale, .speed]
                  let idx := sliders.findIdx? (fun s => s == which) |>.getD 0
                  let layout := Demos.Linalg.domainWarpingSliderLayout rect.width rect.height env.screenScale idx
                  let t := Linalg.Float.clamp ((localX - layout.x) / layout.width) 0.0 1.0
                  Demos.Linalg.domainWarpingApplySlider state which t
              | .none => state
        | none => id
    | none => id
    ) hoverEvents

  let mouseUpEvents ← useAllMouseUp
  let mouseUpUpdates ← Event.mapM (fun _ =>
    fun (s : Demos.Linalg.DomainWarpingState) => { s with dragging := .none }
    ) mouseUpEvents

  let keyEvents ← useKeyboard
  let keyUpdates ← Event.mapM (fun data =>
    fun (s : Demos.Linalg.DomainWarpingState) =>
      if data.event.isPress then
        match data.event.key with
        | .char 'r' => Demos.Linalg.domainWarpingInitialState
        | _ => s
      else s
    ) keyEvents

  -- Time-based animation updates (track lastTime in state)
  let timeUpdates ← Event.mapM (fun t =>
    fun (state : Demos.Linalg.DomainWarpingState) =>
      let dt := if state.lastTime == 0.0 then 0.0 else max 0.0 (t - state.lastTime)
      if state.animate then
        { state with time := state.time + dt * state.speed, lastTime := t }
      else
        { state with lastTime := t }
    ) elapsedTime.updated

  let allUpdates ← Event.mergeAllListM [clickUpdates, hoverUpdates, mouseUpUpdates, keyUpdates, timeUpdates]
  let state ← foldDyn (fun f s => f s) Demos.Linalg.domainWarpingInitialState allUpdates

  let _ ← dynWidget state fun s => do
    let containerStyle : BoxStyle := {
      flexItem := some (FlexItem.growing 1)
      width := .percent 1.0
      height := .percent 1.0
    }
    emit (pure (namedColumn warpName 0 containerStyle #[
      Demos.Linalg.domainWarpingDemoWidget env s
    ]))
  pure ()

end Demos
