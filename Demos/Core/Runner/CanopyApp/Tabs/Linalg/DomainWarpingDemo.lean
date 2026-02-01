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
def domainWarpingDemoTabContent (env : DemoEnv) (elapsedTime : Dynamic Spider Float)
    (stateRef : IO.Ref Demos.Linalg.DomainWarpingState)
    (lastTimeRef : IO.Ref Float) : WidgetM Unit := do
  let warpName ← registerComponentW "domain-warping-demo"

  let clickEvents ← useClickData warpName
  let clickAction ← Event.mapM (fun data => do
    if data.click.button != 0 then
      pure ()
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
              if hitToggle toggleA then
                stateRef.modify fun s => { s with useAdvanced := !s.useAdvanced }
              else if hitToggle toggleB then
                stateRef.modify fun s => { s with animate := !s.animate }
              else if hitToggle toggleC then
                stateRef.modify fun s => { s with showVectors := !s.showVectors }
              else
                let sliders : Array Demos.Linalg.WarpingSlider := #[.strength1, .strength2, .scale, .speed]
                let mut hit : Option (Nat × Demos.Linalg.WarpingSlider) := none
                for i in [:sliders.size] do
                  let layout := Demos.Linalg.domainWarpingSliderLayout rect.width rect.height env.screenScale i
                  let within := localX >= layout.x && localX <= layout.x + layout.width
                    && localY >= layout.y - 10.0 && localY <= layout.y + layout.height + 10.0
                  if within then
                    hit := some (i, sliders.getD i .strength1)
                match hit with
                | some (idx, which) =>
                    let layout := Demos.Linalg.domainWarpingSliderLayout rect.width rect.height env.screenScale idx
                    let t := Linalg.Float.clamp ((localX - layout.x) / layout.width) 0.0 1.0
                    let newState := Demos.Linalg.domainWarpingApplySlider (← stateRef.get) which t
                    stateRef.set { newState with dragging := .slider which }
                | none => pure ()
          | none => pure ()
      | none => pure ()
    ) clickEvents
  performEvent_ clickAction

  let hoverEvents ← useAllHovers
  let hoverAction ← Event.mapM (fun data => do
    let state ← stateRef.get
    match state.dragging with
    | .slider which =>
        match data.nameMap.get? warpName with
        | some wid =>
            match data.layouts.get wid with
            | some layout =>
                let rect := layout.contentRect
                let localX := data.x - rect.x
                let sliders : Array Demos.Linalg.WarpingSlider := #[.strength1, .strength2, .scale, .speed]
                let idx := sliders.findIdx? (fun s => s == which) |>.getD 0
                let layout := Demos.Linalg.domainWarpingSliderLayout rect.width rect.height env.screenScale idx
                let t := Linalg.Float.clamp ((localX - layout.x) / layout.width) 0.0 1.0
                stateRef.set (Demos.Linalg.domainWarpingApplySlider state which t)
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
          stateRef.set Demos.Linalg.domainWarpingInitialState
      | _ => pure ()
    ) keyEvents
  performEvent_ keyAction

  let _ ← dynWidget elapsedTime fun t => do
    let state ← SpiderM.liftIO do
      let lastT ← lastTimeRef.get
      let dt := if lastT == 0.0 then 0.0 else max 0.0 (t - lastT)
      let mut current ← stateRef.get
      if current.animate then
        current := { current with time := current.time + dt * current.speed }
        stateRef.set current
      lastTimeRef.set t
      pure current
    let containerStyle : BoxStyle := {
      flexItem := some (FlexItem.growing 1)
      width := .percent 1.0
      height := .percent 1.0
    }
    emit (pure (namedColumn warpName 0 containerStyle #[
      Demos.Linalg.domainWarpingDemoWidget env state
    ]))
  pure ()

end Demos
