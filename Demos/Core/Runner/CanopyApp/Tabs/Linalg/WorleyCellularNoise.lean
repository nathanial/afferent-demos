/-
  Demo Runner - Canopy app linalg WorleyCellularNoise tab content.
-/
import Reactive
import Afferent
import Afferent.Canopy
import Afferent.Canopy.Reactive
import Demos.Core.Demo
import Demos.Linalg.WorleyCellularNoise
import Trellis

open Reactive Reactive.Host
open Afferent
open Afferent.Arbor
open Afferent.Canopy
open Afferent.Canopy.Reactive
open Trellis

namespace Demos
def worleyCellularNoiseTabContent (env : DemoEnv) (elapsedTime : Dynamic Spider Float)
    (stateRef : IO.Ref Demos.Linalg.WorleyCellularState) : WidgetM Unit := do
  let worleyName ← registerComponentW "worley-cellular-noise"

  let clickEvents ← useClickData worleyName
  let clickAction ← Event.mapM (fun data => do
    if data.click.button != 0 then
      pure ()
    else
      match data.nameMap.get? worleyName with
      | some wid =>
          match data.layouts.get wid with
          | some layout =>
              let rect := layout.contentRect
              let localX := data.click.x - rect.x
              let localY := data.click.y - rect.y
              let drop := Demos.Linalg.worleyDropdownLayout rect.width rect.height env.screenScale
              let inDrop := localX >= drop.x && localX <= drop.x + drop.width
                && localY >= drop.y && localY <= drop.y + drop.height
              let state ← stateRef.get
              if inDrop then
                stateRef.set { state with dropdownOpen := !state.dropdownOpen }
              else if state.dropdownOpen then
                let mut selected : Option Demos.Linalg.WorleyMode := none
                for i in [:Demos.Linalg.worleyModeOptions.size] do
                  let optLayout := Demos.Linalg.worleyDropdownOptionLayout drop i
                  if localX >= optLayout.x && localX <= optLayout.x + optLayout.width
                      && localY >= optLayout.y && localY <= optLayout.y + optLayout.height then
                    selected := some (Demos.Linalg.worleyModeOptions.getD i .f1)
                match selected with
                | some opt => stateRef.set { state with mode := opt, dropdownOpen := false }
                | none => stateRef.set { state with dropdownOpen := false }
              else
                let layout := Demos.Linalg.worleySliderLayout rect.width rect.height env.screenScale
                let inSlider := localX >= layout.x && localX <= layout.x + layout.width
                  && localY >= layout.y - 10.0 && localY <= layout.y + layout.height + 10.0
                if inSlider then
                  let t := Linalg.Float.clamp ((localX - layout.x) / layout.width) 0.0 1.0
                  let newState := { state with jitter := Demos.Linalg.worleyJitterFromSlider t }
                  stateRef.set { newState with dragging := .slider }
                else
                  let toggleA := Demos.Linalg.worleyToggleLayout rect.width rect.height env.screenScale 0
                  let toggleB := Demos.Linalg.worleyToggleLayout rect.width rect.height env.screenScale 1
                  let toggleC := Demos.Linalg.worleyToggleLayout rect.width rect.height env.screenScale 2
                  let hitToggle (t : Demos.Linalg.WorleyToggleLayout) : Bool :=
                    localX >= t.x && localX <= t.x + t.size && localY >= t.y && localY <= t.y + t.size
                  if hitToggle toggleA then
                    stateRef.set { state with showEdges := !state.showEdges }
                  else if hitToggle toggleB then
                    stateRef.set { state with showPoints := !state.showPoints }
                  else if hitToggle toggleC then
                    stateRef.set { state with showConnections := !state.showConnections }
                  else
                    pure ()
          | none => pure ()
      | none => pure ()
    ) clickEvents
  performEvent_ clickAction

  let hoverEvents ← useAllHovers
  let hoverAction ← Event.mapM (fun data => do
    let state ← stateRef.get
    match state.dragging with
    | .slider =>
        match data.nameMap.get? worleyName with
        | some wid =>
            match data.layouts.get wid with
            | some layout =>
                let rect := layout.contentRect
                let localX := data.x - rect.x
                let layout := Demos.Linalg.worleySliderLayout rect.width rect.height env.screenScale
                let t := Linalg.Float.clamp ((localX - layout.x) / layout.width) 0.0 1.0
                stateRef.set { state with jitter := Demos.Linalg.worleyJitterFromSlider t }
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
          stateRef.set Demos.Linalg.worleyCellularInitialState
      | _ => pure ()
    ) keyEvents
  performEvent_ keyAction

  let _ ← dynWidget elapsedTime fun _ => do
    let state ← SpiderM.liftIO stateRef.get
    let containerStyle : BoxStyle := {
      flexItem := some (FlexItem.growing 1)
      width := .percent 1.0
      height := .percent 1.0
    }
    emit (pure (namedColumn worleyName 0 containerStyle #[
      Demos.Linalg.worleyCellularNoiseWidget env state
    ]))
  pure ()

end Demos
