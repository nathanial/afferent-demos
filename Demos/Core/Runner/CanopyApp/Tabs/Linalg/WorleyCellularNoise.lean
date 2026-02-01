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
def worleyCellularNoiseTabContent (env : DemoEnv) : WidgetM Unit := do
  let worleyName ← registerComponentW "worley-cellular-noise"

  let clickEvents ← useClickData worleyName
  let clickUpdates ← Event.mapM (fun data =>
    if data.click.button != 0 then
      id
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
              fun (state : Demos.Linalg.WorleyCellularState) =>
                if inDrop then
                  { state with dropdownOpen := !state.dropdownOpen }
                else if state.dropdownOpen then
                  let selected := (Array.range Demos.Linalg.worleyModeOptions.size).findSome? fun i =>
                    let optLayout := Demos.Linalg.worleyDropdownOptionLayout drop i
                    if localX >= optLayout.x && localX <= optLayout.x + optLayout.width
                        && localY >= optLayout.y && localY <= optLayout.y + optLayout.height then
                      some (Demos.Linalg.worleyModeOptions.getD i .f1)
                    else none
                  match selected with
                  | some opt => { state with mode := opt, dropdownOpen := false }
                  | none => { state with dropdownOpen := false }
                else
                  let layout := Demos.Linalg.worleySliderLayout rect.width rect.height env.screenScale
                  let inSlider := localX >= layout.x && localX <= layout.x + layout.width
                    && localY >= layout.y - 10.0 && localY <= layout.y + layout.height + 10.0
                  if inSlider then
                    let t := Linalg.Float.clamp ((localX - layout.x) / layout.width) 0.0 1.0
                    let newState := { state with jitter := Demos.Linalg.worleyJitterFromSlider t }
                    { newState with dragging := .slider }
                  else
                    let toggleA := Demos.Linalg.worleyToggleLayout rect.width rect.height env.screenScale 0
                    let toggleB := Demos.Linalg.worleyToggleLayout rect.width rect.height env.screenScale 1
                    let toggleC := Demos.Linalg.worleyToggleLayout rect.width rect.height env.screenScale 2
                    let hitToggle (t : Demos.Linalg.WorleyToggleLayout) : Bool :=
                      localX >= t.x && localX <= t.x + t.size && localY >= t.y && localY <= t.y + t.size
                    if hitToggle toggleA then
                      { state with showEdges := !state.showEdges }
                    else if hitToggle toggleB then
                      { state with showPoints := !state.showPoints }
                    else if hitToggle toggleC then
                      { state with showConnections := !state.showConnections }
                    else
                      state
          | none => id
      | none => id
    ) clickEvents

  let hoverEvents ← useAllHovers
  let hoverUpdates ← Event.mapM (fun data =>
    match data.nameMap.get? worleyName with
    | some wid =>
        match data.layouts.get wid with
        | some layout =>
            let rect := layout.contentRect
            let localX := data.x - rect.x
            fun (state : Demos.Linalg.WorleyCellularState) =>
              match state.dragging with
              | .slider =>
                  let layout := Demos.Linalg.worleySliderLayout rect.width rect.height env.screenScale
                  let t := Linalg.Float.clamp ((localX - layout.x) / layout.width) 0.0 1.0
                  { state with jitter := Demos.Linalg.worleyJitterFromSlider t }
              | .none => state
        | none => id
    | none => id
    ) hoverEvents

  let mouseUpEvents ← useAllMouseUp
  let mouseUpUpdates ← Event.mapM (fun _ =>
    fun (s : Demos.Linalg.WorleyCellularState) => { s with dragging := .none }
    ) mouseUpEvents

  let keyEvents ← useKeyboard
  let keyUpdates ← Event.mapM (fun data =>
    fun (s : Demos.Linalg.WorleyCellularState) =>
      if data.event.isPress then
        match data.event.key with
        | .char 'r' => Demos.Linalg.worleyCellularInitialState
        | _ => s
      else s
    ) keyEvents

  let allUpdates ← Event.mergeAllListM [clickUpdates, hoverUpdates, mouseUpUpdates, keyUpdates]
  let state ← foldDyn (fun f s => f s) Demos.Linalg.worleyCellularInitialState allUpdates

  let _ ← dynWidget state fun s => do
    let containerStyle : BoxStyle := {
      flexItem := some (FlexItem.growing 1)
      width := .percent 1.0
      height := .percent 1.0
    }
    emit (pure (namedColumn worleyName 0 containerStyle #[
      Demos.Linalg.worleyCellularNoiseWidget env s
    ]))
  pure ()

end Demos
