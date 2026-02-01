/-
  Demo Runner - Canopy app linalg NoiseExplorer2D tab content.
-/
import Reactive
import Afferent
import Afferent.Canopy
import Afferent.Canopy.Reactive
import Demos.Core.Demo
import Demos.Linalg.NoiseExplorer2D
import Trellis

open Reactive Reactive.Host
open Afferent
open Afferent.Arbor
open Afferent.Canopy
open Afferent.Canopy.Reactive
open Trellis

namespace Demos
def noiseExplorer2DTabContent (env : DemoEnv) : WidgetM Unit := do
  let noiseName ← registerComponentW "noise-explorer-2d"

  let clickEvents ← useClickData noiseName
  let clickUpdates ← Event.mapM (fun data =>
    if data.click.button != 0 then
      id
    else
      match data.nameMap.get? noiseName with
      | some wid =>
          match data.layouts.get wid with
          | some layout =>
              let rect := layout.contentRect
              let localX := data.click.x - rect.x
              let localY := data.click.y - rect.y
              let drop := Demos.Linalg.noiseExplorerDropdownLayout rect.width rect.height env.screenScale
              let inDrop := localX >= drop.x && localX <= drop.x + drop.width
                && localY >= drop.y && localY <= drop.y + drop.height
              fun (state : Demos.Linalg.NoiseExplorerState) =>
                if inDrop then
                  { state with dropdownOpen := !state.dropdownOpen }
                else if state.dropdownOpen then
                  let selected := (Array.range Demos.Linalg.noiseExplorerOptions.size).findSome? fun i =>
                    let optLayout := Demos.Linalg.noiseExplorerDropdownOptionLayout drop i
                    if localX >= optLayout.x && localX <= optLayout.x + optLayout.width
                        && localY >= optLayout.y && localY <= optLayout.y + optLayout.height then
                      some (Demos.Linalg.noiseExplorerOptions.getD i .perlin)
                    else none
                  match selected with
                  | some opt => { state with noiseType := opt, dropdownOpen := false }
                  | none => { state with dropdownOpen := false }
                else
                  let toggle := Demos.Linalg.noiseExplorerFbmToggleLayout rect.width rect.height env.screenScale
                  let inToggle := localX >= toggle.x && localX <= toggle.x + toggle.size
                    && localY >= toggle.y && localY <= toggle.y + toggle.size
                  if inToggle then
                    { state with useFbm := !state.useFbm }
                  else
                    let sliders : Array Demos.Linalg.NoiseExplorerSlider :=
                      #[.scale, .offsetX, .offsetY, .octaves, .lacunarity, .persistence, .jitter]
                    let hit := (Array.range sliders.size).findSome? fun i =>
                      let layout := Demos.Linalg.noiseExplorerSliderLayout rect.width rect.height env.screenScale i
                      let within := localX >= layout.x && localX <= layout.x + layout.width
                        && localY >= layout.y - 10.0 && localY <= layout.y + layout.height + 10.0
                      if within then some (sliders.getD i .scale) else none
                    match hit with
                    | some which =>
                        let idx := sliders.findIdx? (fun s => s == which) |>.getD 0
                        let layout := Demos.Linalg.noiseExplorerSliderLayout rect.width rect.height env.screenScale idx
                        let t := Linalg.Float.clamp ((localX - layout.x) / layout.width) 0.0 1.0
                        let newState := Demos.Linalg.noiseExplorerApplySlider state which t
                        { newState with dragging := .slider which }
                    | none => state
          | none => id
      | none => id
    ) clickEvents

  let hoverEvents ← useAllHovers
  let hoverUpdates ← Event.mapM (fun data =>
    match data.nameMap.get? noiseName with
    | some wid =>
        match data.layouts.get wid with
        | some layout =>
            let rect := layout.contentRect
            let localX := data.x - rect.x
            fun (state : Demos.Linalg.NoiseExplorerState) =>
              match state.dragging with
              | .slider which =>
                  let sliders : Array Demos.Linalg.NoiseExplorerSlider :=
                    #[.scale, .offsetX, .offsetY, .octaves, .lacunarity, .persistence, .jitter]
                  let idx := sliders.findIdx? (fun s => s == which) |>.getD 0
                  let layout := Demos.Linalg.noiseExplorerSliderLayout rect.width rect.height env.screenScale idx
                  let t := Linalg.Float.clamp ((localX - layout.x) / layout.width) 0.0 1.0
                  Demos.Linalg.noiseExplorerApplySlider state which t
              | .none => state
        | none => id
    | none => id
    ) hoverEvents

  let mouseUpEvents ← useAllMouseUp
  let mouseUpUpdates ← Event.mapM (fun _ =>
    fun (s : Demos.Linalg.NoiseExplorerState) => { s with dragging := .none }
    ) mouseUpEvents

  let keyEvents ← useKeyboard
  let keyUpdates ← Event.mapM (fun data =>
    fun (s : Demos.Linalg.NoiseExplorerState) =>
      if data.event.isPress then
        match data.event.key with
        | .char 'r' => Demos.Linalg.noiseExplorer2DInitialState
        | _ => s
      else s
    ) keyEvents

  let allUpdates ← Event.mergeAllListM [clickUpdates, hoverUpdates, mouseUpUpdates, keyUpdates]
  let state ← foldDyn (fun f s => f s) Demos.Linalg.noiseExplorer2DInitialState allUpdates

  let _ ← dynWidget state fun s => do
    let containerStyle : BoxStyle := {
      flexItem := some (FlexItem.growing 1)
      width := .percent 1.0
      height := .percent 1.0
    }
    emit (pure (namedColumn noiseName 0 containerStyle #[
      Demos.Linalg.noiseExplorer2DWidget env s
    ]))
  pure ()

end Demos
