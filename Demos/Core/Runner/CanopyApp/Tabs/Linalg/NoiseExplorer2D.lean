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
  let elapsedTime ← useElapsedTime
  let stateRef ← SpiderM.liftIO (IO.mkRef Demos.Linalg.noiseExplorer2DInitialState)
  let noiseName ← registerComponentW "noise-explorer-2d"

  let clickEvents ← useClickData noiseName
  let clickAction ← Event.mapM (fun data => do
    if data.click.button != 0 then
      pure ()
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
              let state ← stateRef.get
              if inDrop then
                stateRef.set { state with dropdownOpen := !state.dropdownOpen }
              else if state.dropdownOpen then
                let mut selected : Option Demos.Linalg.NoiseType := none
                for i in [:Demos.Linalg.noiseExplorerOptions.size] do
                  let optLayout := Demos.Linalg.noiseExplorerDropdownOptionLayout drop i
                  if localX >= optLayout.x && localX <= optLayout.x + optLayout.width
                      && localY >= optLayout.y && localY <= optLayout.y + optLayout.height then
                    selected := some (Demos.Linalg.noiseExplorerOptions.getD i .perlin)
                match selected with
                | some opt => stateRef.set { state with noiseType := opt, dropdownOpen := false }
                | none => stateRef.set { state with dropdownOpen := false }
              else
                let toggle := Demos.Linalg.noiseExplorerFbmToggleLayout rect.width rect.height env.screenScale
                let inToggle := localX >= toggle.x && localX <= toggle.x + toggle.size
                  && localY >= toggle.y && localY <= toggle.y + toggle.size
                if inToggle then
                  stateRef.set { state with useFbm := !state.useFbm }
                else
                  let sliders : Array Demos.Linalg.NoiseExplorerSlider :=
                    #[.scale, .offsetX, .offsetY, .octaves, .lacunarity, .persistence, .jitter]
                  let mut hit : Option Demos.Linalg.NoiseExplorerSlider := none
                  for i in [:sliders.size] do
                    let layout := Demos.Linalg.noiseExplorerSliderLayout rect.width rect.height env.screenScale i
                    let within := localX >= layout.x && localX <= layout.x + layout.width
                      && localY >= layout.y - 10.0 && localY <= layout.y + layout.height + 10.0
                    if within then
                      hit := some (sliders.getD i .scale)
                  match hit with
                  | some which =>
                      let idx := sliders.findIdx? (fun s => s == which) |>.getD 0
                      let layout := Demos.Linalg.noiseExplorerSliderLayout rect.width rect.height env.screenScale idx
                      let t := Linalg.Float.clamp ((localX - layout.x) / layout.width) 0.0 1.0
                      let newState := Demos.Linalg.noiseExplorerApplySlider state which t
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
        match data.nameMap.get? noiseName with
        | some wid =>
            match data.layouts.get wid with
            | some layout =>
                let rect := layout.contentRect
                let localX := data.x - rect.x
                let sliders : Array Demos.Linalg.NoiseExplorerSlider :=
                  #[.scale, .offsetX, .offsetY, .octaves, .lacunarity, .persistence, .jitter]
                let idx := sliders.findIdx? (fun s => s == which) |>.getD 0
                let layout := Demos.Linalg.noiseExplorerSliderLayout rect.width rect.height env.screenScale idx
                let t := Linalg.Float.clamp ((localX - layout.x) / layout.width) 0.0 1.0
                stateRef.set (Demos.Linalg.noiseExplorerApplySlider state which t)
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
          stateRef.set Demos.Linalg.noiseExplorer2DInitialState
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
    emit (pure (namedColumn noiseName 0 containerStyle #[
      Demos.Linalg.noiseExplorer2DWidget env state
    ]))
  pure ()

end Demos
