/-
  Demo Runner - Canopy app linalg FBMTerrainGenerator tab content.
-/
import Reactive
import Afferent
import Afferent.Canopy
import Afferent.Canopy.Reactive
import Demos.Core.Demo
import Demos.Linalg.FBMTerrainGenerator
import Trellis

open Reactive Reactive.Host
open Afferent
open Afferent.Arbor
open Afferent.Canopy
open Afferent.Canopy.Reactive
open Trellis

namespace Demos
def fbmTerrainGeneratorTabContent (env : DemoEnv) : WidgetM Unit := do
  let elapsedTime ← useElapsedTime
  let terrainName ← registerComponentW "fbm-terrain-generator"

  let clickEvents ← useClickData terrainName
  let clickUpdates ← Event.mapM (fun data =>
    let button := data.click.button
    match button with
    | 1 =>
        fun (s : Demos.Linalg.FBMTerrainState) =>
          { s with dragging := .camera, lastMouseX := data.click.x, lastMouseY := data.click.y }
    | 0 =>
        match data.nameMap.get? terrainName with
        | some wid =>
            match data.layouts.get wid with
            | some layout =>
                let rect := layout.contentRect
                let localX := data.click.x - rect.x
                let localY := data.click.y - rect.y
                let toggleA := Demos.Linalg.fbmTerrainToggleLayout rect.width rect.height env.screenScale 0
                let toggleB := Demos.Linalg.fbmTerrainToggleLayout rect.width rect.height env.screenScale 1
                let toggleC := Demos.Linalg.fbmTerrainToggleLayout rect.width rect.height env.screenScale 2
                let hitToggle (t : Demos.Linalg.FBMTerrainToggleLayout) : Bool :=
                  localX >= t.x && localX <= t.x + t.size && localY >= t.y && localY <= t.y + t.size
                fun (state : Demos.Linalg.FBMTerrainState) =>
                  if hitToggle toggleA then
                    { state with showWireframe := !state.showWireframe }
                  else if hitToggle toggleB then
                    { state with showTexture := !state.showTexture }
                  else if hitToggle toggleC then
                    { state with showNormals := !state.showNormals }
                  else
                    let sliders : Array Demos.Linalg.TerrainSlider :=
                      #[.scale, .height, .octaves, .lacunarity, .persistence, .power, .terrace]
                    let hit := (Array.range sliders.size).findSome? fun i =>
                      let layout := Demos.Linalg.fbmTerrainSliderLayout rect.width rect.height env.screenScale i
                      let within := localX >= layout.x && localX <= layout.x + layout.width
                        && localY >= layout.y - 10.0 && localY <= layout.y + layout.height + 10.0
                      if within then some (i, sliders.getD i .scale) else none
                    match hit with
                    | some (idx, which) =>
                        let layout := Demos.Linalg.fbmTerrainSliderLayout rect.width rect.height env.screenScale idx
                        let t := Linalg.Float.clamp ((localX - layout.x) / layout.width) 0.0 1.0
                        let newState := Demos.Linalg.fbmTerrainApplySlider state which t
                        { newState with dragging := .slider which }
                    | none => state
            | none => id
        | none => id
    | _ => id
    ) clickEvents

  let hoverEvents ← useAllHovers
  let hoverUpdates ← Event.mapM (fun data =>
    match data.nameMap.get? terrainName with
    | some wid =>
        match data.layouts.get wid with
        | some layout =>
            let rect := layout.contentRect
            let localX := data.x - rect.x
            fun (state : Demos.Linalg.FBMTerrainState) =>
              match state.dragging with
              | .camera =>
                  let dx := data.x - state.lastMouseX
                  let dy := data.y - state.lastMouseY
                  let newYaw := state.cameraYaw + dx * 0.005
                  let newPitch := Linalg.Float.clamp (state.cameraPitch + dy * 0.005) (-0.2) 1.4
                  { state with
                    cameraYaw := newYaw
                    cameraPitch := newPitch
                    lastMouseX := data.x
                    lastMouseY := data.y
                  }
              | .slider which =>
                  let sliders : Array Demos.Linalg.TerrainSlider :=
                    #[.scale, .height, .octaves, .lacunarity, .persistence, .power, .terrace]
                  let idx := sliders.findIdx? (fun s => s == which) |>.getD 0
                  let layout := Demos.Linalg.fbmTerrainSliderLayout rect.width rect.height env.screenScale idx
                  let t := Linalg.Float.clamp ((localX - layout.x) / layout.width) 0.0 1.0
                  Demos.Linalg.fbmTerrainApplySlider state which t
              | .none => state
        | none => id
    | none => id
    ) hoverEvents

  let mouseUpEvents ← useAllMouseUp
  let mouseUpUpdates ← Event.mapM (fun _ =>
    fun (s : Demos.Linalg.FBMTerrainState) => { s with dragging := .none }
    ) mouseUpEvents

  let keyEvents ← useKeyboard
  let keyUpdates ← Event.mapM (fun data =>
    fun (s : Demos.Linalg.FBMTerrainState) =>
      if data.event.isPress then
        match data.event.key with
        | .char 'r' => Demos.Linalg.fbmTerrainInitialState
        | .char 'w' => { s with showWireframe := !s.showWireframe }
        | .char 't' => { s with showTexture := !s.showTexture }
        | .char 'n' => { s with showNormals := !s.showNormals }
        | _ => s
      else s
    ) keyEvents

  let allUpdates ← Event.mergeAllListM [clickUpdates, hoverUpdates, mouseUpUpdates, keyUpdates]
  let state ← foldDyn (fun f s => f s) Demos.Linalg.fbmTerrainInitialState allUpdates

  let _ ← dynWidget state fun s => do
    let containerStyle : BoxStyle := {
      flexItem := some (FlexItem.growing 1)
      width := .percent 1.0
      height := .percent 1.0
    }
    emit (pure (namedColumn terrainName 0 containerStyle #[
      Demos.Linalg.fbmTerrainWidget env s
    ]))
  pure ()

end Demos
