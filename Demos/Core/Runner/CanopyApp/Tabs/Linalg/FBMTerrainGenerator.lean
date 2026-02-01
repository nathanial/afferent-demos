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
  let stateRef ← SpiderM.liftIO (IO.mkRef Demos.Linalg.fbmTerrainInitialState)
  let terrainName ← registerComponentW "fbm-terrain-generator"

  let clickEvents ← useClickData terrainName
  let clickAction ← Event.mapM (fun data => do
    let button := data.click.button
    match button with
    | 1 =>
        stateRef.modify fun s =>
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
                if hitToggle toggleA then
                  stateRef.modify fun s => { s with showWireframe := !s.showWireframe }
                else if hitToggle toggleB then
                  stateRef.modify fun s => { s with showTexture := !s.showTexture }
                else if hitToggle toggleC then
                  stateRef.modify fun s => { s with showNormals := !s.showNormals }
                else
                  let sliders : Array Demos.Linalg.TerrainSlider :=
                    #[.scale, .height, .octaves, .lacunarity, .persistence, .power, .terrace]
                  let mut hit : Option (Nat × Demos.Linalg.TerrainSlider) := none
                  for i in [:sliders.size] do
                    let layout := Demos.Linalg.fbmTerrainSliderLayout rect.width rect.height env.screenScale i
                    let within := localX >= layout.x && localX <= layout.x + layout.width
                      && localY >= layout.y - 10.0 && localY <= layout.y + layout.height + 10.0
                    if within then
                      hit := some (i, sliders.getD i .scale)
                  match hit with
                  | some (idx, which) =>
                      let layout := Demos.Linalg.fbmTerrainSliderLayout rect.width rect.height env.screenScale idx
                      let t := Linalg.Float.clamp ((localX - layout.x) / layout.width) 0.0 1.0
                      let newState := Demos.Linalg.fbmTerrainApplySlider (← stateRef.get) which t
                      stateRef.set { newState with dragging := .slider which }
                  | none => pure ()
            | none => pure ()
        | none => pure ()
    | _ => pure ()
    ) clickEvents
  performEvent_ clickAction

  let hoverEvents ← useAllHovers
  let hoverAction ← Event.mapM (fun data => do
    let state ← stateRef.get
    match state.dragging with
    | .camera =>
        let dx := data.x - state.lastMouseX
        let dy := data.y - state.lastMouseY
        let newYaw := state.cameraYaw + dx * 0.005
        let newPitch := Linalg.Float.clamp (state.cameraPitch + dy * 0.005) (-0.2) 1.4
        stateRef.set { state with
          cameraYaw := newYaw
          cameraPitch := newPitch
          lastMouseX := data.x
          lastMouseY := data.y
        }
    | .slider which =>
        match data.nameMap.get? terrainName with
        | some wid =>
            match data.layouts.get wid with
            | some layout =>
                let rect := layout.contentRect
                let localX := data.x - rect.x
                let sliders : Array Demos.Linalg.TerrainSlider :=
                  #[.scale, .height, .octaves, .lacunarity, .persistence, .power, .terrace]
                let idx := sliders.findIdx? (fun s => s == which) |>.getD 0
                let layout := Demos.Linalg.fbmTerrainSliderLayout rect.width rect.height env.screenScale idx
                let t := Linalg.Float.clamp ((localX - layout.x) / layout.width) 0.0 1.0
                stateRef.set (Demos.Linalg.fbmTerrainApplySlider state which t)
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
          stateRef.set Demos.Linalg.fbmTerrainInitialState
      | .char 'w' =>
          stateRef.modify fun s => { s with showWireframe := !s.showWireframe }
      | .char 't' =>
          stateRef.modify fun s => { s with showTexture := !s.showTexture }
      | .char 'n' =>
          stateRef.modify fun s => { s with showNormals := !s.showNormals }
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
    emit (pure (namedColumn terrainName 0 containerStyle #[
      Demos.Linalg.fbmTerrainWidget env state
    ]))
  pure ()

end Demos
