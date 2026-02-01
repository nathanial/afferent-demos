/-
  Demo Runner - Canopy app linalg InertiaTensorVisualizer tab content.
-/
import Reactive
import Afferent
import Afferent.Canopy
import Afferent.Canopy.Reactive
import Demos.Core.Demo
import Demos.Linalg.InertiaTensorVisualizer
import Trellis

open Reactive Reactive.Host
open Afferent
open Afferent.Arbor
open Afferent.Canopy
open Afferent.Canopy.Reactive
open Trellis

namespace Demos

private structure SliderLayout where
  x : Float
  y : Float
  width : Float
  height : Float

private def sliderLayout (w _h screenScale : Float) (idx : Nat) : SliderLayout :=
  let width := 220 * screenScale
  let height := 16 * screenScale
  let x := w - width - 30 * screenScale
  let y := 110 * screenScale + idx.toFloat * 34 * screenScale
  { x := x, y := y, width := width, height := height }

private structure DropdownLayout where
  x : Float
  y : Float
  width : Float
  height : Float

private def dropdownLayout (w _h screenScale : Float) : DropdownLayout :=
  let width := 160 * screenScale
  let height := 28 * screenScale
  let x := w - width - 30 * screenScale
  let y := 30 * screenScale
  { x := x, y := y, width := width, height := height }

private def dropdownOptionLayout (drop : DropdownLayout) (idx : Nat) : DropdownLayout :=
  { x := drop.x, y := drop.y + drop.height + idx.toFloat * drop.height,
    width := drop.width, height := drop.height }

private def sliderValue (layout : SliderLayout) (x : Float) : Float :=
  Linalg.Float.clamp ((x - layout.x) / layout.width) 0.0 1.0

private def sliderRange (which : Demos.Linalg.TensorSlider) : Float × Float :=
  match which with
  | .sizeA => (0.3, 2.5)
  | .sizeB => (0.3, 2.5)
  | .sizeC => (0.3, 2.5)
  | .mass => (0.5, 5.0)
  | .offsetX => (-1.5, 1.5)
  | .offsetY => (-1.5, 1.5)

private def applySlider (state : Demos.Linalg.InertiaTensorVisualizerState)
    (which : Demos.Linalg.TensorSlider) (t : Float) : Demos.Linalg.InertiaTensorVisualizerState :=
  let (minV, maxV) := sliderRange which
  let value := minV + t * (maxV - minV)
  match which with
  | .sizeA => { state with sizeA := value }
  | .sizeB => { state with sizeB := value }
  | .sizeC => { state with sizeC := value }
  | .mass => { state with mass := value }
  | .offsetX => { state with offsetX := value }
  | .offsetY => { state with offsetY := value }

def inertiaTensorVisualizerTabContent (env : DemoEnv) : WidgetM Unit := do
  let demoName ← registerComponentW "inertia-tensor-visualizer"

  let clickEvents ← useClickData demoName
  let clickUpdates ← Event.mapM (fun data =>
    if data.click.button != 0 then
      id
    else
      match data.nameMap.get? demoName with
      | some wid =>
          match data.layouts.get wid with
          | some layout =>
              let rect := layout.contentRect
              let localX := data.click.x - rect.x
              let localY := data.click.y - rect.y
              let drop := dropdownLayout rect.width rect.height env.screenScale
              let inDrop := localX >= drop.x && localX <= drop.x + drop.width
                && localY >= drop.y && localY <= drop.y + drop.height
              fun (state : Demos.Linalg.InertiaTensorVisualizerState) =>
                if inDrop then
                  { state with dropdownOpen := !state.dropdownOpen }
                else if state.dropdownOpen then
                  let options : Array Demos.Linalg.TensorShape := #[.sphere, .box, .cylinder]
                  let selected := (Array.range options.size).findSome? fun i =>
                    let optLayout := dropdownOptionLayout drop i
                    if localX >= optLayout.x && localX <= optLayout.x + optLayout.width
                        && localY >= optLayout.y && localY <= optLayout.y + optLayout.height then
                      some (options.getD i .sphere)
                    else none
                  match selected with
                  | some opt => { state with shape := opt, dropdownOpen := false }
                  | none => { state with dropdownOpen := false }
                else
                  let sliders : Array Demos.Linalg.TensorSlider :=
                    #[.sizeA, .sizeB, .sizeC, .mass, .offsetX, .offsetY]
                  let hit := (Array.range sliders.size).findSome? fun i =>
                    let layout := sliderLayout rect.width rect.height env.screenScale i
                    let within := localX >= layout.x && localX <= layout.x + layout.width
                      && localY >= layout.y - 10.0 && localY <= layout.y + layout.height + 10.0
                    if within then some (sliders.getD i .sizeA) else none
                  match hit with
                  | some which =>
                      let idx := sliders.findIdx? (fun s => s == which) |>.getD 0
                      let layout := sliderLayout rect.width rect.height env.screenScale idx
                      let t := sliderValue layout localX
                      let newState := applySlider state which t
                      { newState with dragging := some which }
                  | none => state
          | none => id
      | none => id
    ) clickEvents

  let hoverEvents ← useAllHovers
  let hoverUpdates ← Event.mapM (fun data =>
    match data.nameMap.get? demoName with
    | some wid =>
        match data.layouts.get wid with
        | some layout =>
            let rect := layout.contentRect
            let localX := data.x - rect.x
            fun (state : Demos.Linalg.InertiaTensorVisualizerState) =>
              match state.dragging with
              | some which =>
                  let sliders : Array Demos.Linalg.TensorSlider :=
                    #[.sizeA, .sizeB, .sizeC, .mass, .offsetX, .offsetY]
                  let idx := sliders.findIdx? (fun s => s == which) |>.getD 0
                  let layout := sliderLayout rect.width rect.height env.screenScale idx
                  let t := sliderValue layout localX
                  applySlider state which t
              | none => state
        | none => id
    | none => id
    ) hoverEvents

  let mouseUpEvents ← useAllMouseUp
  let mouseUpUpdates ← Event.mapM (fun _ =>
    fun (s : Demos.Linalg.InertiaTensorVisualizerState) => { s with dragging := none }
    ) mouseUpEvents

  let keyEvents ← useKeyboard
  let keyUpdates ← Event.mapM (fun data =>
    fun (s : Demos.Linalg.InertiaTensorVisualizerState) =>
      if data.event.isPress then
        match data.event.key with
        | .char 'r' => Demos.Linalg.inertiaTensorVisualizerInitialState
        | _ => s
      else s
    ) keyEvents

  let allUpdates ← Event.mergeAllListM [clickUpdates, hoverUpdates, mouseUpUpdates, keyUpdates]
  let state ← foldDyn (fun f s => f s) Demos.Linalg.inertiaTensorVisualizerInitialState allUpdates

  let _ ← dynWidget state fun s => do
    let containerStyle : BoxStyle := {
      flexItem := some (FlexItem.growing 1)
      width := .percent 1.0
      height := .percent 1.0
    }
    emit (pure (namedColumn demoName 0 containerStyle #[
      Demos.Linalg.inertiaTensorVisualizerWidget env s
    ]))
  pure ()

end Demos
