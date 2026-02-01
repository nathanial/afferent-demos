/-
  Demo Runner - Canopy app linalg CollisionResponseDemo tab content.
-/
import Reactive
import Afferent
import Afferent.Canopy
import Afferent.Canopy.Reactive
import Demos.Core.Demo
import Demos.Linalg.CollisionResponseDemo
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
  let y := 90 * screenScale + idx.toFloat * 36 * screenScale
  { x := x, y := y, width := width, height := height }

private def hitSlider (layout : SliderLayout) (x y : Float) : Bool :=
  x >= layout.x && x <= layout.x + layout.width &&
  y >= layout.y - 8.0 && y <= layout.y + layout.height + 8.0

private def sliderValue (layout : SliderLayout) (x : Float) : Float :=
  Linalg.Float.clamp ((x - layout.x) / layout.width) 0.0 1.0

private def updateSlider (which : Demos.Linalg.CollisionSlider) (t : Float)
    (state : Demos.Linalg.CollisionResponseDemoState) : Demos.Linalg.CollisionResponseDemoState :=
  match which with
  | .restitution => { state with restitution := t }
  | .friction => { state with friction := t }


def collisionResponseDemoTabContent (env : DemoEnv) : WidgetM Unit := do
  let animFrame ← useAnimationFrame
  let demoName ← registerComponentW "collision-response-demo"

  let keyEvents ← useKeyboard
  let keyUpdates ← Event.mapM (fun data =>
    fun (s : Demos.Linalg.CollisionResponseDemoState) =>
      if data.event.isPress then
        match data.event.key with
        | .char 'r' => Demos.Linalg.collisionResponseDemoInitialState
        | .space => { s with animating := !s.animating }
        | _ => s
      else s
    ) keyEvents

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
              let restLayout := sliderLayout rect.width rect.height env.screenScale 0
              let fricLayout := sliderLayout rect.width rect.height env.screenScale 1
              fun (state : Demos.Linalg.CollisionResponseDemoState) =>
                if hitSlider restLayout localX localY then
                  let t := sliderValue restLayout localX
                  updateSlider .restitution t { state with dragging := some .restitution }
                else if hitSlider fricLayout localX localY then
                  let t := sliderValue fricLayout localX
                  updateSlider .friction t { state with dragging := some .friction }
                else
                  state
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
            fun (state : Demos.Linalg.CollisionResponseDemoState) =>
              match state.dragging with
              | some which =>
                  let layout := match which with
                    | .restitution => sliderLayout rect.width rect.height env.screenScale 0
                    | .friction => sliderLayout rect.width rect.height env.screenScale 1
                  let t := sliderValue layout localX
                  updateSlider which t state
              | none => state
        | none => id
    | none => id
    ) hoverEvents

  let mouseUpEvents ← useAllMouseUp
  let mouseUpUpdates ← Event.mapM (fun _ =>
    fun (s : Demos.Linalg.CollisionResponseDemoState) => { s with dragging := none }
    ) mouseUpEvents

  let animUpdates ← Event.mapM (fun dt =>
    fun (s : Demos.Linalg.CollisionResponseDemoState) =>
      Demos.Linalg.stepCollisionResponseDemo s dt
    ) animFrame

  let allUpdates ← Event.mergeAllListM [keyUpdates, clickUpdates, hoverUpdates, mouseUpUpdates, animUpdates]
  let state ← foldDyn (fun f s => f s) Demos.Linalg.collisionResponseDemoInitialState allUpdates

  let _ ← dynWidget state fun s => do
    let containerStyle : BoxStyle := {
      flexItem := some (FlexItem.growing 1)
      width := .percent 1.0
      height := .percent 1.0
    }
    emit (pure (namedColumn demoName 0 containerStyle #[
      Demos.Linalg.collisionResponseDemoWidget env s
    ]))
  pure ()

end Demos
