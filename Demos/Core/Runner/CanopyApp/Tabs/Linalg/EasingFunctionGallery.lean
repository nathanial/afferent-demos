/-
  Demo Runner - Canopy app linalg EasingFunctionGallery tab content.
-/
import Reactive
import Afferent
import Afferent.Canopy
import Afferent.Canopy.Reactive
import Demos.Core.Demo
import Demos.Linalg.EasingFunctionGallery
import Trellis

open Reactive Reactive.Host
open Afferent
open Afferent.Arbor
open Afferent.Canopy
open Afferent.Canopy.Reactive
open Trellis

namespace Demos
def easingFunctionGalleryTabContent (env : DemoEnv) : WidgetM Unit := do
  let elapsedTime ← useElapsedTime
  let easingName ← registerComponentW "easing-function-gallery"

  let keyEvents ← useKeyboard
  let keyUpdates ← Event.mapM (fun data =>
    fun (s : Demos.Linalg.EasingFunctionGalleryState) =>
      if data.event.isPress then
        let count := Demos.Linalg.easingEntryCount
        if count == 0 then
          s
        else
          let next :=
            match data.event.key with
            | .char 'r' =>
                Demos.Linalg.easingFunctionGalleryInitialState
            | .space =>
                { s with animating := !s.animating }
            | .char 'c' =>
                { s with compareMode := !s.compareMode }
            | .char 'x' =>
                { s with compare := (s.compare + 1) % count }
            | .tab =>
                if data.event.modifiers.shift then
                  { s with selected := (s.selected + count - 1) % count }
                else
                  { s with selected := (s.selected + 1) % count }
            | .left =>
                { s with selected := (s.selected + count - 1) % count }
            | .right =>
                { s with selected := (s.selected + 1) % count }
            | .up =>
                { s with speed := Linalg.Float.clamp (s.speed + 0.1) 0.1 3.0 }
            | .down =>
                { s with speed := Linalg.Float.clamp (s.speed - 0.1) 0.1 3.0 }
            | _ => s
          if next.compare == next.selected then
            { next with compare := (next.compare + 1) % count }
          else
            next
      else s
    ) keyEvents

  -- Time-based animation updates (track lastTime in state)
  let timeUpdates ← Event.mapM (fun t =>
    fun (state : Demos.Linalg.EasingFunctionGalleryState) =>
      let dt := if state.lastTime == 0.0 then 0.0 else max 0.0 (t - state.lastTime)
      let count := Demos.Linalg.easingEntryCount
      let current := if state.animating && count > 0 then
        let newT := state.t + dt * state.speed
        { state with t := if newT > 1.0 then newT - 1.0 else newT, lastTime := t }
      else
        { state with lastTime := t }
      if count > 0 && current.compare == current.selected then
        { current with compare := (current.compare + 1) % count }
      else
        current
    ) elapsedTime.updated

  let allUpdates ← Event.mergeAllListM [keyUpdates, timeUpdates]
  let state ← foldDyn (fun f s => f s) Demos.Linalg.easingFunctionGalleryInitialState allUpdates

  let _ ← dynWidget state fun s => do
    let containerStyle : BoxStyle := {
      flexItem := some (FlexItem.growing 1)
      width := .percent 1.0
      height := .percent 1.0
    }
    emit (pure (namedColumn easingName 0 containerStyle #[
      Demos.Linalg.easingFunctionGalleryWidget env s
    ]))
  pure ()

end Demos
