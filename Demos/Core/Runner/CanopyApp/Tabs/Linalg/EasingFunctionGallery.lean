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
  let stateRef ← SpiderM.liftIO (IO.mkRef Demos.Linalg.easingFunctionGalleryInitialState)
  let lastTimeRef ← SpiderM.liftIO (IO.mkRef 0.0)
  let easingName ← registerComponentW "easing-function-gallery"

  let keyEvents ← useKeyboard
  let keyAction ← Event.mapM (fun data => do
    if data.event.isPress then
      let count := Demos.Linalg.easingEntryCount
      if count == 0 then
        pure ()
      else
        stateRef.modify fun s =>
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
    ) keyEvents
  performEvent_ keyAction

  let _ ← dynWidget elapsedTime fun t => do
    let state ← SpiderM.liftIO do
      let lastT ← lastTimeRef.get
      let dt := if lastT == 0.0 then 0.0 else max 0.0 (t - lastT)
      let mut current ← stateRef.get
      let count := Demos.Linalg.easingEntryCount
      if current.animating && count > 0 then
        let newT := current.t + dt * current.speed
        current := { current with t := if newT > 1.0 then newT - 1.0 else newT }
      if count > 0 && current.compare == current.selected then
        current := { current with compare := (current.compare + 1) % count }
      stateRef.set current
      lastTimeRef.set t
      pure current
    let containerStyle : BoxStyle := {
      flexItem := some (FlexItem.growing 1)
      width := .percent 1.0
      height := .percent 1.0
    }
    emit (pure (namedColumn easingName 0 containerStyle #[
      Demos.Linalg.easingFunctionGalleryWidget env state
    ]))
  pure ()

end Demos
