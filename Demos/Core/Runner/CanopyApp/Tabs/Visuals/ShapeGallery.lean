/-
  Demo Runner - Canopy app visuals ShapeGallery tab content.
-/
import Reactive
import Afferent
import Afferent.Canopy
import Afferent.Canopy.Reactive
import Demos.Core.Demo
import Demos.Visuals.ShapeGallery
import Trellis

open Reactive Reactive.Host
open Afferent
open Afferent.Arbor
open Afferent.Canopy
open Afferent.Canopy.Reactive
open Trellis

namespace Demos

structure ShapeGalleryState where
  index : Nat := 0
  lastTime : Float := 0.0
  deriving Inhabited

def shapeGalleryTabContent (env : DemoEnv) : WidgetM Unit := do
  let elapsedTime ← useElapsedTime
  let keyEvents ← useKeyboard

  -- Map key events to state update functions
  let keyUpdates ← Event.mapM (fun data =>
    let total := shapeGalleryCount
    if total == 0 then
      fun s => s
    else
      match data.event.key with
      | .right | .space =>
          fun s => { s with index := (s.index + 1) % total }
      | .left =>
          fun s => { s with index := if s.index == 0 then total - 1 else s.index - 1 }
      | _ => fun s => s
    ) keyEvents

  -- Map elapsed time updates to state update functions (for tracking lastTime)
  let timeUpdates ← Event.mapM (fun t =>
    fun s => { s with lastTime := t }
    ) elapsedTime.updated

  -- Merge all updates and fold into state
  let allUpdates ← Event.mergeAllListM [keyUpdates, timeUpdates]
  let state ← foldDyn (fun f s => f s) ({} : ShapeGalleryState) allUpdates

  let _ ← dynWidget state fun s => do
    emit (pure (shapeGalleryWidget s.index env.screenScale env.fontLarge env.fontSmall env.fontMedium))
  pure ()

end Demos
