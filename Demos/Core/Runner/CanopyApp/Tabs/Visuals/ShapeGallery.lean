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
def shapeGalleryTabContent (env : DemoEnv) (elapsedTime : Dynamic Spider Float)
    (indexRef : IO.Ref Nat) : WidgetM Unit := do
  let keyEvents ← useKeyboard
  let keyAction ← Event.mapM (fun data => do
    let total := shapeGalleryCount
    if total == 0 then
      pure ()
    else
      match data.event.key with
      | .right | .space =>
          indexRef.modify fun idx => (idx + 1) % total
      | .left =>
          indexRef.modify fun idx => if idx == 0 then total - 1 else idx - 1
      | _ => pure ()
    ) keyEvents
  performEvent_ keyAction

  let _ ← dynWidget elapsedTime fun _ => do
    let idx ← SpiderM.liftIO indexRef.get
    emit (pure (shapeGalleryWidget idx env.screenScale env.fontLarge env.fontSmall env.fontMedium))
  pure ()

end Demos
