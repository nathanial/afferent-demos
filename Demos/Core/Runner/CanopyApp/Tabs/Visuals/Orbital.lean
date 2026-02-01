/-
  Demo Runner - Canopy app visuals Orbital tab content.
-/
import Reactive
import Afferent
import Afferent.Canopy
import Afferent.Canopy.Reactive
import Demos.Core.Demo
import Demos.Visuals.Orbital
import Trellis

open Reactive Reactive.Host
open Afferent
open Afferent.Arbor
open Afferent.Canopy
open Afferent.Canopy.Reactive
open Trellis

namespace Demos
def orbitalInstancedTabContent (env : DemoEnv) (elapsedTime : Dynamic Spider Float) : WidgetM Unit := do
  let _ ← dynWidget elapsedTime fun t => do
    emit (pure (orbitalInstancedWidget t env.screenScale env.windowWidthF env.windowHeightF
      env.fontMedium env.orbitalCount env.orbitalParams env.orbitalBuffer))
  pure ()

end Demos
