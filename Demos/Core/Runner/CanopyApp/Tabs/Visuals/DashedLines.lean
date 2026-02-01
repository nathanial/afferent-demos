/-
  Demo Runner - Canopy app visuals DashedLines tab content.
-/
import Reactive
import Afferent
import Afferent.Canopy
import Afferent.Canopy.Reactive
import Demos.Core.Demo
import Demos.Visuals.DashedLines
import Trellis

open Reactive Reactive.Host
open Afferent
open Afferent.Arbor
open Afferent.Canopy
open Afferent.Canopy.Reactive
open Trellis

namespace Demos
def dashedLinesTabContent (env : DemoEnv) (elapsedTime : Dynamic Spider Float) : WidgetM Unit := do
  let _ ← dynWidget elapsedTime fun _ => do
    emit (pure (dashedLinesWidget env.screenScale env.fontSmall env.fontMedium))
  pure ()

end Demos
