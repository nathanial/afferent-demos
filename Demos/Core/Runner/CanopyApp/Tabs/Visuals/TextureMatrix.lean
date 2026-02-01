/-
  Demo Runner - Canopy app visuals TextureMatrix tab content.
-/
import Reactive
import Afferent
import Afferent.Canopy
import Afferent.Canopy.Reactive
import Demos.Core.Demo
import Demos.Visuals.TextureMatrix
import Trellis

open Reactive Reactive.Host
open Afferent
open Afferent.Arbor
open Afferent.Canopy
open Afferent.Canopy.Reactive
open Trellis

namespace Demos
def textureMatrixTabContent (env : DemoEnv) (elapsedTime : Dynamic Spider Float) : WidgetM Unit := do
  let _ ← dynWidget elapsedTime fun t => do
    emit (pure (textureMatrixWidget t env.screenScale env.windowWidthF env.windowHeightF
      env.fontMedium env.fontSmall env.spriteTexture))
  pure ()

end Demos
