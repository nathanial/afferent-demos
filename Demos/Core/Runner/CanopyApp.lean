/-
  Demo Runner - Canopy app shell for demo tabs.
-/
import Reactive
import Afferent
import Afferent.Canopy
import Afferent.Canopy.Reactive
import Demos.Core.Demo
import Demos.Core.DemoRegistry
import Demos.Overview.Card
import Demos.Overview.DemoGrid
import Demos.Overview.SpinningCubes
import Demos.Perf.Grid
import Trellis

open Reactive Reactive.Host
open Afferent
open Afferent.Arbor
open Afferent.Canopy
open Afferent.Canopy.Reactive
open Trellis

namespace Demos

structure CanopyAppState where
  render : ComponentRender

private def demoFontsFromEnv (env : DemoEnv) : DemoFonts := {
  label := env.fontSmallId
  small := env.fontSmallId
  medium := env.fontMediumId
  large := env.fontLargeId
  huge := env.fontHugeId
}

private def overviewTabContent (env : DemoEnv) (elapsedTime : Dynamic Spider Float) : WidgetM Unit := do
  let demoFonts := demoFontsFromEnv env
  let cubes := spinningCubesInitialState
  let _ ← dynWidget elapsedTime fun t => do
    emit (pure (demoGridWidget env.screenScale t demoFonts cubes env.windowWidthF env.windowHeightF))
  pure ()

private def gridTabContent (env : DemoEnv) (elapsedTime : Dynamic Spider Float) : WidgetM Unit := do
  let _ ← dynWidget elapsedTime fun t => do
    emit (pure (gridPerfWidget t env.fontMedium env.gridParticles env.halfSize))
  pure ()

private def demoStubContent (id : DemoId) : WidgetM Unit := do
  let inst := demoInstance id
  filledPanel' 24 do
    heading2' inst.name
    caption' s!"{inst.shortName} • stub"
    spacer' 0 8
    bodyText' "This demo has not been migrated to Canopy yet."
    bodyText' "Replace this panel with the real demo widget."

/-- Create the demo shell as a single Canopy widget tree. -/
def createCanopyApp (env : DemoEnv) : ReactiveM CanopyAppState := do
  let elapsedTime ← useElapsedTime
  let tabs : Array TabDef := demoIds.map fun id => {
    label := (demoInstance id).shortName
    content := match id with
      | .demoGrid => overviewTabContent env elapsedTime
      | .gridPerf => gridTabContent env elapsedTime
      | _ => demoStubContent id
  }

  let (_, render) ← runWidget do
    let rootStyle : BoxStyle := {
      backgroundColor := some (Color.gray 0.08)
      padding := EdgeInsets.uniform 16
      width := .percent 1.0
      height := .percent 1.0
      flexItem := some (FlexItem.growing 1)
    }

    column' (gap := 16) (style := rootStyle) do
      heading1' "Afferent Demos"

      let contentStyle : BoxStyle := {
        flexItem := some (FlexItem.growing 1)
        width := .percent 1.0
        height := .percent 1.0
      }

      column' (gap := 0) (style := contentStyle) do
        let _ ← tabView tabs 0
        pure ()

  pure { render := render }

end Demos
