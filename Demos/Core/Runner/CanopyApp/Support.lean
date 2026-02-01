/-
  Demo Runner - Canopy app helpers.
-/
import Reactive
import Afferent
import Afferent.Canopy
import Afferent.Canopy.Reactive
import Demos.Core.Demo
import Trellis

open Reactive Reactive.Host
open Afferent
open Afferent.Arbor
open Afferent.Canopy
open Afferent.Canopy.Reactive
open Trellis

namespace Demos

private def roundTo (v : Float) (places : Nat) : Float :=
  let factor := (10 : Float) ^ places.toFloat
  (v * factor).round / factor

private def formatFloat (v : Float) (places : Nat := 2) : String :=
  let s := toString (roundTo v places)
  if s.any (· == '.') then
    let s := s.dropRightWhile (· == '0')
    if s.endsWith "." then s.dropRight 1 else s
  else
    s

private def formatStatsLines (stats : RunnerStats) : Array String :=
  #[
    s!"layout {formatFloat stats.layoutMs}ms • collect {formatFloat stats.collectMs}ms • exec {formatFloat stats.executeMs}ms",
    s!"cmds {stats.commandCount} • widgets {stats.widgetCount} • layouts {stats.layoutCount}",
    s!"draws {stats.drawCalls} • batched {stats.batchedCalls} • single {stats.individualCalls}",
    s!"cache hits {stats.cacheHits} • misses {stats.cacheMisses}",
    s!"frame {formatFloat stats.frameMs}ms • {formatFloat stats.fps 1} fps"
  ]

/-- Show frame stats under the tab content. -/
def statsFooter (env : DemoEnv) (elapsedTime : Dynamic Spider Float) : WidgetM Unit := do
  let footerHeight := 110.0 * env.screenScale
  let footerStyle : BoxStyle := {
    backgroundColor := some (Color.gray 0.08)
    padding := EdgeInsets.symmetric (6.0 * env.screenScale) (4.0 * env.screenScale)
    width := .percent 1.0
    height := .length footerHeight
    flexItem := some (FlexItem.fixed footerHeight)
  }
  column' (gap := 2.0 * env.screenScale) (style := footerStyle) do
    let _ ← dynWidget elapsedTime fun _ => do
      let stats ← SpiderM.liftIO env.statsRef.get
      let lines := formatStatsLines stats
      for line in lines do
        caption' line
      pure ()
    pure ()

end Demos
