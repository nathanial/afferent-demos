/-
  BarChartGrid - Performance test page with multiple Bar charts.
  Used to compare performance with SankeyGrid (which has FRP caching).
-/
import Reactive
import Afferent
import Afferent.Canopy
import Afferent.Canopy.Reactive
import Demos.Demo

open Reactive Reactive.Host
open Afferent CanvasM
open Afferent.Arbor
open Afferent.Canopy
open Afferent.Canopy.Reactive
open Trellis

namespace Demos.BarChartGrid

/-- Sample bar chart data for testing. -/
def sampleBarData : Array Float := #[42.0, 78.0, 56.0, 91.0]
def sampleBarLabels : Array String := #["Q1", "Q2", "Q3", "Q4"]

/-- Create a single bar chart panel. -/
def barChartPanel (theme : Theme) (index : Nat) : WidgetM Unit :=
  titledPanel' s!"Bar {index}" .outlined theme do
    let dims : BarChart.Dimensions := {
      width := 200, height := 100
      marginLeft := 30, marginRight := 10
      marginTop := 10, marginBottom := 20
    }
    let _ ← barChart sampleBarData sampleBarLabels theme .primary dims
    pure ()

/-- Application state. -/
structure AppState where
  render : ComponentRender

/-- Create the Bar Chart grid test application. -/
def createApp (env : DemoEnv) : ReactiveM AppState := do
  let theme : Theme := { Theme.dark with font := env.fontCanopyId, smallFont := env.fontCanopySmallId }

  let (_, render) ← runWidget do
    let rootStyle : BoxStyle := {
      backgroundColor := some (Color.gray 0.1)
      padding := EdgeInsets.uniform 24
      width := .percent 1.0
      height := .percent 1.0
      flexItem := some (FlexItem.growing 1)
    }

    column' (gap := 20) (style := rootStyle) do
      heading1' "Bar Chart Grid Performance Test" theme
      caption' "Multiple Bar charts (no FRP caching) for comparison" theme

      -- Wrap grid in a growing container so it fills remaining space
      let gridStyle : BoxStyle := {
        flexItem := some (FlexItem.growing 1)
        width := .percent 1.0
      }
      let rowStyle : BoxStyle := {
        flexItem := some (FlexItem.growing 1)
        width := .percent 1.0
      }
      column' (gap := 12) (style := gridStyle) do
        -- Create a grid of Bar charts
        -- 7 rows x 8 columns = 56 charts (to match ~51 panels in ReactiveShowcase)
        for row in [0:7] do
          row' (gap := 12) (style := rowStyle) do
            for col in [0:8] do
              let index := row * 8 + col
              barChartPanel theme index

  pure { render }

end Demos.BarChartGrid
