/-
  SankeyGrid - Performance test page with multiple Sankey diagrams.
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

namespace Demos.SankeyGrid

/-- Sample Sankey data for testing. -/
def sampleSankeyData : SankeyDiagram.Data :=
  let nodes : Array SankeyDiagram.Node := #[
    { id := "a", label := "Source A", column := 0 },
    { id := "b", label := "Source B", column := 0 },
    { id := "c", label := "Middle", column := 1 },
    { id := "d", label := "End X", column := 2 },
    { id := "e", label := "End Y", column := 2 }
  ]
  let links : Array SankeyDiagram.Link := #[
    { source := "a", target := "c", value := 50 },
    { source := "b", target := "c", value := 30 },
    { source := "c", target := "d", value := 45 },
    { source := "c", target := "e", value := 35 }
  ]
  { nodes, links }

/-- Create a single Sankey panel. -/
def sankeyPanel (theme : Theme) (index : Nat) : WidgetM Unit :=
  titledPanel' s!"Sankey {index}" .outlined theme do
    let dims : SankeyDiagram.Dimensions := {
      width := 200, height := 100
      marginLeft := 8, marginRight := 50
      marginTop := 8, marginBottom := 8
      nodeWidth := 8
      showLabels := true
      showValues := false
    }
    let _ ← sankeyDiagram sampleSankeyData theme dims
    pure ()

/-- Application state. -/
structure AppState where
  render : ComponentRender

/-- Create the Sankey grid test application. -/
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
      heading1' "Sankey Grid Performance Test" theme
      caption' "Multiple Sankey diagrams to test FRP caching performance" theme

      -- Create a grid of Sankey charts
      -- 7 rows x 8 columns = 56 charts (to match ~51 panels in ReactiveShowcase)
      for row in [0:7] do
        row' (gap := 12) (style := {}) do
          for col in [0:8] do
            let index := row * 8 + col
            sankeyPanel theme index

  pure { render }

end Demos.SankeyGrid
