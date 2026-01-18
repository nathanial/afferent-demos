/-
  WidgetPerf - Diagnostic page to isolate widget performance.
  Select a widget type from the list to see 1000 instances in a grid.
-/
import Reactive
import Afferent
import Afferent.Canopy
import Afferent.Canopy.Reactive
import Demos.Core.Demo

open Reactive Reactive.Host
open Afferent CanvasM
open Afferent.Arbor
open Afferent.Canopy
open Afferent.Canopy.Reactive
open Trellis

namespace Demos.WidgetPerf

/-- Widget types we can test. -/
inductive WidgetType
  -- Simple
  | label | caption | spacer | panel
  -- Controls
  | button | checkbox | switch | radioGroup
  | slider | stepper | progressBar | progressIndeterminate
  | dropdown
  -- Spinners (standard)
  | spinnerCircleDots | spinnerRing | spinnerBouncingDots
  | spinnerBars | spinnerDualRing
  -- Spinners (creative)
  | spinnerOrbit | spinnerPulse | spinnerHelix | spinnerWave | spinnerSpiral
  | spinnerClock | spinnerPendulum | spinnerRipple | spinnerHeartbeat | spinnerGears
  -- Charts
  | barChart | lineChart | areaChart
  | pieChart | donutChart
  | scatterPlot | horizontalBarChart | bubbleChart
  | histogram | boxPlot | heatmap
  | stackedBarChart | groupedBarChart | stackedAreaChart
  | radarChart | candlestickChart | waterfallChart
  | gaugeChart | funnelChart | treemapChart | sankeyDiagram
  -- Mixed
  | mixed
deriving BEq, Repr

def WidgetType.name : WidgetType → String
  | .label => "Label"
  | .caption => "Caption"
  | .spacer => "Spacer"
  | .panel => "Panel"
  | .button => "Button"
  | .checkbox => "Checkbox"
  | .switch => "Switch"
  | .radioGroup => "Radio Group"
  | .slider => "Slider"
  | .stepper => "Stepper"
  | .progressBar => "Progress Bar"
  | .progressIndeterminate => "Progress (Indeterminate)"
  | .dropdown => "Dropdown"
  -- Spinners (standard)
  | .spinnerCircleDots => "Spinner: Circle Dots"
  | .spinnerRing => "Spinner: Ring"
  | .spinnerBouncingDots => "Spinner: Bouncing Dots"
  | .spinnerBars => "Spinner: Bars"
  | .spinnerDualRing => "Spinner: Dual Ring"
  -- Spinners (creative)
  | .spinnerOrbit => "Spinner: Orbit"
  | .spinnerPulse => "Spinner: Pulse"
  | .spinnerHelix => "Spinner: Helix"
  | .spinnerWave => "Spinner: Wave"
  | .spinnerSpiral => "Spinner: Spiral"
  | .spinnerClock => "Spinner: Clock"
  | .spinnerPendulum => "Spinner: Pendulum"
  | .spinnerRipple => "Spinner: Ripple"
  | .spinnerHeartbeat => "Spinner: Heartbeat"
  | .spinnerGears => "Spinner: Gears"
  -- Charts
  | .barChart => "Bar Chart"
  | .lineChart => "Line Chart"
  | .areaChart => "Area Chart"
  | .pieChart => "Pie Chart"
  | .donutChart => "Donut Chart"
  | .scatterPlot => "Scatter Plot"
  | .horizontalBarChart => "Horizontal Bar"
  | .bubbleChart => "Bubble Chart"
  | .histogram => "Histogram"
  | .boxPlot => "Box Plot"
  | .heatmap => "Heatmap"
  | .stackedBarChart => "Stacked Bar"
  | .groupedBarChart => "Grouped Bar"
  | .stackedAreaChart => "Stacked Area"
  | .radarChart => "Radar Chart"
  | .candlestickChart => "Candlestick"
  | .waterfallChart => "Waterfall"
  | .gaugeChart => "Gauge"
  | .funnelChart => "Funnel"
  | .treemapChart => "Treemap"
  | .sankeyDiagram => "Sankey"
  | .mixed => "Mixed (All)"

/-- All widget types except mixed (the renderable set for the mixed grid). -/
def renderableWidgetTypes : Array WidgetType := #[
  .label, .caption, .spacer, .panel,
  .button, .checkbox, .switch, .radioGroup,
  .slider, .stepper, .progressBar, .progressIndeterminate,
  .dropdown,
  -- Spinners (standard)
  .spinnerCircleDots, .spinnerRing, .spinnerBouncingDots,
  .spinnerBars, .spinnerDualRing,
  -- Spinners (creative)
  .spinnerOrbit, .spinnerPulse, .spinnerHelix, .spinnerWave, .spinnerSpiral,
  .spinnerClock, .spinnerPendulum, .spinnerRipple, .spinnerHeartbeat, .spinnerGears,
  -- Charts
  .barChart, .lineChart, .areaChart,
  .pieChart, .donutChart,
  .scatterPlot, .horizontalBarChart, .bubbleChart,
  .histogram, .boxPlot, .heatmap,
  .stackedBarChart, .groupedBarChart, .stackedAreaChart,
  .radarChart, .candlestickChart, .waterfallChart,
  .gaugeChart, .funnelChart, .treemapChart, .sankeyDiagram
]

def allWidgetTypes : Array WidgetType := #[
  .label, .caption, .spacer, .panel,
  .button, .checkbox, .switch, .radioGroup,
  .slider, .stepper, .progressBar, .progressIndeterminate,
  .dropdown,
  -- Spinners (standard)
  .spinnerCircleDots, .spinnerRing, .spinnerBouncingDots,
  .spinnerBars, .spinnerDualRing,
  -- Spinners (creative)
  .spinnerOrbit, .spinnerPulse, .spinnerHelix, .spinnerWave, .spinnerSpiral,
  .spinnerClock, .spinnerPendulum, .spinnerRipple, .spinnerHeartbeat, .spinnerGears,
  -- Charts
  .barChart, .lineChart, .areaChart,
  .pieChart, .donutChart,
  .scatterPlot, .horizontalBarChart, .bubbleChart,
  .histogram, .boxPlot, .heatmap,
  .stackedBarChart, .groupedBarChart, .stackedAreaChart,
  .radarChart, .candlestickChart, .waterfallChart,
  .gaugeChart, .funnelChart, .treemapChart, .sankeyDiagram,
  -- Mixed
  .mixed
]

def widgetTypeNames : Array String := allWidgetTypes.map WidgetType.name

/-- Sample data for charts. -/
def sampleBarData : Array Float := #[42.0, 78.0, 56.0, 91.0]
def sampleBarLabels : Array String := #["Q1", "Q2", "Q3", "Q4"]

def sampleSankeyData : SankeyDiagram.Data :=
  let nodes : Array SankeyDiagram.Node := #[
    { id := "a", label := "A", column := 0 },
    { id := "b", label := "B", column := 0 },
    { id := "c", label := "C", column := 1 },
    { id := "d", label := "D", column := 2 },
    { id := "e", label := "E", column := 2 }
  ]
  let links : Array SankeyDiagram.Link := #[
    { source := "a", target := "c", value := 50 },
    { source := "b", target := "c", value := 30 },
    { source := "c", target := "d", value := 45 },
    { source := "c", target := "e", value := 35 }
  ]
  { nodes, links }

/-- Render a single widget of the given type, wrapped in a growing cell. -/
def renderWidget (wtype : WidgetType) (theme : Theme) (index : Nat) : WidgetM Unit := do
  let cellStyle : BoxStyle := {
    flexItem := some (FlexItem.growing 1)
    width := .percent 1.0
    height := .percent 1.0
  }
  column' (gap := 0) (style := cellStyle) do match wtype with
  -- Simple
  | .label => heading3' s!"Label {index}" theme
  | .caption => caption' s!"Caption {index}" theme
  | .spacer => spacer' 80 30
  | .panel =>
    outlinedPanel' theme 6 do
      caption' s!"Panel {index}" theme

  -- Controls
  | .button =>
    let _ ← button s!"Btn {index}" theme .primary
    pure ()
  | .checkbox =>
    let _ ← checkbox s!"Chk {index}" theme false
    pure ()
  | .switch =>
    let _ ← switch none theme false
    pure ()
  | .radioGroup =>
    let opts : Array RadioOption := #[{ label := "A", value := "a" }, { label := "B", value := "b" }]
    let _ ← radioGroup opts theme "a"
    pure ()
  | .slider =>
    let _ ← slider none theme 0.5
    pure ()
  | .stepper =>
    let config : StepperConfig := { min := 0, max := 10, step := 1, width := 80 }
    let _ ← stepper theme 5 config
    pure ()
  | .progressBar =>
    let _ ← progressBar theme 0.65 .primary none false
    pure ()
  | .progressIndeterminate =>
    let _ ← progressBarIndeterminate theme .primary none
    pure ()
  | .dropdown =>
    let opts := #["Apple", "Banana", "Cherry"]
    let _ ← dropdown opts theme 0
    pure ()

  -- Spinners (standard)
  | .spinnerCircleDots =>
    let _ ← spinner theme { variant := .circleDots, dims := { size := 50 } }
    pure ()
  | .spinnerRing =>
    let _ ← spinner theme { variant := .ring, dims := { size := 50 } }
    pure ()
  | .spinnerBouncingDots =>
    let _ ← spinner theme { variant := .bouncingDots, dims := { size := 50 } }
    pure ()
  | .spinnerBars =>
    let _ ← spinner theme { variant := .bars, dims := { size := 50 } }
    pure ()
  | .spinnerDualRing =>
    let _ ← spinner theme { variant := .dualRing, dims := { size := 50 } }
    pure ()

  -- Spinners (creative)
  | .spinnerOrbit =>
    let _ ← spinner theme { variant := .orbit, dims := { size := 50 } }
    pure ()
  | .spinnerPulse =>
    let _ ← spinner theme { variant := .pulse, dims := { size := 50 } }
    pure ()
  | .spinnerHelix =>
    let _ ← spinner theme { variant := .helix, dims := { size := 50 } }
    pure ()
  | .spinnerWave =>
    let _ ← spinner theme { variant := .wave, dims := { size := 50 } }
    pure ()
  | .spinnerSpiral =>
    let _ ← spinner theme { variant := .spiral, dims := { size := 50 } }
    pure ()
  | .spinnerClock =>
    let _ ← spinner theme { variant := .clock, dims := { size := 50 } }
    pure ()
  | .spinnerPendulum =>
    let _ ← spinner theme { variant := .pendulum, dims := { size := 50 } }
    pure ()
  | .spinnerRipple =>
    let _ ← spinner theme { variant := .ripple, dims := { size := 50 } }
    pure ()
  | .spinnerHeartbeat =>
    let _ ← spinner theme { variant := .heartbeat, dims := { size := 50 } }
    pure ()
  | .spinnerGears =>
    let _ ← spinner theme { variant := .gears, dims := { size := 50 } }
    pure ()

  -- Charts
  | .barChart =>
    let dims : BarChart.Dimensions := { width := 140, height := 80, marginLeft := 25 }
    let dataDyn ← Dynamic.pureM sampleBarData
    let _ ← barChart dataDyn sampleBarLabels theme .primary dims
    pure ()
  | .lineChart =>
    let dims : LineChart.Dimensions := { width := 140, height := 80, marginLeft := 25 }
    let dataDyn ← Dynamic.pureM sampleBarData
    let _ ← lineChart dataDyn sampleBarLabels theme .primary dims
    pure ()
  | .areaChart =>
    let dims : AreaChart.Dimensions := { width := 140, height := 80, marginLeft := 25 }
    let dataDyn ← Dynamic.pureM sampleBarData
    let _ ← areaChart dataDyn sampleBarLabels theme .primary dims
    pure ()
  | .pieChart =>
    let slices : Array PieChart.Slice := #[
      { value := 40.0, label := some "A" },
      { value := 30.0, label := some "B" },
      { value := 30.0, label := some "C" }
    ]
    let dims : PieChart.Dimensions := { width := 100, height := 80, radius := 30, showLabels := false }
    let slicesDyn ← Dynamic.pureM slices
    let _ ← pieChart slicesDyn theme dims
    pure ()
  | .donutChart =>
    let slices : Array DonutChart.Slice := #[
      { value := 40.0, label := some "A" },
      { value := 30.0, label := some "B" },
      { value := 30.0, label := some "C" }
    ]
    let dims : DonutChart.Dimensions := { width := 100, height := 80, outerRadius := 30, innerRadius := 18, showLabels := false }
    let slicesDyn ← Dynamic.pureM slices
    let _ ← donutChart slicesDyn theme dims
    pure ()
  | .scatterPlot =>
    let points : Array ScatterPlot.DataPoint := #[
      { x := 10, y := 20 }, { x := 30, y := 40 }, { x := 50, y := 30 },
      { x := 70, y := 60 }, { x := 90, y := 50 }
    ]
    let dims : ScatterPlot.Dimensions := { width := 140, height := 80, marginLeft := 25 }
    let pointsDyn ← Dynamic.pureM points
    let _ ← scatterPlot pointsDyn theme dims
    pure ()
  | .horizontalBarChart =>
    let dims : HorizontalBarChart.Dimensions := { width := 140, height := 80, marginLeft := 35 }
    let dataDyn ← Dynamic.pureM sampleBarData
    let _ ← horizontalBarChart dataDyn sampleBarLabels theme .primary dims
    pure ()
  | .bubbleChart =>
    let points : Array BubbleChart.DataPoint := #[
      { x := 20, y := 30, size := 5 }, { x := 50, y := 60, size := 8 }, { x := 80, y := 40, size := 6 }
    ]
    let dims : BubbleChart.Dimensions := { width := 140, height := 80, marginLeft := 25, minBubbleRadius := 3, maxBubbleRadius := 12 }
    let pointsDyn ← Dynamic.pureM points
    let _ ← bubbleChart pointsDyn theme dims
    pure ()
  | .histogram =>
    let data : Array Float := #[10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80]
    let dims : Histogram.Dimensions := { width := 140, height := 80, marginLeft := 25 }
    let dataDyn ← Dynamic.pureM data
    let _ ← histogram dataDyn theme .primary dims {}
    pure ()
  | .boxPlot =>
    let data := #[#[10.0, 20.0, 30.0, 40.0, 50.0], #[15.0, 25.0, 35.0, 45.0, 55.0]]
    let dims : BoxPlot.Dimensions := { width := 140, height := 80, marginLeft := 25, boxWidth := 25 }
    let dataDyn ← Dynamic.pureM data
    let _ ← boxPlotFromData dataDyn #["A", "B"] theme dims
    pure ()
  | .heatmap =>
    let values : Array (Array Float) := #[#[1.0, 0.5, 0.2], #[0.5, 1.0, 0.7], #[0.2, 0.7, 1.0]]
    let dims : Heatmap.Dimensions := { width := 100, height := 80, marginLeft := 15, marginTop := 15, showValues := false }
    let valuesDyn ← Dynamic.pureM values
    let _ ← correlationMatrix valuesDyn #["A", "B", "C"] theme dims
    pure ()
  | .stackedBarChart =>
    let data : StackedBarChart.Data := {
      categories := #["Q1", "Q2"]
      series := #[{ name := "A", values := #[30.0, 40.0] }, { name := "B", values := #[20.0, 30.0] }]
    }
    let dims : StackedBarChart.Dimensions := { width := 140, height := 80, marginLeft := 25 }
    let dataDyn ← Dynamic.pureM data
    let _ ← stackedBarChart dataDyn theme dims
    pure ()
  | .groupedBarChart =>
    let data : GroupedBarChart.Data := {
      categories := #["Q1", "Q2"]
      series := #[{ name := "A", values := #[30.0, 40.0] }, { name := "B", values := #[20.0, 30.0] }]
    }
    let dims : GroupedBarChart.Dimensions := { width := 140, height := 80, marginLeft := 25 }
    let dataDyn ← Dynamic.pureM data
    let _ ← groupedBarChart dataDyn theme dims
    pure ()
  | .stackedAreaChart =>
    let data : StackedAreaChart.Data := {
      labels := #["A", "B", "C"]
      series := #[{ name := "X", values := #[10.0, 20.0, 15.0] }, { name := "Y", values := #[20.0, 15.0, 25.0] }]
    }
    let dims : StackedAreaChart.Dimensions := { width := 140, height := 80, marginLeft := 25 }
    let dataDyn ← Dynamic.pureM data
    let _ ← stackedAreaChart dataDyn theme dims
    pure ()
  | .radarChart =>
    let data : RadarChart.Data := {
      axisLabels := #["A", "B", "C", "D", "E"]
      series := #[{ name := "X", values := #[80.0, 70.0, 90.0, 60.0, 75.0] }]
    }
    let dims : RadarChart.Dimensions := { width := 120, height := 100, radius := 40 }
    let dataDyn ← Dynamic.pureM data
    let _ ← radarChart dataDyn theme dims
    pure ()
  | .candlestickChart =>
    let candles : Array CandlestickChart.Candle := #[
      { openPrice := 100.0, highPrice := 105.0, lowPrice := 98.0, closePrice := 103.0, label := some "1" },
      { openPrice := 103.0, highPrice := 108.0, lowPrice := 101.0, closePrice := 106.0, label := some "2" },
      { openPrice := 106.0, highPrice := 110.0, lowPrice := 104.0, closePrice := 105.0, label := some "3" }
    ]
    let dims : CandlestickChart.Dimensions := { width := 140, height := 80, marginLeft := 30 }
    let dataDyn ← Dynamic.pureM ({ candles } : CandlestickChart.Data)
    let _ ← candlestickChart dataDyn theme CandlestickChart.defaultColors dims
    pure ()
  | .waterfallChart =>
    let bars : Array WaterfallChart.Bar := #[
      { label := "Start", value := 100.0, barType := .initial },
      { label := "Add", value := 30.0, barType := .increase },
      { label := "Sub", value := -20.0, barType := .decrease },
      { label := "End", value := 110.0, barType := .total }
    ]
    let dims : WaterfallChart.Dimensions := { width := 140, height := 80, marginLeft := 30 }
    let dataDyn ← Dynamic.pureM ({ bars } : WaterfallChart.Data)
    let _ ← waterfallChart dataDyn theme WaterfallChart.defaultColors dims
    pure ()
  | .gaugeChart =>
    let data : GaugeChart.Data := {
      value := 72.0, minValue := 0.0, maxValue := 100.0
      segments := #[
        { startFrac := 0.0, endFrac := 0.6, color := Color.rgba 0.20 0.69 0.35 1.0 },
        { startFrac := 0.6, endFrac := 0.8, color := Color.rgba 0.95 0.75 0.10 1.0 },
        { startFrac := 0.8, endFrac := 1.0, color := Color.rgba 0.90 0.25 0.20 1.0 }
      ]
    }
    let dims : GaugeChart.Dimensions := { width := 100, height := 70, radius := 30 }
    let dataDyn ← Dynamic.pureM data
    let _ ← gaugeChart dataDyn theme GaugeChart.defaultColors dims
    pure ()
  | .funnelChart =>
    let stages : Array FunnelChart.Stage := #[
      { label := "A", value := 100.0 }, { label := "B", value := 60.0 }, { label := "C", value := 30.0 }
    ]
    let dims : FunnelChart.Dimensions := { width := 140, height := 80, marginRight := 45 }
    let dataDyn ← Dynamic.pureM ({ stages } : FunnelChart.Data)
    let _ ← funnelChart dataDyn theme dims
    pure ()
  | .treemapChart =>
    let nodes : Array TreemapChart.TreeNode := #[
      { label := "A", value := 40 }, { label := "B", value := 30 },
      { label := "C", value := 20 }, { label := "D", value := 10 }
    ]
    let dims : TreemapChart.Dimensions := { width := 120, height := 80 }
    let dataDyn ← Dynamic.pureM ({ nodes } : TreemapChart.Data)
    let _ ← treemapChart dataDyn theme dims
    pure ()
  | .sankeyDiagram =>
    let dims : SankeyDiagram.Dimensions := {
      width := 160, height := 80
      marginLeft := 5, marginRight := 40
      marginTop := 5, marginBottom := 5
      nodeWidth := 6
      showLabels := true
      showValues := false
    }
    let dataDyn ← Dynamic.pureM sampleSankeyData
    let _ ← sankeyDiagram dataDyn theme dims
    pure ()
  | .mixed =>
    -- Mixed is handled at the grid level, not individual widget level
    caption' s!"Mixed {index}" theme

/-- Render a mixed grid with 10 instances of each widget type, interleaved. -/
def renderMixedGrid (theme : Theme) : WidgetM Unit := do
  let gridStyle : BoxStyle := {
    flexItem := some (FlexItem.growing 1)
    width := .percent 1.0
  }
  let rowStyle : BoxStyle := {
    flexItem := some (FlexItem.growing 1)
    width := .percent 1.0
  }
  let numTypes := renderableWidgetTypes.size
  let totalWidgets := numTypes * 10  -- 10 of each type
  let numCols := 47  -- One of each type per row
  let numRows := 10  -- 10 instances of each
  column' (gap := 6) (style := gridStyle) do
    heading3' s!"Mixed Grid ({numTypes} types × 10 = {totalWidgets} instances)" theme
    for row in [:numRows] do
      row' (gap := 6) (style := rowStyle) do
        for col in [:numCols] do
          let wtype := renderableWidgetTypes.getD col .label
          let index := row * numCols + col
          renderWidget wtype theme index

/-- Render a grid of widgets for a given type. -/
def renderWidgetGrid (wtype : WidgetType) (theme : Theme) : WidgetM Unit := do
  if wtype == .mixed then
    renderMixedGrid theme
  else
    let gridStyle : BoxStyle := {
      flexItem := some (FlexItem.growing 1)
      width := .percent 1.0
    }
    let rowStyle : BoxStyle := {
      flexItem := some (FlexItem.growing 1)
      width := .percent 1.0
    }
    column' (gap := 6) (style := gridStyle) do
      heading3' s!"Grid of {wtype.name} (1000 instances)" theme
      for row in [0:25] do
        row' (gap := 6) (style := rowStyle) do
          for col in [0:40] do
            let index := row * 40 + col
            renderWidget wtype theme index

/-- Application state. -/
structure AppState where
  render : ComponentRender

/-- Create the widget performance test application. -/
def createApp (env : DemoEnv) : ReactiveM AppState := do
  let theme : Theme := { Theme.dark with font := env.fontCanopyId, smallFont := env.fontCanopySmallId }

  -- Pre-create a Dynamic for the selected widget type
  let (selectionEvent, fireSelection) ← Reactive.newTriggerEvent (t := Spider) (a := Nat)
  let selectedType ← Reactive.holdDyn 0 selectionEvent

  let (_, render) ← runWidget do
    let rootStyle : BoxStyle := {
      backgroundColor := some (Color.gray 0.1)
      padding := EdgeInsets.uniform 16
      width := .percent 1.0
      height := .percent 1.0
      flexItem := some (FlexItem.growing 1)
    }

    column' (gap := 16) (style := rootStyle) do
      heading1' "Widget Performance Test" theme
      caption' "Select a widget type to render 1000 instances" theme

      -- Main content row (fills remaining space)
      let contentRowStyle : BoxStyle := {
        flexItem := some (FlexItem.growing 1)
        width := .percent 1.0
        height := .percent 1.0
      }
      flexRow' { FlexContainer.row 16 with alignItems := .stretch }
          (style := contentRowStyle) do
        -- Left panel: widget selector (fixed width, fills height)
        let leftPanelStyle : BoxStyle := {
          minWidth := some 180
          height := .percent 1.0
        }
        column' (gap := 8) (style := leftPanelStyle) do
          caption' "Widget type:" theme

          let result ← listBox widgetTypeNames theme { fillHeight := true }

          -- Wire selection to the external Dynamic
          let selAction ← Event.mapM (fun idx => fireSelection idx) result.onSelect
          performEvent_ selAction

          -- Show current selection
          let _ ← dynWidget selectedType (fun sel =>
            caption' s!"Selected: {widgetTypeNames.getD sel "none"}" theme)
          pure ()

        -- Right panel: Grid of selected widget type (fills remaining space)
        let rightPanelStyle : BoxStyle := {
          flexItem := some (FlexItem.growing 1)
          width := .percent 1.0
          height := .percent 1.0
        }
        column' (gap := 0) (style := rightPanelStyle) do
          let _ ← dynWidget selectedType (fun selIdx => do
            let wtype := allWidgetTypes.getD selIdx .label
            renderWidgetGrid wtype theme)
          pure ()

  pure { render }

end Demos.WidgetPerf
