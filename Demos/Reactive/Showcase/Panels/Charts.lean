/-
  Chart Panels - All data visualization chart components.
-/
import Reactive
import Afferent
import Afferent.Canopy
import Afferent.Canopy.Reactive

open Reactive Reactive.Host
open Afferent CanvasM
open Afferent.Arbor
open Afferent.Canopy
open Afferent.Canopy.Reactive
open Trellis

namespace Demos.ReactiveShowcase

/-! ## Chart Dimension Constants -/

/-- Standard chart width for simple charts. -/
def standardWidth : Float := 280
/-- Medium chart width for charts with more data. -/
def mediumWidth : Float := 320
/-- Large chart width for complex charts. -/
def largeWidth : Float := 340
/-- Wide chart width for charts needing extra horizontal space. -/
def wideWidth : Float := 380
/-- Extra wide chart width for diagrams. -/
def extraWideWidth : Float := 480

/-- Standard chart height. -/
def standardHeight : Float := 180
/-- Medium chart height. -/
def mediumHeight : Float := 200
/-- Tall chart height. -/
def tallHeight : Float := 220
/-- Extra tall chart height. -/
def extraTallHeight : Float := 240

/-- Standard left margin. -/
def standardMarginLeft : Float := 40
/-- Medium left margin for longer labels. -/
def mediumMarginLeft : Float := 45
/-- Large left margin for wide labels. -/
def largeMarginLeft : Float := 50
/-- Extra large left margin for very wide labels. -/
def extraLargeMarginLeft : Float := 70

/-- Standard bottom margin. -/
def standardMarginBottom : Float := 35
/-- Large bottom margin for rotated labels. -/
def largeMarginBottom : Float := 45

/-- Standard radius for circular charts. -/
def standardRadius : Float := 70
/-- Large radius for radar charts. -/
def largeRadius : Float := 90

/-- BarChart panel - demonstrates bar chart visualization. -/
def barChartPanel (theme : Theme) : WidgetM Unit :=
  titledPanel' "Bar Chart" .outlined theme do
    caption' "Sales data by quarter:" theme
    let salesData := #[42.0, 78.0, 56.0, 91.0]
    let labels := #["Q1", "Q2", "Q3", "Q4"]
    let dims : BarChart.Dimensions := { width := standardWidth, height := standardHeight, marginLeft := standardMarginLeft }
    let dataDyn ← Dynamic.pureM salesData
    let _ ← barChart dataDyn labels theme .primary dims
    pure ()

/-- LineChart panel - demonstrates line chart visualization. -/
def lineChartPanel (theme : Theme) : WidgetM Unit :=
  titledPanel' "Line Chart" .outlined theme do
    caption' "Monthly revenue trend:" theme
    let revenueData := #[12.0, 19.0, 15.0, 25.0, 22.0, 30.0]
    let labels := #["Jan", "Feb", "Mar", "Apr", "May", "Jun"]
    let dims : LineChart.Dimensions := { width := standardWidth, height := standardHeight, marginLeft := standardMarginLeft }
    let dataDyn ← Dynamic.pureM revenueData
    let _ ← lineChart dataDyn labels theme .primary dims
    pure ()

/-- AreaChart panel - demonstrates area chart visualization. -/
def areaChartPanel (theme : Theme) : WidgetM Unit :=
  titledPanel' "Area Chart" .outlined theme do
    caption' "Website traffic over time:" theme
    let trafficData := #[120.0, 180.0, 150.0, 220.0, 190.0, 280.0, 250.0]
    let labels := #["Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"]
    let dims : AreaChart.Dimensions := { width := standardWidth, height := standardHeight, marginLeft := standardMarginLeft }
    let dataDyn ← Dynamic.pureM trafficData
    let _ ← areaChart dataDyn labels theme .primary dims
    pure ()

/-- PieChart panel - demonstrates pie chart visualization. -/
def pieChartPanel (theme : Theme) : WidgetM Unit :=
  titledPanel' "Pie Chart" .outlined theme do
    caption' "Market share by browser:" theme
    let slices : Array PieChart.Slice := #[
      { value := 65.0, label := some "Chrome" },
      { value := 19.0, label := some "Safari" },
      { value := 4.0, label := some "Firefox" },
      { value := 3.0, label := some "Edge" },
      { value := 9.0, label := some "Other" }
    ]
    let dims : PieChart.Dimensions := { width := standardWidth, height := mediumHeight, radius := standardRadius, showLabels := false }
    let slicesDyn ← Dynamic.pureM slices
    let _ ← pieChartWithLegend slicesDyn theme dims
    pure ()

/-- DonutChart panel - demonstrates donut chart visualization. -/
def donutChartPanel (theme : Theme) : WidgetM Unit :=
  titledPanel' "Donut Chart" .outlined theme do
    caption' "Expense breakdown:" theme
    let slices : Array DonutChart.Slice := #[
      { value := 35.0, label := some "Housing" },
      { value := 20.0, label := some "Food" },
      { value := 15.0, label := some "Transport" },
      { value := 10.0, label := some "Utilities" },
      { value := 20.0, label := some "Other" }
    ]
    let dims : DonutChart.Dimensions := {
      width := standardWidth, height := mediumHeight
      outerRadius := standardRadius, innerRadius := 40
      showLabels := false
      centerLabel := some "Total"
      centerValue := some "$3,500"
    }
    let slicesDyn ← Dynamic.pureM slices
    let _ ← donutChartWithLegend slicesDyn theme dims
    pure ()

/-- ScatterPlot panel - demonstrates scatter plot visualization. -/
def scatterPlotPanel (theme : Theme) : WidgetM Unit :=
  titledPanel' "Scatter Plot" .outlined theme do
    caption' "Height vs Weight correlation:" theme
    let points : Array ScatterPlot.DataPoint := #[
      { x := 160, y := 55 }, { x := 165, y := 62 }, { x := 170, y := 68 },
      { x := 172, y := 65 }, { x := 175, y := 72 }, { x := 178, y := 75 },
      { x := 180, y := 78 }, { x := 182, y := 82 }, { x := 168, y := 60 },
      { x := 174, y := 70 }, { x := 185, y := 88 }, { x := 163, y := 58 },
      { x := 177, y := 74 }, { x := 169, y := 64 }, { x := 183, y := 85 }
    ]
    let dims : ScatterPlot.Dimensions := { width := standardWidth, height := mediumHeight, marginLeft := mediumMarginLeft }
    let pointsDyn ← Dynamic.pureM points
    let _ ← scatterPlot pointsDyn theme dims
    pure ()

/-- HorizontalBarChart panel - demonstrates horizontal bar chart. -/
def horizontalBarChartPanel (theme : Theme) : WidgetM Unit :=
  titledPanel' "Horizontal Bar Chart" .outlined theme do
    caption' "Programming language popularity:" theme
    let data := #[85.0, 72.0, 58.0, 45.0, 38.0]
    let labels := #["Python", "JavaScript", "Java", "C++", "Go"]
    let dims : HorizontalBarChart.Dimensions := { width := standardWidth, height := standardHeight, marginLeft := extraLargeMarginLeft }
    let dataDyn ← Dynamic.pureM data
    let _ ← horizontalBarChart dataDyn labels theme .primary dims
    pure ()

/-- BubbleChart panel - demonstrates bubble chart visualization. -/
def bubbleChartPanel (theme : Theme) : WidgetM Unit :=
  titledPanel' "Bubble Chart" .outlined theme do
    caption' "Country stats (GDP vs Population):" theme
    -- GDP (x, trillions), Population (y, hundreds of millions), Size = land area
    let points : Array BubbleChart.DataPoint := #[
      { x := 25.5, y := 3.3, size := 9.8, label := some "USA" },
      { x := 17.9, y := 14.1, size := 9.6, label := some "China" },
      { x := 4.2, y := 1.3, size := 0.4, label := some "Japan" },
      { x := 4.1, y := 0.8, size := 0.4, label := some "Germany" },
      { x := 3.5, y := 14.2, size := 3.3, label := some "India" },
      { x := 3.1, y := 0.7, size := 0.2, label := some "UK" },
      { x := 2.8, y := 0.7, size := 0.6, label := some "France" },
      { x := 2.0, y := 2.1, size := 8.5, label := some "Brazil" },
      { x := 1.8, y := 1.5, size := 17.1, label := some "Russia" },
      { x := 1.7, y := 0.5, size := 10.0, label := some "Canada" }
    ]
    let dims : BubbleChart.Dimensions := {
      width := mediumWidth, height := tallHeight
      marginLeft := largeMarginLeft, marginBottom := 40
      minBubbleRadius := 6, maxBubbleRadius := 28
      showBubbleLabels := false
    }
    let pointsDyn ← Dynamic.pureM points
    let _ ← bubbleChart pointsDyn theme dims
    pure ()

/-- Histogram panel - demonstrates histogram visualization. -/
def histogramPanel (theme : Theme) : WidgetM Unit :=
  titledPanel' "Histogram" .outlined theme do
    caption' "Test score distribution:" theme
    -- Simulated test scores (roughly normal distribution)
    let scores : Array Float := #[
      62, 65, 67, 68, 70, 71, 72, 73, 74, 74,
      75, 75, 76, 76, 77, 77, 77, 78, 78, 78,
      79, 79, 80, 80, 80, 81, 81, 82, 82, 83,
      83, 84, 85, 86, 87, 88, 90, 92, 95, 98
    ]
    let dims : Histogram.Dimensions := {
      width := 300, height := standardHeight
      marginLeft := mediumMarginLeft, marginBottom := standardMarginBottom
    }
    let binConfig : Histogram.BinConfig := { binCount := some 8 }
    let scoresDyn ← Dynamic.pureM scores
    let _ ← histogram scoresDyn theme .primary dims binConfig
    pure ()

/-- BoxPlot panel - demonstrates box and whisker plot. -/
def boxPlotPanel (theme : Theme) : WidgetM Unit :=
  titledPanel' "Box Plot" .outlined theme do
    caption' "Salary distribution by department:" theme
    -- Simulated salary data for different departments (in thousands)
    let engineering := #[65, 72, 78, 82, 85, 88, 90, 92, 95, 98, 102, 105, 110, 115, 145]
    let sales := #[45, 52, 55, 58, 60, 62, 65, 68, 70, 72, 75, 78, 82, 95]
    let marketing := #[50, 55, 58, 60, 62, 65, 68, 70, 72, 75, 78, 80, 85]
    let labels := #["Eng", "Sales", "Mkt"]
    let dims : BoxPlot.Dimensions := {
      width := standardWidth, height := mediumHeight
      marginLeft := mediumMarginLeft, marginBottom := standardMarginBottom
      boxWidth := 50
    }
    let dataDyn ← Dynamic.pureM #[engineering, sales, marketing]
    let _ ← boxPlotFromData dataDyn labels theme dims
    pure ()

/-- Heatmap panel - demonstrates correlation matrix heatmap. -/
def heatmapPanel (theme : Theme) : WidgetM Unit :=
  titledPanel' "Heatmap" .outlined theme do
    caption' "Feature correlation matrix:" theme
    -- Simulated correlation matrix
    let values : Array (Array Float) := #[
      #[ 1.0,  0.8,  0.3, -0.2, -0.5],
      #[ 0.8,  1.0,  0.5,  0.1, -0.3],
      #[ 0.3,  0.5,  1.0,  0.6,  0.2],
      #[-0.2,  0.1,  0.6,  1.0,  0.7],
      #[-0.5, -0.3,  0.2,  0.7,  1.0]
    ]
    let labels := #["A", "B", "C", "D", "E"]
    let dims : Heatmap.Dimensions := {
      width := standardWidth, height := tallHeight
      marginLeft := 30, marginTop := 30
      marginBottom := 20, marginRight := 50
      showValues := true
    }
    let valuesDyn ← Dynamic.pureM values
    let _ ← correlationMatrix valuesDyn labels theme dims
    pure ()

/-- StackedBarChart panel - demonstrates stacked bar chart visualization. -/
def stackedBarChartPanel (theme : Theme) : WidgetM Unit :=
  titledPanel' "Stacked Bar Chart" .outlined theme do
    caption' "Quarterly revenue by product line:" theme
    let categories := #["Q1", "Q2", "Q3", "Q4"]
    let series : Array StackedBarChart.Series := #[
      { name := "Hardware", values := #[42.0, 48.0, 55.0, 61.0] },
      { name := "Software", values := #[28.0, 35.0, 42.0, 50.0] },
      { name := "Services", values := #[15.0, 18.0, 22.0, 28.0] }
    ]
    let data : StackedBarChart.Data := { categories, series }
    let dims : StackedBarChart.Dimensions := {
      width := largeWidth, height := tallHeight
      marginLeft := mediumMarginLeft, marginBottom := standardMarginBottom
    }
    let dataDyn ← Dynamic.pureM data
    let _ ← stackedBarChart dataDyn theme dims
    pure ()

/-- GroupedBarChart panel - demonstrates grouped bar chart visualization. -/
def groupedBarChartPanel (theme : Theme) : WidgetM Unit :=
  titledPanel' "Grouped Bar Chart" .outlined theme do
    caption' "Monthly sales by region:" theme
    let categories := #["Jan", "Feb", "Mar", "Apr"]
    let series : Array GroupedBarChart.Series := #[
      { name := "North", values := #[85.0, 92.0, 78.0, 95.0] },
      { name := "South", values := #[65.0, 70.0, 82.0, 75.0] },
      { name := "West", values := #[72.0, 68.0, 90.0, 88.0] }
    ]
    let data : GroupedBarChart.Data := { categories, series }
    let dims : GroupedBarChart.Dimensions := {
      width := largeWidth, height := tallHeight
      marginLeft := mediumMarginLeft, marginBottom := standardMarginBottom
    }
    let dataDyn ← Dynamic.pureM data
    let _ ← groupedBarChart dataDyn theme dims
    pure ()

/-- StackedAreaChart panel - demonstrates stacked area chart visualization. -/
def stackedAreaChartPanel (theme : Theme) : WidgetM Unit :=
  titledPanel' "Stacked Area Chart" .outlined theme do
    caption' "Website traffic sources over time:" theme
    let labels := #["Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"]
    let series : Array StackedAreaChart.Series := #[
      { name := "Direct", values := #[120.0, 132.0, 101.0, 134.0, 90.0, 230.0, 210.0] },
      { name := "Search", values := #[220.0, 182.0, 191.0, 234.0, 290.0, 330.0, 310.0] },
      { name := "Social", values := #[150.0, 232.0, 201.0, 154.0, 190.0, 330.0, 410.0] }
    ]
    let data : StackedAreaChart.Data := { labels, series }
    let dims : StackedAreaChart.Dimensions := {
      width := largeWidth, height := tallHeight
      marginLeft := mediumMarginLeft, marginBottom := standardMarginBottom
    }
    let dataDyn ← Dynamic.pureM data
    let _ ← stackedAreaChart dataDyn theme dims
    pure ()

/-- RadarChart panel - demonstrates radar/spider chart visualization. -/
def radarChartPanel (theme : Theme) : WidgetM Unit :=
  titledPanel' "Radar Chart" .outlined theme do
    caption' "Product comparison (features):" theme
    let axisLabels := #["Speed", "Reliability", "Comfort", "Safety", "Efficiency", "Price"]
    let series : Array RadarChart.Series := #[
      { name := "Product A", values := #[85.0, 90.0, 70.0, 95.0, 80.0, 60.0] },
      { name := "Product B", values := #[70.0, 75.0, 90.0, 80.0, 85.0, 90.0] }
    ]
    let data : RadarChart.Data := { axisLabels, series }
    let dims : RadarChart.Dimensions := {
      width := mediumWidth, height := 280
      radius := largeRadius
    }
    let dataDyn ← Dynamic.pureM data
    let _ ← radarChart dataDyn theme dims
    pure ()

/-- CandlestickChart panel - demonstrates financial OHLC chart. -/
def candlestickChartPanel (theme : Theme) : WidgetM Unit :=
  titledPanel' "Candlestick Chart" .outlined theme do
    caption' "Stock price movement (OHLC):" theme
    let candles : Array CandlestickChart.Candle := #[
      { openPrice := 100.0, highPrice := 105.0, lowPrice := 98.0, closePrice := 103.0, label := some "Mon" },
      { openPrice := 103.0, highPrice := 108.0, lowPrice := 101.0, closePrice := 106.0, label := some "Tue" },
      { openPrice := 106.0, highPrice := 110.0, lowPrice := 104.0, closePrice := 105.0, label := some "Wed" },
      { openPrice := 105.0, highPrice := 107.0, lowPrice := 99.0, closePrice := 101.0, label := some "Thu" },
      { openPrice := 101.0, highPrice := 104.0, lowPrice := 97.0, closePrice := 98.0, label := some "Fri" },
      { openPrice := 98.0, highPrice := 102.0, lowPrice := 96.0, closePrice := 100.0, label := some "Sat" },
      { openPrice := 100.0, highPrice := 106.0, lowPrice := 99.0, closePrice := 105.0, label := some "Sun" },
      { openPrice := 105.0, highPrice := 112.0, lowPrice := 104.0, closePrice := 110.0, label := some "Mon" },
      { openPrice := 110.0, highPrice := 115.0, lowPrice := 108.0, closePrice := 113.0, label := some "Tue" },
      { openPrice := 113.0, highPrice := 116.0, lowPrice := 109.0, closePrice := 111.0, label := some "Wed" }
    ]
    let data : CandlestickChart.Data := { candles }
    let dims : CandlestickChart.Dimensions := {
      width := 360, height := extraTallHeight
      marginLeft := largeMarginLeft, marginBottom := standardMarginBottom
    }
    let dataDyn ← Dynamic.pureM data
    let _ ← candlestickChart dataDyn theme CandlestickChart.defaultColors dims
    pure ()

/-- WaterfallChart panel - demonstrates cumulative effect chart. -/
def waterfallChartPanel (theme : Theme) : WidgetM Unit :=
  titledPanel' "Waterfall Chart" .outlined theme do
    caption' "Profit breakdown analysis:" theme
    let bars : Array WaterfallChart.Bar := #[
      { label := "Revenue", value := 500.0, barType := .initial },
      { label := "COGS", value := -180.0, barType := .decrease },
      { label := "Gross", value := 320.0, barType := .total },
      { label := "Marketing", value := -45.0, barType := .decrease },
      { label := "R&D", value := -65.0, barType := .decrease },
      { label := "Admin", value := -30.0, barType := .decrease },
      { label := "Other Inc", value := 20.0, barType := .increase },
      { label := "Net Profit", value := 200.0, barType := .total }
    ]
    let data : WaterfallChart.Data := { bars }
    let dims : WaterfallChart.Dimensions := {
      width := wideWidth, height := extraTallHeight
      marginLeft := largeMarginLeft, marginBottom := largeMarginBottom
    }
    let dataDyn ← Dynamic.pureM data
    let _ ← waterfallChart dataDyn theme WaterfallChart.defaultColors dims
    pure ()

/-- GaugeChart panel - demonstrates speedometer-style gauge. -/
def gaugeChartPanel (theme : Theme) : WidgetM Unit :=
  titledPanel' "Gauge Chart" .outlined theme do
    caption' "CPU usage indicator:" theme
    let segments : Array GaugeChart.Segment := #[
      { startFrac := 0.0, endFrac := 0.6, color := Color.rgba 0.20 0.69 0.35 1.0 },   -- Green
      { startFrac := 0.6, endFrac := 0.8, color := Color.rgba 0.95 0.75 0.10 1.0 },   -- Yellow
      { startFrac := 0.8, endFrac := 1.0, color := Color.rgba 0.90 0.25 0.20 1.0 }    -- Red
    ]
    let data : GaugeChart.Data := {
      value := 72.0
      minValue := 0.0
      maxValue := 100.0
      label := some "CPU Usage"
      unit := some "%"
      segments
    }
    let dims : GaugeChart.Dimensions := {
      width := 220, height := 160
      radius := standardRadius
    }
    let dataDyn ← Dynamic.pureM data
    let _ ← gaugeChart dataDyn theme GaugeChart.defaultColors dims
    pure ()

/-- FunnelChart panel - demonstrates sales pipeline funnel. -/
def funnelChartPanel (theme : Theme) : WidgetM Unit :=
  titledPanel' "Funnel Chart" .outlined theme do
    caption' "Sales conversion funnel:" theme
    let stages : Array FunnelChart.Stage := #[
      { label := "Visitors", value := 10000.0 },
      { label := "Leads", value := 5200.0 },
      { label := "Prospects", value := 2800.0 },
      { label := "Negotiations", value := 1400.0 },
      { label := "Sales", value := 680.0 }
    ]
    let data : FunnelChart.Data := { stages }
    let dims : FunnelChart.Dimensions := {
      width := largeWidth, height := tallHeight
      marginRight := 110
    }
    let dataDyn ← Dynamic.pureM data
    let _ ← funnelChart dataDyn theme dims
    pure ()

/-- TreemapChart panel - demonstrates hierarchical treemap visualization. -/
def treemapChartPanel (theme : Theme) : WidgetM Unit :=
  titledPanel' "Treemap Chart" .outlined theme do
    caption' "Disk usage by category:" theme
    let nodes : Array TreemapChart.TreeNode := #[
      { label := "Documents", value := 0, children := #[
          { label := "PDFs", value := 2500 },
          { label := "Word", value := 1800 },
          { label := "Spreadsheets", value := 1200 }
        ]
      },
      { label := "Media", value := 0, children := #[
          { label := "Photos", value := 4500 },
          { label := "Videos", value := 8000 },
          { label := "Music", value := 2200 }
        ]
      },
      { label := "Apps", value := 5500 },
      { label := "System", value := 3200 },
      { label := "Other", value := 1500 }
    ]
    let data : TreemapChart.Data := { nodes }
    let dims : TreemapChart.Dimensions := {
      width := wideWidth, height := 260
      maxDepth := 2
    }
    let dataDyn ← Dynamic.pureM data
    let _ ← treemapChart dataDyn theme dims
    pure ()

/-- SankeyDiagram panel - demonstrates flow diagram visualization. -/
def sankeyDiagramPanel (theme : Theme) : WidgetM Unit :=
  titledPanel' "Sankey Diagram" .outlined theme do
    caption' "Energy flow from sources to uses:" theme
    let nodes : Array SankeyDiagram.Node := #[
      -- Sources (column 0)
      { id := "coal", label := "Coal", column := 0 },
      { id := "gas", label := "Natural Gas", column := 0 },
      { id := "nuclear", label := "Nuclear", column := 0 },
      { id := "renewable", label := "Renewable", column := 0 },
      -- Conversion (column 1)
      { id := "electricity", label := "Electricity", column := 1 },
      { id := "heat", label := "Heat", column := 1 },
      -- End uses (column 2)
      { id := "residential", label := "Residential", column := 2 },
      { id := "commercial", label := "Commercial", column := 2 },
      { id := "industrial", label := "Industrial", column := 2 },
      { id := "transport", label := "Transport", column := 2 }
    ]
    let links : Array SankeyDiagram.Link := #[
      -- Sources to conversion
      { source := "coal", target := "electricity", value := 120 },
      { source := "coal", target := "heat", value := 30 },
      { source := "gas", target := "electricity", value := 80 },
      { source := "gas", target := "heat", value := 60 },
      { source := "nuclear", target := "electricity", value := 90 },
      { source := "renewable", target := "electricity", value := 50 },
      -- Conversion to end uses
      { source := "electricity", target := "residential", value := 100 },
      { source := "electricity", target := "commercial", value := 80 },
      { source := "electricity", target := "industrial", value := 120 },
      { source := "electricity", target := "transport", value := 40 },
      { source := "heat", target := "residential", value := 50 },
      { source := "heat", target := "commercial", value := 25 },
      { source := "heat", target := "industrial", value := 15 }
    ]
    let data : SankeyDiagram.Data := { nodes, links }
    let dims : SankeyDiagram.Dimensions := {
      width := extraWideWidth, height := 280
      marginLeft := 10, marginRight := 80
      nodeWidth := 15
    }
    let dataDyn ← Dynamic.pureM data
    let _ ← sankeyDiagram dataDyn theme dims
    pure ()

end Demos.ReactiveShowcase
