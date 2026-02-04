/-
  Chart Panels - All data visualization chart components.
-/
import Reactive
import Afferent
import Afferent.Canopy
import Afferent.Canopy.Reactive
import AfferentCharts.Canopy.Widget.Charts
import Linalg.Core

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

/-! ## Animation Helpers -/

/-- Oscillate a value around a base with sine wave. -/
def oscillate (base : Float) (amplitude : Float) (speed : Float) (phase : Float) (t : Float) : Float :=
  base + amplitude * Float.sin (t * speed + phase)

/-- Oscillate array values with phase offsets per element. -/
def oscillateArray (base : Array Float) (amplitude : Float) (speed : Float) (t : Float) : Array Float :=
  base.mapIdx fun i v => oscillate v amplitude speed (i.toFloat * 0.7) t

/-- Normalize an array to sum to a target (for pie/donut charts). -/
def normalizeToSum (arr : Array Float) (target : Float) : Array Float :=
  let sum := arr.foldl (· + ·) 0.0
  if sum == 0.0 then arr else arr.map (· * target / sum)

/-- Smooth periodic value between min and max. -/
def smoothPeriodic (min max speed t : Float) : Float :=
  let mid := (min + max) / 2.0
  let amp := (max - min) / 2.0
  mid + amp * Float.sin (t * speed)

/-- BarChart panel - demonstrates bar chart visualization. -/
def barChartPanel : WidgetM Unit :=
  titledPanel' "Bar Chart" .outlined do
    caption' "Sales data by quarter:"
    let baseData := #[42.0, 78.0, 56.0, 91.0]
    let labels := #["Q1", "Q2", "Q3", "Q4"]
    let dims : BarChart.Dimensions := { width := standardWidth, height := standardHeight, marginLeft := standardMarginLeft }
    let elapsedTime ← useElapsedTime
    let dataDyn ← Dynamic.mapM (fun t => oscillateArray baseData 8.0 1.5 t) elapsedTime
    let _ ← barChart dataDyn labels .primary dims
    pure ()

/-- LineChart panel - demonstrates line chart visualization. -/
def lineChartPanel : WidgetM Unit :=
  titledPanel' "Line Chart" .outlined do
    caption' "Monthly revenue trend:"
    let baseData := #[12.0, 19.0, 15.0, 25.0, 22.0, 30.0]
    let labels := #["Jan", "Feb", "Mar", "Apr", "May", "Jun"]
    let dims : LineChart.Dimensions := { width := standardWidth, height := standardHeight, marginLeft := standardMarginLeft }
    let elapsedTime ← useElapsedTime
    let dataDyn ← Dynamic.mapM (fun t => oscillateArray baseData 3.0 2.0 t) elapsedTime
    let _ ← lineChart dataDyn labels .primary dims
    pure ()

/-- AreaChart panel - demonstrates area chart visualization. -/
def areaChartPanel : WidgetM Unit :=
  titledPanel' "Area Chart" .outlined do
    caption' "Website traffic over time:"
    let baseData := #[120.0, 180.0, 150.0, 220.0, 190.0, 280.0, 250.0]
    let labels := #["Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"]
    let dims : AreaChart.Dimensions := { width := standardWidth, height := standardHeight, marginLeft := standardMarginLeft }
    let elapsedTime ← useElapsedTime
    let dataDyn ← Dynamic.mapM (fun t => oscillateArray baseData 25.0 1.8 t) elapsedTime
    let _ ← areaChart dataDyn labels .primary dims
    pure ()

/-- PieChart panel - demonstrates pie chart visualization. -/
def pieChartPanel : WidgetM Unit :=
  titledPanel' "Pie Chart" .outlined do
    caption' "Market share by browser:"
    let baseValues := #[65.0, 19.0, 4.0, 3.0, 9.0]
    let labels := #["Chrome", "Safari", "Firefox", "Edge", "Other"]
    let dims : PieChart.Dimensions := { width := standardWidth, height := mediumHeight, radius := standardRadius, showLabels := false }
    let elapsedTime ← useElapsedTime
    let slicesDyn ← Dynamic.mapM (fun t =>
      let animated := oscillateArray baseValues 5.0 1.2 t
      let normalized := normalizeToSum animated 100.0
      normalized.zipWith (fun v lbl => ({ value := v, label := some lbl } : PieChart.Slice)) labels
    ) elapsedTime
    let _ ← pieChartWithLegend slicesDyn dims
    pure ()

/-- DonutChart panel - demonstrates donut chart visualization. -/
def donutChartPanel : WidgetM Unit :=
  titledPanel' "Donut Chart" .outlined do
    caption' "Expense breakdown:"
    let baseValues := #[35.0, 20.0, 15.0, 10.0, 20.0]
    let labels := #["Housing", "Food", "Transport", "Utilities", "Other"]
    let dims : DonutChart.Dimensions := {
      width := standardWidth, height := mediumHeight
      outerRadius := standardRadius, innerRadius := 40
      showLabels := false
      centerLabel := some "Total"
      centerValue := some "$3,500"
    }
    let elapsedTime ← useElapsedTime
    let slicesDyn ← Dynamic.mapM (fun t =>
      let animated := oscillateArray baseValues 4.0 1.0 t
      let normalized := normalizeToSum animated 100.0
      normalized.zipWith (fun v lbl => ({ value := v, label := some lbl } : DonutChart.Slice)) labels
    ) elapsedTime
    let _ ← donutChartWithLegend slicesDyn dims
    pure ()

/-- ScatterPlot panel - demonstrates scatter plot visualization. -/
def scatterPlotPanel : WidgetM Unit :=
  titledPanel' "Scatter Plot" .outlined do
    caption' "Height vs Weight correlation:"
    let basePoints : Array (Float × Float) := #[
      (160, 55), (165, 62), (170, 68), (172, 65), (175, 72), (178, 75),
      (180, 78), (182, 82), (168, 60), (174, 70), (185, 88), (163, 58),
      (177, 74), (169, 64), (183, 85)
    ]
    let dims : ScatterPlot.Dimensions := { width := standardWidth, height := mediumHeight, marginLeft := mediumMarginLeft }
    let elapsedTime ← useElapsedTime
    let pointsDyn ← Dynamic.mapM (fun t =>
      basePoints.mapIdx fun i (bx, by_) =>
        let phase := i.toFloat * 0.5
        let dx := 3.0 * Float.sin (t * 1.5 + phase)
        let dy := 3.0 * Float.cos (t * 1.5 + phase)
        ({ x := bx + dx, y := by_ + dy } : ScatterPlot.DataPoint)
    ) elapsedTime
    let _ ← scatterPlot pointsDyn dims
    pure ()

/-- HorizontalBarChart panel - demonstrates horizontal bar chart. -/
def horizontalBarChartPanel : WidgetM Unit :=
  titledPanel' "Horizontal Bar Chart" .outlined do
    caption' "Programming language popularity:"
    let baseData := #[85.0, 72.0, 58.0, 45.0, 38.0]
    let labels := #["Python", "JavaScript", "Java", "C++", "Go"]
    let dims : HorizontalBarChart.Dimensions := { width := standardWidth, height := standardHeight, marginLeft := extraLargeMarginLeft }
    let elapsedTime ← useElapsedTime
    let dataDyn ← Dynamic.mapM (fun t => oscillateArray baseData 6.0 1.3 t) elapsedTime
    let _ ← horizontalBarChart dataDyn labels .primary dims
    pure ()

/-- BubbleChart panel - demonstrates bubble chart visualization. -/
def bubbleChartPanel : WidgetM Unit :=
  titledPanel' "Bubble Chart" .outlined do
    caption' "Country stats (GDP vs Population):"
    -- GDP (x, trillions), Population (y, hundreds of millions), Size = land area
    let basePoints : Array (Float × Float × Float × String) := #[
      (25.5, 3.3, 9.8, "USA"), (17.9, 14.1, 9.6, "China"), (4.2, 1.3, 0.4, "Japan"),
      (4.1, 0.8, 0.4, "Germany"), (3.5, 14.2, 3.3, "India"), (3.1, 0.7, 0.2, "UK"),
      (2.8, 0.7, 0.6, "France"), (2.0, 2.1, 8.5, "Brazil"), (1.8, 1.5, 17.1, "Russia"),
      (1.7, 0.5, 10.0, "Canada")
    ]
    let dims : BubbleChart.Dimensions := {
      width := mediumWidth, height := tallHeight
      marginLeft := largeMarginLeft, marginBottom := 40
      minBubbleRadius := 6, maxBubbleRadius := 28
      showBubbleLabels := false
    }
    let elapsedTime ← useElapsedTime
    let pointsDyn ← Dynamic.mapM (fun t =>
      basePoints.mapIdx fun i (bx, by_, bsize, lbl) =>
        let phase := i.toFloat * 0.6
        let sizePulse := bsize * (1.0 + 0.15 * Float.sin (t * 2.0 + phase))
        let dx := 0.3 * Float.sin (t * 1.2 + phase)
        let dy := 0.2 * Float.cos (t * 1.2 + phase)
        ({ x := bx + dx, y := by_ + dy, size := sizePulse, label := some lbl } : BubbleChart.DataPoint)
    ) elapsedTime
    let _ ← bubbleChart pointsDyn dims
    pure ()

/-- Histogram panel - demonstrates histogram visualization. -/
def histogramPanel : WidgetM Unit :=
  titledPanel' "Histogram" .outlined do
    caption' "Test score distribution:"
    -- Simulated test scores (roughly normal distribution)
    let baseScores : Array Float := #[
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
    let elapsedTime ← useElapsedTime
    let scoresDyn ← Dynamic.mapM (fun t =>
      -- Shift the distribution peak left/right over time
      let shift := 5.0 * Float.sin (t * 0.8)
      baseScores.map (· + shift)
    ) elapsedTime
    let _ ← histogram scoresDyn .primary dims binConfig
    pure ()

/-- BoxPlot panel - demonstrates box and whisker plot. -/
def boxPlotPanel : WidgetM Unit :=
  titledPanel' "Box Plot" .outlined do
    caption' "Salary distribution by department:"
    -- Simulated salary data for different departments (in thousands)
    let baseEngineering : Array Float := #[65, 72, 78, 82, 85, 88, 90, 92, 95, 98, 102, 105, 110, 115, 145]
    let baseSales : Array Float := #[45, 52, 55, 58, 60, 62, 65, 68, 70, 72, 75, 78, 82, 95]
    let baseMarketing : Array Float := #[50, 55, 58, 60, 62, 65, 68, 70, 72, 75, 78, 80, 85]
    let labels := #["Eng", "Sales", "Mkt"]
    let dims : BoxPlot.Dimensions := {
      width := standardWidth, height := mediumHeight
      marginLeft := mediumMarginLeft, marginBottom := standardMarginBottom
      boxWidth := 50
    }
    let elapsedTime ← useElapsedTime
    let dataDyn ← Dynamic.mapM (fun t =>
      let shift0 := 8.0 * Float.sin (t * 1.0)
      let shift1 := 6.0 * Float.sin (t * 1.0 + 1.0)
      let shift2 := 5.0 * Float.sin (t * 1.0 + 2.0)
      #[baseEngineering.map (· + shift0), baseSales.map (· + shift1), baseMarketing.map (· + shift2)]
    ) elapsedTime
    let _ ← boxPlotFromData dataDyn labels dims
    pure ()

/-- Heatmap panel - demonstrates correlation matrix heatmap. -/
def heatmapPanel : WidgetM Unit :=
  titledPanel' "Heatmap" .outlined do
    caption' "Feature correlation matrix:"
    -- Simulated correlation matrix (base values)
    let baseValues : Array (Array Float) := #[
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
    let elapsedTime ← useElapsedTime
    let valuesDyn ← Dynamic.mapM (fun t =>
      baseValues.mapIdx fun row rowArr =>
        rowArr.mapIdx fun col v =>
          -- Keep diagonal at 1.0, pulse off-diagonal values
          if row == col then v
          else
            let phase := (row.toFloat + col.toFloat) * 0.5
            let pulse := 0.15 * Float.sin (t * 1.5 + phase)
            min 1.0 (max (-1.0) (v + pulse))
    ) elapsedTime
    let _ ← correlationMatrix valuesDyn labels dims
    pure ()

/-- MathPlot panel - demonstrates function sampling with line + scatter series. -/
def mathPlotPanel : WidgetM Unit :=
  titledPanel' "Math Plot" .outlined do
    caption' "Sampled functions with animated phase shift:"
    let xMin := -2.0 * Linalg.Float.pi
    let xMax := 2.0 * Linalg.Float.pi
    let dims : MathPlot.Dimensions := {
      width := mediumWidth, height := mediumHeight
      marginLeft := standardMarginLeft, marginBottom := standardMarginBottom
      showMarkers := false
    }
    let config : MathPlot.Config := {
      xRange := { min := some xMin, max := some xMax }
      yRange := { min := some (-1.5), max := some 1.5 }
      xLabel := some "x"
      yLabel := some "f(x)"
    }
    let elapsedTime ← useElapsedTime
    let seriesDyn ← Dynamic.mapM (fun t =>
      let phase := t * 0.8
      let sinSpec : MathPlot.FunctionSpec := {
        f := fun x => Float.sin (x + phase)
        xMin := xMin
        xMax := xMax
        samples := 220
      }
      let cosSpec : MathPlot.FunctionSpec := {
        f := fun x => 0.6 * Float.cos (x * 0.8 - phase)
        xMin := xMin
        xMax := xMax
        samples := 220
      }
      let sinSeries := MathPlot.seriesFromFunction sinSpec
        (label := some "sin(x)")
        (style := { kind := .line, showMarkers := some false })
      let cosSeries := MathPlot.seriesFromFunction cosSpec
        (label := some "0.6 cos(0.8x)")
        (style := { kind := .line, showMarkers := some false })
      let scatterPoints := Id.run do
        let mut pts : Array MathPlot.Point := #[]
        for i in [0:12] do
          let x := xMin + (xMax - xMin) * i.toFloat / 11.0
          let y := 0.4 * Float.sin (x - phase * 0.5)
          pts := pts.push { x, y }
        pts
      let scatterSeries : MathPlot.Series := {
        points := scatterPoints
        color := some (Color.rgba 0.95 0.7 0.2 1.0)
        style := { kind := .scatter, pointRadius := some 4.5 }
      }
      #[sinSeries, cosSeries, scatterSeries]
    ) elapsedTime
    let _ ← mathPlot seriesDyn dims config
    pure ()

/-- StackedBarChart panel - demonstrates stacked bar chart visualization. -/
def stackedBarChartPanel : WidgetM Unit :=
  titledPanel' "Stacked Bar Chart" .outlined do
    caption' "Quarterly revenue by product line:"
    let categories := #["Q1", "Q2", "Q3", "Q4"]
    let baseHardware := #[42.0, 48.0, 55.0, 61.0]
    let baseSoftware := #[28.0, 35.0, 42.0, 50.0]
    let baseServices := #[15.0, 18.0, 22.0, 28.0]
    let dims : StackedBarChart.Dimensions := {
      width := largeWidth, height := tallHeight
      marginLeft := mediumMarginLeft, marginBottom := standardMarginBottom
    }
    let elapsedTime ← useElapsedTime
    let dataDyn ← Dynamic.mapM (fun t =>
      let series : Array StackedBarChart.Series := #[
        { name := "Hardware", values := oscillateArray baseHardware 5.0 1.2 t },
        { name := "Software", values := oscillateArray baseSoftware 4.0 1.2 (t + 0.5) },
        { name := "Services", values := oscillateArray baseServices 3.0 1.2 (t + 1.0) }
      ]
      ({ categories, series } : StackedBarChart.Data)
    ) elapsedTime
    let _ ← stackedBarChart dataDyn dims
    pure ()

/-- GroupedBarChart panel - demonstrates grouped bar chart visualization. -/
def groupedBarChartPanel : WidgetM Unit :=
  titledPanel' "Grouped Bar Chart" .outlined do
    caption' "Monthly sales by region:"
    let categories := #["Jan", "Feb", "Mar", "Apr"]
    let baseNorth := #[85.0, 92.0, 78.0, 95.0]
    let baseSouth := #[65.0, 70.0, 82.0, 75.0]
    let baseWest := #[72.0, 68.0, 90.0, 88.0]
    let dims : GroupedBarChart.Dimensions := {
      width := largeWidth, height := tallHeight
      marginLeft := mediumMarginLeft, marginBottom := standardMarginBottom
    }
    let elapsedTime ← useElapsedTime
    let dataDyn ← Dynamic.mapM (fun t =>
      let series : Array GroupedBarChart.Series := #[
        { name := "North", values := oscillateArray baseNorth 7.0 1.4 t },
        { name := "South", values := oscillateArray baseSouth 6.0 1.4 (t + 0.7) },
        { name := "West", values := oscillateArray baseWest 6.0 1.4 (t + 1.4) }
      ]
      ({ categories, series } : GroupedBarChart.Data)
    ) elapsedTime
    let _ ← groupedBarChart dataDyn dims
    pure ()

/-- StackedAreaChart panel - demonstrates stacked area chart visualization. -/
def stackedAreaChartPanel : WidgetM Unit :=
  titledPanel' "Stacked Area Chart" .outlined do
    caption' "Website traffic sources over time:"
    let labels := #["Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"]
    let baseDirect := #[120.0, 132.0, 101.0, 134.0, 90.0, 230.0, 210.0]
    let baseSearch := #[220.0, 182.0, 191.0, 234.0, 290.0, 330.0, 310.0]
    let baseSocial := #[150.0, 232.0, 201.0, 154.0, 190.0, 330.0, 410.0]
    let dims : StackedAreaChart.Dimensions := {
      width := largeWidth, height := tallHeight
      marginLeft := mediumMarginLeft, marginBottom := standardMarginBottom
    }
    let elapsedTime ← useElapsedTime
    let dataDyn ← Dynamic.mapM (fun t =>
      let series : Array StackedAreaChart.Series := #[
        { name := "Direct", values := oscillateArray baseDirect 20.0 1.0 t },
        { name := "Search", values := oscillateArray baseSearch 25.0 1.0 (t + 0.5) },
        { name := "Social", values := oscillateArray baseSocial 30.0 1.0 (t + 1.0) }
      ]
      ({ labels, series } : StackedAreaChart.Data)
    ) elapsedTime
    let _ ← stackedAreaChart dataDyn dims
    pure ()

/-- RadarChart panel - demonstrates radar/spider chart visualization. -/
def radarChartPanel : WidgetM Unit :=
  titledPanel' "Radar Chart" .outlined do
    caption' "Product comparison (features):"
    let axisLabels := #["Speed", "Reliability", "Comfort", "Safety", "Efficiency", "Price"]
    let baseProductA := #[85.0, 90.0, 70.0, 95.0, 80.0, 60.0]
    let baseProductB := #[70.0, 75.0, 90.0, 80.0, 85.0, 90.0]
    let dims : RadarChart.Dimensions := {
      width := mediumWidth, height := 280
      radius := largeRadius
    }
    let elapsedTime ← useElapsedTime
    let dataDyn ← Dynamic.mapM (fun t =>
      let series : Array RadarChart.Series := #[
        { name := "Product A", values := oscillateArray baseProductA 8.0 1.5 t },
        { name := "Product B", values := oscillateArray baseProductB 8.0 1.5 (t + 3.14159) }
      ]
      ({ axisLabels, series } : RadarChart.Data)
    ) elapsedTime
    let _ ← radarChart dataDyn dims
    pure ()

/-- CandlestickChart panel - demonstrates financial OHLC chart. -/
def candlestickChartPanel : WidgetM Unit :=
  titledPanel' "Candlestick Chart" .outlined do
    caption' "Stock price movement (OHLC):"
    -- Base OHLC data: (open, high, low, close, label)
    let baseCandles : Array (Float × Float × Float × Float × String) := #[
      (100.0, 105.0, 98.0, 103.0, "Mon"), (103.0, 108.0, 101.0, 106.0, "Tue"),
      (106.0, 110.0, 104.0, 105.0, "Wed"), (105.0, 107.0, 99.0, 101.0, "Thu"),
      (101.0, 104.0, 97.0, 98.0, "Fri"), (98.0, 102.0, 96.0, 100.0, "Sat"),
      (100.0, 106.0, 99.0, 105.0, "Sun"), (105.0, 112.0, 104.0, 110.0, "Mon"),
      (110.0, 115.0, 108.0, 113.0, "Tue"), (113.0, 116.0, 109.0, 111.0, "Wed")
    ]
    let dims : CandlestickChart.Dimensions := {
      width := 360, height := extraTallHeight
      marginLeft := largeMarginLeft, marginBottom := standardMarginBottom
    }
    let elapsedTime ← useElapsedTime
    let dataDyn ← Dynamic.mapM (fun t =>
      let candles := baseCandles.mapIdx fun i (o, h, l, c, lbl) =>
        let phase := i.toFloat * 0.3
        let shift := 3.0 * Float.sin (t * 1.5 + phase)
        ({ openPrice := o + shift, highPrice := h + shift, lowPrice := l + shift,
           closePrice := c + shift, label := some lbl } : CandlestickChart.Candle)
      ({ candles } : CandlestickChart.Data)
    ) elapsedTime
    let _ ← candlestickChart dataDyn CandlestickChart.defaultColors dims
    pure ()

/-- WaterfallChart panel - demonstrates cumulative effect chart. -/
def waterfallChartPanel : WidgetM Unit :=
  titledPanel' "Waterfall Chart" .outlined do
    caption' "Profit breakdown analysis:"
    -- Base data: (label, baseValue, barType)
    let baseBars : Array (String × Float × WaterfallChart.BarType) := #[
      ("Revenue", 500.0, .initial), ("COGS", -180.0, .decrease),
      ("Gross", 320.0, .total), ("Marketing", -45.0, .decrease),
      ("R&D", -65.0, .decrease), ("Admin", -30.0, .decrease),
      ("Other Inc", 20.0, .increase), ("Net Profit", 200.0, .total)
    ]
    let dims : WaterfallChart.Dimensions := {
      width := wideWidth, height := extraTallHeight
      marginLeft := largeMarginLeft, marginBottom := largeMarginBottom
    }
    let elapsedTime ← useElapsedTime
    let dataDyn ← Dynamic.mapM (fun t =>
      let bars := baseBars.mapIdx fun i (lbl, v, bt) =>
        let phase := i.toFloat * 0.4
        let fluctuation := match bt with
          | .total => 0.0  -- Keep totals stable
          | _ => v * 0.1 * Float.sin (t * 1.2 + phase)
        ({ label := lbl, value := v + fluctuation, barType := bt } : WaterfallChart.Bar)
      ({ bars } : WaterfallChart.Data)
    ) elapsedTime
    let _ ← waterfallChart dataDyn WaterfallChart.defaultColors dims
    pure ()

/-- GaugeChart panel - demonstrates speedometer-style gauge. -/
def gaugeChartPanel : WidgetM Unit :=
  titledPanel' "Gauge Chart" .outlined do
    caption' "CPU usage indicator:"
    let segments : Array GaugeChart.Segment := #[
      { startFrac := 0.0, endFrac := 0.6, color := Color.rgba 0.20 0.69 0.35 1.0 },   -- Green
      { startFrac := 0.6, endFrac := 0.8, color := Color.rgba 0.95 0.75 0.10 1.0 },   -- Yellow
      { startFrac := 0.8, endFrac := 1.0, color := Color.rgba 0.90 0.25 0.20 1.0 }    -- Red
    ]
    let dims : GaugeChart.Dimensions := {
      width := 220, height := 160
      radius := standardRadius
    }
    let elapsedTime ← useElapsedTime
    let dataDyn ← Dynamic.mapM (fun t =>
      let cpuValue := smoothPeriodic 40.0 90.0 0.8 t
      ({ value := cpuValue, minValue := 0.0, maxValue := 100.0,
         label := some "CPU Usage", unit := some "%", segments } : GaugeChart.Data)
    ) elapsedTime
    let _ ← gaugeChart dataDyn GaugeChart.defaultColors dims
    pure ()

/-- FunnelChart panel - demonstrates sales pipeline funnel. -/
def funnelChartPanel : WidgetM Unit :=
  titledPanel' "Funnel Chart" .outlined do
    caption' "Sales conversion funnel:"
    let baseStages : Array (String × Float) := #[
      ("Visitors", 10000.0), ("Leads", 5200.0), ("Prospects", 2800.0),
      ("Negotiations", 1400.0), ("Sales", 680.0)
    ]
    let dims : FunnelChart.Dimensions := {
      width := largeWidth, height := tallHeight
      marginRight := 110
    }
    let elapsedTime ← useElapsedTime
    let dataDyn ← Dynamic.mapM (fun t =>
      let stages := baseStages.mapIdx fun i (lbl, v) =>
        let phase := i.toFloat * 0.5
        let fluctuation := v * 0.08 * Float.sin (t * 1.0 + phase)
        ({ label := lbl, value := v + fluctuation } : FunnelChart.Stage)
      ({ stages } : FunnelChart.Data)
    ) elapsedTime
    let _ ← funnelChart dataDyn dims
    pure ()

/-- TreemapChart panel - demonstrates hierarchical treemap visualization. -/
def treemapChartPanel : WidgetM Unit :=
  titledPanel' "Treemap Chart" .outlined do
    caption' "Disk usage by category:"
    let dims : TreemapChart.Dimensions := {
      width := wideWidth, height := 260
      maxDepth := 2
    }
    let elapsedTime ← useElapsedTime
    let dataDyn ← Dynamic.mapM (fun t =>
      -- Animate leaf values with pulsing
      let pulse (baseVal : Float) (idx : Float) : Float :=
        baseVal * (1.0 + 0.12 * Float.sin (t * 1.5 + idx * 0.6))
      let nodes : Array TreemapChart.TreeNode := #[
        { label := "Documents", value := 0, children := #[
            { label := "PDFs", value := pulse 2500 0 },
            { label := "Word", value := pulse 1800 1 },
            { label := "Spreadsheets", value := pulse 1200 2 }
          ]
        },
        { label := "Media", value := 0, children := #[
            { label := "Photos", value := pulse 4500 3 },
            { label := "Videos", value := pulse 8000 4 },
            { label := "Music", value := pulse 2200 5 }
          ]
        },
        { label := "Apps", value := pulse 5500 6 },
        { label := "System", value := pulse 3200 7 },
        { label := "Other", value := pulse 1500 8 }
      ]
      ({ nodes } : TreemapChart.Data)
    ) elapsedTime
    let _ ← treemapChart dataDyn dims
    pure ()

/-- SankeyDiagram panel - demonstrates flow diagram visualization. -/
def sankeyDiagramPanel : WidgetM Unit :=
  titledPanel' "Sankey Diagram" .outlined do
    caption' "Energy flow from sources to uses:"
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
    -- Base link values: (source, target, baseValue)
    let baseLinks : Array (String × String × Float) := #[
      ("coal", "electricity", 120), ("coal", "heat", 30),
      ("gas", "electricity", 80), ("gas", "heat", 60),
      ("nuclear", "electricity", 90), ("renewable", "electricity", 50),
      ("electricity", "residential", 100), ("electricity", "commercial", 80),
      ("electricity", "industrial", 120), ("electricity", "transport", 40),
      ("heat", "residential", 50), ("heat", "commercial", 25),
      ("heat", "industrial", 15)
    ]
    let dims : SankeyDiagram.Dimensions := {
      width := extraWideWidth, height := 280
      marginLeft := 10, marginRight := 80
      nodeWidth := 15
    }
    let elapsedTime ← useElapsedTime
    let dataDyn ← Dynamic.mapM (fun t =>
      let links := baseLinks.mapIdx fun i (src, tgt, v) =>
        let phase := i.toFloat * 0.4
        let fluctuation := v * 0.15 * Float.sin (t * 1.2 + phase)
        ({ source := src, target := tgt, value := v + fluctuation } : SankeyDiagram.Link)
      ({ nodes, links } : SankeyDiagram.Data)
    ) elapsedTime
    let _ ← sankeyDiagram dataDyn dims
    pure ()

end Demos.ReactiveShowcase
