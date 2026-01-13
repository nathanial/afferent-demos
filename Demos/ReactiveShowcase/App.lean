/-
  ReactiveShowcase App - Composes all components into the demo UI using WidgetM.
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

namespace Demos.ReactiveShowcase

/-! ## Panel Components

Each panel is a self-contained WidgetM component that can be composed into the main layout.
-/

/-- Labels panel - demonstrates text styling variants. -/
def labelsPanel (theme : Theme) : WidgetM Unit :=
  titledPanel' "Labels" .outlined theme do
    heading1' "Heading 1" theme
    heading2' "Heading 2" theme
    heading3' "Heading 3" theme
    bodyText' "Body text - normal paragraph content" theme
    caption' "Caption - small muted text" theme

/-- Buttons panel - demonstrates button variants with click counter.
    Returns the merged click event for external wiring. -/
def buttonsPanel (theme : Theme) : WidgetM (Reactive.Event Spider Unit) :=
  titledPanel' "Buttons" .outlined theme do
    caption' "Click a button to increment the counter:" theme
    row' (gap := 8) (style := {}) do
      let c1 ← button "Primary" theme .primary
      let c2 ← button "Secondary" theme .secondary
      let c3 ← button "Outline" theme .outline
      let c4 ← button "Ghost" theme .ghost
      Event.leftmostM [c1, c2, c3, c4]

/-- Click counter panel - demonstrates a button that shows its own click count.
    Uses lower-level hooks with dynWidget for dynamic label updates. -/
def clickCounterPanel (theme : Theme) : WidgetM Unit :=
  titledPanel' "Click Counter" .outlined theme do
    caption' "Button displays its own click count:" theme
    -- Register the button for event handling
    let name ← registerComponentW "counter-button"
    let isHovered ← useHover name
    let onClick ← useClick name
    -- Count clicks using foldDyn
    let clickCount ← Reactive.foldDyn (fun _ n => n + 1) 0 onClick
    -- Emit button with dynamic label based on count
    let combined ← Dynamic.zipWithM Prod.mk clickCount isHovered
    let _ ← dynWidget combined fun (count, hovered) => do
      let state : WidgetState := { hovered, pressed := false, focused := false }
      let label := if count == 0 then "Click me!" else s!"Clicked {count} times"
      emit (pure (buttonVisual name label theme .primary state))

/-- Checkboxes panel - demonstrates checkbox toggle behavior. -/
def checkboxesPanel (theme : Theme) : WidgetM Unit :=
  titledPanel' "Checkboxes" .outlined theme do
    caption' "Click to toggle:" theme
    row' (gap := 24) (style := {}) do
      let _ ← checkbox "Option 1" theme false
      let _ ← checkbox "Option 2" theme true
      pure ()

/-- Radio buttons panel - demonstrates single-selection radio group. -/
def radioButtonsPanel (theme : Theme) : WidgetM Unit :=
  titledPanel' "Radio Buttons" .outlined theme do
    caption' "Click to select one option:" theme
    let radioOptions : Array RadioOption := #[
      { label := "Option 1", value := "option1" },
      { label := "Option 2", value := "option2" },
      { label := "Option 3", value := "option3" }
    ]
    let _ ← radioGroup radioOptions theme "option1"
    pure ()

/-- Switches panel - demonstrates iOS-style toggle switches. -/
def switchesPanel (theme : Theme) : WidgetM Unit :=
  titledPanel' "Switches" .outlined theme do
    caption' "Click to toggle:" theme
    row' (gap := 24) (style := {}) do
      let _ ← switch (some "Notifications") theme false
      let _ ← switch (some "Dark Mode") theme true
      pure ()

/-- Modal trigger panel - button to open the modal dialog.
    Returns the click event for external wiring. -/
def modalTriggerPanel (theme : Theme) : WidgetM (Reactive.Event Spider Unit) :=
  titledPanel' "Modal" .outlined theme do
    caption' "Click button to open modal:" theme
    button "Open Modal" theme .primary

/-- Toast triggers panel - buttons to show different toast notifications.
    Takes fire functions for each toast type. -/
def toastsPanel (theme : Theme)
    (fireInfo fireSuccess fireWarning fireError : Unit → IO Unit) : WidgetM Unit :=
  titledPanel' "Toasts" .outlined theme do
    caption' "Click to show notifications:" theme
    row' (gap := 8) (style := {}) do
      let infoClick ← button "Info" theme .primary
      let successClick ← button "Success" theme .primary
      let warnClick ← button "Warning" theme .secondary
      let errorClick ← button "Error" theme .secondary
      -- Wire clicks to toast triggers
      let infoAction ← Event.mapM (fun _ => fireInfo ()) infoClick
      let successAction ← Event.mapM (fun _ => fireSuccess ()) successClick
      let warnAction ← Event.mapM (fun _ => fireWarning ()) warnClick
      let errorAction ← Event.mapM (fun _ => fireError ()) errorClick
      performEvent_ infoAction
      performEvent_ successAction
      performEvent_ warnAction
      performEvent_ errorAction

/-- Menu panel - demonstrates trigger-based menu with actions and separators. -/
def menuPanel (theme : Theme) : WidgetM Unit :=
  titledPanel' "Menu" .outlined theme do
    caption' "Click button to open menu (hover submenus):" theme
    row' (gap := 16) (style := {}) do
      let items := #[
        MenuItem.action "Cut",
        MenuItem.action "Copy",
        MenuItem.action "Paste",
        MenuItem.separator,
        MenuItem.submenu "Format" #[
          MenuItem.action "Bold",
          MenuItem.action "Italic",
          MenuItem.action "Underline",
          MenuItem.separator,
          MenuItem.submenu "Text Color" #[
            MenuItem.action "Red",
            MenuItem.action "Green",
            MenuItem.action "Blue"
          ]
        ],
        MenuItem.separator,
        MenuItem.action "Delete" (enabled := false)
      ]
      let (_, menuResult) ← menu items theme (trigger := do
        let _ ← button "Actions" theme .primary
        pure ())
      -- Show when selection happens (path is now an array)
      let _ ← performEvent_ (← Event.mapM (fun path => do
        IO.println s!"Menu item selected at path: {path.toList}"
      ) menuResult.onSelect)
      pure ()

/-- MenuBar panel - demonstrates horizontal menu bar with multiple dropdown menus. -/
def menuBarPanel (theme : Theme) : WidgetM Unit :=
  titledPanel' "MenuBar" .outlined theme do
    caption' "Click menu triggers, hover to switch while open:" theme
    let fileMenu : MenuBarMenu := {
      label := "File"
      items := #[
        MenuItem.action "New",
        MenuItem.action "Open",
        MenuItem.action "Save",
        MenuItem.separator,
        MenuItem.action "Exit"
      ]
    }
    let editMenu : MenuBarMenu := {
      label := "Edit"
      items := #[
        MenuItem.action "Cut",
        MenuItem.action "Copy",
        MenuItem.action "Paste",
        MenuItem.separator,
        MenuItem.submenu "Format" #[
          MenuItem.action "Bold",
          MenuItem.action "Italic",
          MenuItem.action "Underline"
        ]
      ]
    }
    let viewMenu : MenuBarMenu := {
      label := "View"
      items := #[
        MenuItem.action "Zoom In",
        MenuItem.action "Zoom Out",
        MenuItem.separator,
        MenuItem.action "Full Screen"
      ]
    }
    let helpMenu : MenuBarMenu := {
      label := "Help"
      items := #[
        MenuItem.action "Documentation",
        MenuItem.action "About"
      ]
      enabled := true
    }
    let result ← menuBar #[fileMenu, editMenu, viewMenu, helpMenu] theme
    let _ ← performEvent_ (← Event.mapM (fun path => do
      IO.println s!"MenuBar selected: menu {path.menuIndex}, path {path.itemPath.toList}"
    ) result.onSelect)
    pure ()

/-- Table panel - demonstrates tabular data display with row selection. -/
def tablePanel (theme : Theme) : WidgetM Unit :=
  titledPanel' "Table" .outlined theme do
    caption' "Click rows to select:" theme
    let columns : Array TableColumn := #[
      { header := "Name" },
      { header := "Email", width := some 160 },
      { header := "Role" },
      { header := "Status" }
    ]
    let rows : Array (Array String) := #[
      #["Alice", "alice@ex.com", "Admin", "Active"],
      #["Bob", "bob@ex.com", "User", "Active"],
      #["Carol", "carol@ex.com", "User", "Inactive"],
      #["David", "david@ex.com", "User", "Active"],
      #["Eve", "eve@ex.com", "Moderator", "Active"]
    ]
    let result ← table columns rows theme
    let _ ← performEvent_ (← Event.mapM (fun rowIdx => do
      IO.println s!"Table row selected: {rowIdx}"
    ) result.onRowSelect)
    pure ()

/-- DataGrid panel - demonstrates editable grid cells. -/
def dataGridPanel (theme : Theme) (font : Afferent.Font) : WidgetM Unit :=
  titledPanel' "DataGrid" .outlined theme do
    caption' "Click a cell to edit, press Enter to commit:" theme
    let columns : Array DataGridColumn := #[
      { header := "Item" },
      { header := "Qty", width := some 60 },
      { header := "Price", width := some 80 }
    ]
    let rows : Array (Array String) := #[
      #["Apples", "3", "$2.40"],
      #["Oranges", "5", "$4.10"],
      #["Bananas", "2", "$1.10"],
      #["Grapes", "1", "$3.25"]
    ]
    let result ← dataGrid columns rows theme font {}
    let _ ← performEvent_ (← Event.mapM (fun (r, c, v) => do
      IO.println s!"DataGrid edit: ({r}, {c}) = {v}"
    ) result.onEdit)
    pure ()

/-- ListBox panel - demonstrates scrollable list with selection. -/
def listBoxPanel (theme : Theme) : WidgetM Unit :=
  titledPanel' "ListBox" .outlined theme do
    caption' "Click items to select:" theme
    let fruits := #["Apple", "Banana", "Cherry", "Date",
                    "Elderberry", "Fig", "Grape", "Honeydew",
                    "Kiwi", "Lemon", "Mango", "Nectarine"]
    let result ← listBox fruits theme
    let _ ← performEvent_ (← Event.mapM (fun itemIdx => do
      IO.println s!"ListBox item selected: {itemIdx}"
    ) result.onSelect)
    pure ()

/-- VirtualList panel - demonstrates efficient rendering of long lists. -/
def virtualListPanel (theme : Theme) : WidgetM Unit :=
  titledPanel' "VirtualList" .outlined theme do
    caption' "Only visible rows are rendered:" theme
    let itemCount := 500
    let config : VirtualListConfig := {
      width := 220
      height := 180
      itemHeight := 28
      overscan := 3
    }
    let result ← virtualList itemCount (fun idx => do
      let isEven := idx % 2 == 0
      let bg := if isEven then theme.panel.background.withAlpha 0.2 else Color.transparent
      let rowStyle : BoxStyle := {
        backgroundColor := some bg
        padding := EdgeInsets.symmetric 8 4
        width := .percent 1.0
        minHeight := some config.itemHeight
      }
      let wid ← freshId
      let props : FlexContainer := { FlexContainer.row 0 with alignItems := .center }
      let label ← bodyText s!"Row {idx}" theme
      pure (.flex wid none props rowStyle #[label])
    ) theme config

    let _ ← dynWidget result.visibleRange fun (start, stop) =>
      caption' s!"Visible indices: [{start}, {stop})" theme

    let _ ← performEvent_ (← Event.mapM (fun idx => do
      IO.println s!"VirtualList item clicked: {idx}"
    ) result.onItemClick)
    pure ()

/-- BarChart panel - demonstrates bar chart visualization. -/
def barChartPanel (theme : Theme) : WidgetM Unit :=
  titledPanel' "Bar Chart" .outlined theme do
    caption' "Sales data by quarter:" theme
    let salesData := #[42.0, 78.0, 56.0, 91.0]
    let labels := #["Q1", "Q2", "Q3", "Q4"]
    let dims : BarChart.Dimensions := { width := 280, height := 180, marginLeft := 40 }
    let _ ← barChart salesData labels theme .primary dims
    pure ()

/-- LineChart panel - demonstrates line chart visualization. -/
def lineChartPanel (theme : Theme) : WidgetM Unit :=
  titledPanel' "Line Chart" .outlined theme do
    caption' "Monthly revenue trend:" theme
    let revenueData := #[12.0, 19.0, 15.0, 25.0, 22.0, 30.0]
    let labels := #["Jan", "Feb", "Mar", "Apr", "May", "Jun"]
    let dims : LineChart.Dimensions := { width := 280, height := 180, marginLeft := 40 }
    let _ ← lineChart revenueData labels theme .primary dims
    pure ()

/-- AreaChart panel - demonstrates area chart visualization. -/
def areaChartPanel (theme : Theme) : WidgetM Unit :=
  titledPanel' "Area Chart" .outlined theme do
    caption' "Website traffic over time:" theme
    let trafficData := #[120.0, 180.0, 150.0, 220.0, 190.0, 280.0, 250.0]
    let labels := #["Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"]
    let dims : AreaChart.Dimensions := { width := 280, height := 180, marginLeft := 40 }
    let _ ← areaChart trafficData labels theme .primary dims
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
    let dims : PieChart.Dimensions := { width := 280, height := 200, radius := 70, showLabels := false }
    let _ ← pieChartWithLegend slices theme dims
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
      width := 280, height := 200
      outerRadius := 70, innerRadius := 40
      showLabels := false
      centerLabel := some "Total"
      centerValue := some "$3,500"
    }
    let _ ← donutChartWithLegend slices theme dims
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
    let dims : ScatterPlot.Dimensions := { width := 280, height := 200, marginLeft := 45 }
    let _ ← scatterPlot points theme dims
    pure ()

/-- HorizontalBarChart panel - demonstrates horizontal bar chart. -/
def horizontalBarChartPanel (theme : Theme) : WidgetM Unit :=
  titledPanel' "Horizontal Bar Chart" .outlined theme do
    caption' "Programming language popularity:" theme
    let data := #[85.0, 72.0, 58.0, 45.0, 38.0]
    let labels := #["Python", "JavaScript", "Java", "C++", "Go"]
    let dims : HorizontalBarChart.Dimensions := { width := 280, height := 180, marginLeft := 70 }
    let _ ← horizontalBarChart data labels theme .primary dims
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
      width := 320, height := 220
      marginLeft := 50, marginBottom := 40
      minBubbleRadius := 6, maxBubbleRadius := 28
      showBubbleLabels := false
    }
    let _ ← bubbleChart points theme dims
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
      width := 300, height := 180
      marginLeft := 45, marginBottom := 35
    }
    let binConfig : Histogram.BinConfig := { binCount := some 8 }
    let _ ← histogram scores theme .primary dims binConfig
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
      width := 280, height := 200
      marginLeft := 45, marginBottom := 35
      boxWidth := 50
    }
    let _ ← boxPlotFromData #[engineering, sales, marketing] labels theme dims
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
      width := 280, height := 220
      marginLeft := 30, marginTop := 30
      marginBottom := 20, marginRight := 50
      showValues := true
    }
    let _ ← correlationMatrix values labels theme dims
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
      width := 340, height := 220
      marginLeft := 45, marginBottom := 35
    }
    let _ ← stackedBarChart data theme dims
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
      width := 340, height := 220
      marginLeft := 45, marginBottom := 35
    }
    let _ ← groupedBarChart data theme dims
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
      width := 340, height := 220
      marginLeft := 45, marginBottom := 35
    }
    let _ ← stackedAreaChart data theme dims
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
      width := 320, height := 280
      radius := 90
    }
    let _ ← radarChart data theme dims
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
      width := 360, height := 240
      marginLeft := 50, marginBottom := 35
    }
    let _ ← candlestickChart data theme CandlestickChart.defaultColors dims
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
      width := 380, height := 240
      marginLeft := 50, marginBottom := 45
    }
    let _ ← waterfallChart data theme WaterfallChart.defaultColors dims
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
      radius := 70
    }
    let _ ← gaugeChart data theme GaugeChart.defaultColors dims
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
      width := 340, height := 220
      marginRight := 110
    }
    let _ ← funnelChart data theme dims
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
      width := 380, height := 260
      maxDepth := 2
    }
    let _ ← treemapChart data theme dims
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
      width := 480, height := 280
      marginLeft := 10, marginRight := 80
      nodeWidth := 15
    }
    let _ ← sankeyDiagram data theme dims
    pure ()

/-- ColorPicker panel - demonstrates HSV color picker widget. -/
def colorPickerPanel (theme : Theme) : WidgetM Unit :=
  titledPanel' "ColorPicker" .outlined theme do
    caption' "Click and drag to select color:" theme
    let result ← colorPicker theme Color.red
    -- Display the current color value
    let combined ← Dynamic.zipWith3M (fun a b c => (a, b, c)) result.color result.hsv result.alpha
    let _ ← dynWidget combined fun (color, hsv, alpha) => do
      let r := (color.r * 255).floor.toUInt8
      let g := (color.g * 255).floor.toUInt8
      let b := (color.b * 255).floor.toUInt8
      let a := (alpha * 100).floor.toUInt8
      caption' s!"RGB({r}, {g}, {b}) H:{(hsv.h * 360).floor.toUInt16}° α:{a}%" theme

/-- TreeView panel - demonstrates hierarchical tree with expand/collapse. -/
def treeViewPanel (theme : Theme) : WidgetM Unit :=
  titledPanel' "TreeView" .outlined theme do
    caption' "Click arrows to expand/collapse:" theme
    let nodes : Array TreeNode := #[
      .branch "Documents" #[
        .leaf "Resume.pdf",
        .leaf "Cover Letter.docx",
        .branch "Projects" #[
          .leaf "Project A.pdf",
          .leaf "Project B.pdf"
        ]
      ],
      .branch "Pictures" #[
        .leaf "Vacation.jpg",
        .leaf "Family.png"
      ],
      .leaf "Notes.txt"
    ]
    let result ← treeView nodes theme
    let _ ← performEvent_ (← Event.mapM (fun path => do
      IO.println s!"TreeView node selected: {path}"
    ) result.onNodeSelect)
    let _ ← performEvent_ (← Event.mapM (fun path => do
      IO.println s!"TreeView node toggled: {path}"
    ) result.onNodeToggle)
    pure ()

/-- Sliders panel - demonstrates slider input controls. -/
def slidersPanel (theme : Theme) : WidgetM Unit :=
  titledPanel' "Sliders" .outlined theme do
    caption' "Click to adjust value:" theme
    row' (gap := 24) (style := {}) do
      let _ ← slider (some "Volume") theme 0.3
      let _ ← slider (some "Brightness") theme 0.7
      pure ()

/-- Range slider panel - demonstrates dual-handle slider. -/
def rangeSliderPanel (theme : Theme) : WidgetM Unit :=
  titledPanel' "Range Slider" .outlined theme do
    caption' "Drag handles to select a range:" theme
    let result ← rangeSlider theme 0.2 0.8
    let combined ← Dynamic.zipWithM Prod.mk result.low result.high
    let _ ← dynWidget combined fun (low, high) => do
      let lowPct := (low * 100.0).floor.toUInt32
      let highPct := (high * 100.0).floor.toUInt32
      caption' s!"Range: {lowPct}% - {highPct}%" theme

/-- Stepper panel - demonstrates increment/decrement control. -/
def stepperPanel (theme : Theme) : WidgetM Unit :=
  titledPanel' "Stepper" .outlined theme do
    caption' "Click + or - to change value:" theme
    let config : StepperConfig := { min := 0, max := 20, step := 1, width := 160 }
    let result ← stepper theme 5 config
    let _ ← dynWidget result.value fun value =>
      caption' s!"Value: {value}" theme

/-- Progress bars panel - demonstrates determinate and indeterminate progress. -/
def progressBarsPanel (theme : Theme) : WidgetM Unit :=
  titledPanel' "Progress Bars" .outlined theme do
    caption' "Determinate and indeterminate progress:" theme
    column' (gap := 12) (style := {}) do
      let _ ← progressBar theme 0.65 .primary (some "Download") true
      let _ ← progressBar theme 0.3 .success (some "Upload") true
      let _ ← progressBar theme 0.85 .warning none true
      let _ ← progressBarIndeterminate theme .primary (some "Loading...")
      pure ()

/-- Dropdown panel - demonstrates dropdown selection. -/
def dropdownPanel (theme : Theme) : WidgetM Unit :=
  titledPanel' "Dropdown" .outlined theme do
    caption' "Click to open, select an option:" theme
    let dropdownOptions := #["Apple", "Banana", "Cherry", "Date", "Elderberry"]
    let _ ← dropdown dropdownOptions theme 0
    pure ()

/-- Dependent dropdowns panel - demonstrates dynWidget for dynamic widget rebuilding.
    The second dropdown's options change based on the first dropdown's selection. -/
def dependentDropdownsPanel (theme : Theme) : WidgetM Unit :=
  titledPanel' "Dependent Dropdowns" .outlined theme do
    caption' "Second dropdown options depend on first:" theme
    let categories := #["Fruits", "Vegetables", "Dairy"]
    let itemsForCategory (idx : Nat) : Array String :=
      match idx with
      | 0 => #["Apple", "Banana", "Cherry", "Orange"]
      | 1 => #["Carrot", "Broccoli", "Spinach", "Tomato"]
      | 2 => #["Milk", "Cheese", "Yogurt", "Butter"]
      | _ => #[]
    row' (gap := 16) (style := {}) do
      column' (gap := 4) (style := {}) do
        caption' "Category:" theme
        let catResult ← dropdown categories theme 0
        column' (gap := 4) (style := {}) do
          caption' "Item:" theme
          let _ ← dynWidget catResult.selection fun catIdx =>
            dropdown (itemsForCategory catIdx) theme 0
          pure ()

/-- Text inputs panel - demonstrates single-line text input. -/
def textInputsPanel (theme : Theme) (font : Afferent.Font) : WidgetM Unit :=
  titledPanel' "Text Inputs" .outlined theme do
    caption' "Click to focus, then type:" theme
    let _ ← textInput theme font "Enter text here..." ""
    let _ ← textInput theme font "Type something..." "Hello, World!"
    let _ ← passwordInput theme font "Enter password..." ""
    pure ()

/-- Text area panel - demonstrates multi-line text input. -/
def textAreaPanel (theme : Theme) (font : Afferent.Font) : WidgetM Unit :=
  titledPanel' "Text Area" .outlined theme do
    caption' "Multi-line text with word wrapping:" theme
    let _ ← textArea theme "Enter multi-line text..." {} font
    pure ()

/-- Panels panel - demonstrates different panel styles. -/
def panelsPanel (theme : Theme) : WidgetM Unit :=
  titledPanel' "Panels" .outlined theme do
    row' (gap := 12) (style := {}) do
      elevatedPanel' theme 12 do
        column' (gap := 4) (style := { minWidth := some 100 }) do
          heading3' "Elevated" theme
          caption' "Card-like" theme
      outlinedPanel' theme 12 do
        column' (gap := 4) (style := { minWidth := some 100 }) do
          heading3' "Outlined" theme
          caption' "Border only" theme
      filledPanel' theme 12 do
        column' (gap := 4) (style := { minWidth := some 100 }) do
          heading3' "Filled" theme
          caption' "Solid bg" theme

/-- Tab view panel - demonstrates tabbed content switching. -/
def tabViewPanel (theme : Theme) : WidgetM Unit :=
  titledPanel' "Tab View" .outlined theme do
    caption' "Click tabs to switch content:" theme
    let tabs : Array TabDef := #[
      { label := "Overview", content := do
          bodyText' "TabView organizes content into separate panels." theme
          bodyText' "Click a tab to switch between panels." theme
      },
      { label := "Settings", content := do
          caption' "Sample settings panel:" theme
          row' (gap := 16) (style := {}) do
            let _ ← checkbox "Enable feature" theme false
            pure ()
      },
      { label := "About", content := do
          heading3' "Reactive Widgets" theme
          caption' "Version 1.0.0" theme
      }
    ]
    let _ ← tabView tabs theme 0
    pure ()

/-- Split pane panel - demonstrates draggable split container. -/
def splitPanePanel (theme : Theme) : WidgetM Unit :=
  titledPanel' "Split Pane" .outlined theme do
    caption' "Drag the divider to resize panes:" theme
    let config : SplitPaneConfig := {
      orientation := .horizontal
      initialRatio := 0.4
      minPaneSize := 120
      handleThickness := 6
      width := some 420
      height := some 200
    }
    let ((_, _), result) ← splitPane config theme
      (column' (gap := 6) (style := {
        padding := EdgeInsets.uniform 12
        backgroundColor := some (theme.panel.background.withAlpha 0.2)
        width := .percent 1.0
        height := .percent 1.0
      }) do
        heading3' "Navigator" theme
        caption' "Left pane" theme
      )
      (column' (gap := 6) (style := {
        padding := EdgeInsets.uniform 12
        backgroundColor := some (theme.panel.background.withAlpha 0.1)
        width := .percent 1.0
        height := .percent 1.0
      }) do
        heading3' "Details" theme
        caption' "Right pane" theme
        bodyText' "Resize me with the handle." theme
      )
    let _ ← dynWidget result.ratio fun ratio => do
      let leftPct := (ratio * 100.0).floor.toUInt32
      let rightPct := ((1.0 - ratio) * 100.0).floor.toUInt32
      caption' s!"Split ratio: {leftPct}% / {rightPct}%" theme

/-- Date picker panel - demonstrates calendar-based date selection. -/
def datePickerPanel (theme : Theme) : WidgetM Unit :=
  titledPanel' "Date Picker" .outlined theme do
    caption' "Click a day to select a date:" theme
    let initial : DatePickerDate := { year := 2026, month := 1, day := 11 }
    let result ← datePicker theme initial {}
    let _ ← dynWidget result.selected fun sel =>
      match sel with
      | some date => caption' s!"Selected: {date.year}-{date.month}-{date.day}" theme
      | none => caption' "Selected: (none)" theme

/-- Scroll container panel - demonstrates scrollable content viewport. -/
def scrollContainerPanel (theme : Theme) : WidgetM Unit :=
  titledPanel' "Scroll Container" .outlined theme do
    caption' "Scroll with mouse wheel or trackpad:" theme
    row' (gap := 16) (style := {}) do
      -- Scrollable list of items
      outlinedPanel' theme 0 do
        let (_, scrollResult) ← vscrollContainer 150 theme do
          column' (gap := 4) (style := { padding := EdgeInsets.uniform 8 }) do
            for i in [1:21] do
              bodyText' s!"Item {i} - Scroll to see more" theme
            pure ()

        -- Display current scroll position
        column' (gap := 4) (style := { padding := EdgeInsets.uniform 8 }) do
          caption' "Scroll position:" theme
          let _ ← dynWidget scrollResult.scrollState fun state =>
            caption' s!"Y: {state.offsetY.floor.toUInt32}px" theme

/-- Tooltips panel - demonstrates hover tooltips with different positions. -/
def tooltipsPanel (theme : Theme) (font : Afferent.Font) : WidgetM Unit :=
  titledPanel' "Tooltips" .outlined theme do
    caption' "Hover over buttons to see tooltips:" theme
    row' (gap := 12) (style := {}) do
      -- Tooltip above
      let (_, _) ← tooltipTop "Appears above" theme font (delay := 0.3) do
        let _ ← button "Top" theme .outline
        pure ()
      -- Tooltip below
      let (_, _) ← tooltipBottom "Appears below" theme font (delay := 0.3) do
        let _ ← button "Bottom" theme .outline
        pure ()
      -- Tooltip left
      let (_, _) ← tooltipLeft "Appears left" theme font (delay := 0.3) do
        let _ ← button "Left" theme .outline
        pure ()
      -- Tooltip right
      let (_, _) ← tooltipRight "Appears right" theme font (delay := 0.3) do
        let _ ← button "Right" theme .outline
        pure ()
      pure ()

/-! ## Main Application -/

/-- Application state returned from createApp. -/
structure AppState where
  /-- Render function that samples all component state and returns the complete UI. -/
  render : ComponentRender

/-- Create the complete reactive showcase application.
    Sets up all components and their interactions using WidgetM. -/
def createApp (env : DemoEnv) : ReactiveM AppState := do
  let theme : Theme := { Theme.dark with font := env.fontCanopyId, smallFont := env.fontCanopySmallId }
  let events ← getEvents

  -- Pre-create shared event triggers for cross-tree wiring
  -- 1. Button click counter (buttons are in a panel, counter display is in title)
  let (buttonClickTrigger, fireButtonClick) ← Reactive.newTriggerEvent (t := Spider) (a := Unit)
  let buttonClickCount ← Reactive.foldDyn (fun _ n => n + 1) 0 buttonClickTrigger

  -- 2. Modal open trigger (trigger button is in a panel, modal is at root)
  let (modalOpenTrigger, fireModalOpen) ← Reactive.newTriggerEvent (t := Spider) (a := Unit)

  -- 3. Toast triggers (buttons in panel, toast manager at root)
  let (toastInfoTrigger, fireToastInfo) ← Reactive.newTriggerEvent (t := Spider) (a := Unit)
  let (toastSuccessTrigger, fireToastSuccess) ← Reactive.newTriggerEvent (t := Spider) (a := Unit)
  let (toastWarningTrigger, fireToastWarning) ← Reactive.newTriggerEvent (t := Spider) (a := Unit)
  let (toastErrorTrigger, fireToastError) ← Reactive.newTriggerEvent (t := Spider) (a := Unit)

  let (_, render) ← runWidget do
    let rootStyle : BoxStyle := {
      backgroundColor := some (Color.gray 0.1)
      padding := EdgeInsets.uniform 24
      width := .percent 1.0
      height := .percent 1.0
      flexItem := some (FlexItem.growing 1)
    }

    column' (gap := 20) (style := rootStyle) do
      -- Title
      heading1' "Reactive Showcase" theme
      row' (gap := 16) (style := {}) do
        caption' "FRP-powered widget demo" theme
        let _ ← dynWidget buttonClickCount fun count =>
          if count > 0 then caption' s!"(Clicks: {count})" theme
          else spacer' 0 0

      -- Three-column layout
      flexRow' { FlexContainer.row 20 with alignItems := .flexStart }
          (style := { flexItem := some (FlexItem.growing 1) }) do
        -- Left column
        column' (gap := 16) (style := {}) do
          labelsPanel theme

          -- Buttons panel with click counter wiring
          let buttonClicks ← buttonsPanel theme
          let fireAction ← Event.mapM (fun _ => fireButtonClick ()) buttonClicks
          performEvent_ fireAction

          clickCounterPanel theme

          checkboxesPanel theme
          radioButtonsPanel theme
          switchesPanel theme

          -- Modal trigger panel with open wiring
          let modalClick ← modalTriggerPanel theme
          let fireAction ← Event.mapM (fun _ => fireModalOpen ()) modalClick
          performEvent_ fireAction

          toastsPanel theme fireToastInfo fireToastSuccess fireToastWarning fireToastError

          menuPanel theme
          menuBarPanel theme

        -- Middle column
        column' (gap := 16) (style := {}) do
          slidersPanel theme
          rangeSliderPanel theme
          stepperPanel theme
          progressBarsPanel theme
          dropdownPanel theme
          dependentDropdownsPanel theme
          textInputsPanel theme env.fontCanopy
          textAreaPanel theme env.fontCanopy
          panelsPanel theme
          tabViewPanel theme

        -- Right column
        column' (gap := 16) (style := {}) do
          scrollContainerPanel theme
          tooltipsPanel theme env.fontCanopySmall
          tablePanel theme
          dataGridPanel theme env.fontCanopy
          listBoxPanel theme
          virtualListPanel theme
          treeViewPanel theme

        -- Fourth column
        column' (gap := 16) (style := {}) do
          splitPanePanel theme
          datePickerPanel theme
          colorPickerPanel theme
          barChartPanel theme
          lineChartPanel theme
          areaChartPanel theme
          pieChartPanel theme

        -- Fifth column (Charts continued)
        column' (gap := 16) (style := {}) do
          donutChartPanel theme
          scatterPlotPanel theme
          horizontalBarChartPanel theme
          bubbleChartPanel theme
          histogramPanel theme
          boxPlotPanel theme
          heatmapPanel theme
          stackedBarChartPanel theme

        -- Sixth column (More charts)
        column' (gap := 16) (style := {}) do
          groupedBarChartPanel theme
          stackedAreaChartPanel theme
          radarChartPanel theme
          candlestickChartPanel theme
          waterfallChartPanel theme
          gaugeChartPanel theme
          funnelChartPanel theme
          treemapChartPanel theme
          sankeyDiagramPanel theme

      -- Modal overlay (renders on top when open)
      let modalResult ← modal "Sample Modal" theme do
        bodyText' "This is a modal dialog." theme
        bodyText' "Click outside, press Escape, or click a button to close." theme

      -- Wire modal open trigger to modal's openModal
      let openAction ← Event.mapM (fun _ => modalResult.openModal) modalOpenTrigger
      performEvent_ openAction

      -- Toast manager (renders toast notifications)
      let toastMgr ← toastManager theme

      -- Wire toast triggers to toast manager
      let infoAction ← Event.mapM (fun _ => toastMgr.showInfo "This is an info message") toastInfoTrigger
      let successAction ← Event.mapM (fun _ => toastMgr.showSuccess "Operation completed successfully!") toastSuccessTrigger
      let warningAction ← Event.mapM (fun _ => toastMgr.showWarning "Please check your input") toastWarningTrigger
      let errorAction ← Event.mapM (fun _ => toastMgr.showError "Something went wrong") toastErrorTrigger
      performEvent_ infoAction
      performEvent_ successAction
      performEvent_ warningAction
      performEvent_ errorAction

  -- Set up automatic focus clearing
  events.registry.setupFocusClearing

  pure { render }

end Demos.ReactiveShowcase
