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
    Uses lower-level hooks with emitDynamic for dynamic label updates. -/
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
    emitDynamic do
      let count ← clickCount.sample
      let hovered ← isHovered.sample
      let state : WidgetState := { hovered, pressed := false, focused := false }
      let label := if count == 0 then "Click me!" else s!"Clicked {count} times"
      pure (buttonVisual name label theme .primary state)

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

    emitDynamic do
      let (start, stop) ← result.visibleRange.sample
      pure (caption s!"Visible indices: [{start}, {stop})" theme)

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

/-- ColorPicker panel - demonstrates HSV color picker widget. -/
def colorPickerPanel (theme : Theme) : WidgetM Unit :=
  titledPanel' "ColorPicker" .outlined theme do
    caption' "Click and drag to select color:" theme
    let result ← colorPicker theme Color.red
    -- Display the current color value
    emitDynamic do
      let color ← result.color.sample
      let hsv ← result.hsv.sample
      let alpha ← result.alpha.sample
      let r := (color.r * 255).floor.toUInt8
      let g := (color.g * 255).floor.toUInt8
      let b := (color.b * 255).floor.toUInt8
      let a := (alpha * 100).floor.toUInt8
      pure (caption s!"RGB({r}, {g}, {b}) H:{(hsv.h * 360).floor.toUInt16}° α:{a}%" theme)

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
    emitDynamic do
      let low ← result.low.sample
      let high ← result.high.sample
      let lowPct := (low * 100.0).floor.toUInt32
      let highPct := (high * 100.0).floor.toUInt32
      pure (caption s!"Range: {lowPct}% - {highPct}%" theme)

/-- Stepper panel - demonstrates increment/decrement control. -/
def stepperPanel (theme : Theme) : WidgetM Unit :=
  titledPanel' "Stepper" .outlined theme do
    caption' "Click + or - to change value:" theme
    let config : StepperConfig := { min := 0, max := 20, step := 1, width := 160 }
    let result ← stepper theme 5 config
    emitDynamic do
      let value ← result.value.sample
      pure (caption s!"Value: {value}" theme)

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
    emitDynamic do
      let ratio ← result.ratio.sample
      let leftPct := (ratio * 100.0).floor.toUInt32
      let rightPct := ((1.0 - ratio) * 100.0).floor.toUInt32
      pure (caption s!"Split ratio: {leftPct}% / {rightPct}%" theme)

/-- Date picker panel - demonstrates calendar-based date selection. -/
def datePickerPanel (theme : Theme) : WidgetM Unit :=
  titledPanel' "Date Picker" .outlined theme do
    caption' "Click a day to select a date:" theme
    let initial : DatePickerDate := { year := 2026, month := 1, day := 11 }
    let result ← datePicker theme initial {}
    emitDynamic do
      let sel ← result.selected.sample
      match sel with
      | some date =>
          pure (caption s!"Selected: {date.year}-{date.month}-{date.day}" theme)
      | none =>
          pure (caption "Selected: (none)" theme)

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
          emitDynamic do
            let state ← scrollResult.scrollState.sample
            pure (caption s!"Y: {state.offsetY.floor.toUInt32}px" theme)

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
        emitDynamic do
          let count ← buttonClickCount.sample
          if count > 0 then pure (caption s!"(Clicks: {count})" theme)
          else pure (spacer 0 0)

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
