/-
  ReactiveShowcase App - Composes all components into the demo UI using WidgetM.
-/
import Reactive
import Afferent
import Afferent.Canopy
import Afferent.Canopy.Reactive
import Demos.Core.Demo
import Demos.Reactive.Showcase.Panels.Basic
import Demos.Reactive.Showcase.Panels.Display
import Demos.Reactive.Showcase.Panels.Input
import Demos.Reactive.Showcase.Panels.Data
import Demos.Reactive.Showcase.Panels.Layout
import Demos.Reactive.Showcase.Panels.Overlay
import Demos.Reactive.Showcase.Panels.Charts
import Demos.Reactive.Showcase.Panels.Misc

open Reactive Reactive.Host
open Afferent CanvasM
open Afferent.Arbor
open Afferent.Canopy
open Afferent.Canopy.Reactive
open Trellis

namespace Demos.ReactiveShowcase

/-! ## Tab Content Builders -/

/-- Controls tab: Labels, Buttons, Checkboxes, Radio, Switches, Badge, Chip, Avatar, Link -/
def controlsTabContent (fireButtonClick : Unit → IO Unit) : WidgetM Unit := do
  let colStyle : BoxStyle := { flexItem := some (FlexItem.growing 1) }
  flexRow' { FlexContainer.row 20 with alignItems := .flexStart } (style := {}) do
    column' (gap := 16) (style := colStyle) do
      labelsPanel
      let buttonClicks ← buttonsPanel
      performEvent_ (← Event.mapM (fun _ => fireButtonClick ()) buttonClicks)
      clickCounterPanel
      checkboxesPanel
      radioButtonsPanel
    column' (gap := 16) (style := colStyle) do
      switchesPanel
      badgePanel
      chipPanel
      avatarPanel
      linkPanel

/-- Input tab: Sliders, Stepper, Dropdowns, Text Inputs, Date/Color Pickers -/
def inputTabContent (font : Font) : WidgetM Unit := do
  let colStyle : BoxStyle := { flexItem := some (FlexItem.growing 1) }
  flexRow' { FlexContainer.row 20 with alignItems := .flexStart } (style := {}) do
    column' (gap := 16) (style := colStyle) do
      slidersPanel
      rangeSliderPanel
      stepperPanel
      dropdownPanel
      dependentDropdownsPanel
    column' (gap := 16) (style := colStyle) do
      searchInputPanel font
      comboBoxPanel font
      textInputsPanel font
      textAreaPanel font
      datePickerPanel
      timePickerPanel
      colorPickerPanel

/-- Layout tab: Panels, TabView, Scroll, Separator, Card, SplitPane, Toolbar, Sidebar -/
def layoutTabContent : WidgetM Unit := do
  let colStyle : BoxStyle := { flexItem := some (FlexItem.growing 1) }
  flexRow' { FlexContainer.row 20 with alignItems := .flexStart } (style := {}) do
    column' (gap := 16) (style := colStyle) do
      panelsPanel
      tabViewPanel
      scrollContainerPanel
      separatorPanel
    column' (gap := 16) (style := colStyle) do
      cardPanel
      splitPanePanel
      toolbarPanel
      sidebarPanel

/-- Data tab: Table, DataGrid, ListBox, Virtual List, Tree View, Pagination -/
def dataTabContent (font : Font) : WidgetM Unit := do
  column' (gap := 16) (style := {}) do
    tablePanel
    dataGridPanel font
    listBoxPanel
    virtualListPanel
    treeViewPanel
    paginationPanel

/-- Feedback tab: Progress, Tooltips, Modal, Toasts, Menus -/
def feedbackTabContent (smallFont : Font)
    (fireModalOpen : Unit → IO Unit)
    (fireToastInfo fireToastSuccess fireToastWarning fireToastError : Unit → IO Unit)
    : WidgetM Unit := do
  let colStyle : BoxStyle := { flexItem := some (FlexItem.growing 1) }
  flexRow' { FlexContainer.row 20 with alignItems := .flexStart } (style := {}) do
    column' (gap := 16) (style := colStyle) do
      progressBarsPanel
      tooltipsPanel smallFont
      let modalClick ← modalTriggerPanel
      performEvent_ (← Event.mapM (fun _ => fireModalOpen ()) modalClick)
    column' (gap := 16) (style := colStyle) do
      toastsPanel fireToastInfo fireToastSuccess fireToastWarning fireToastError
      menuPanel
      menuBarPanel

/-- Charts tab: All 19 chart panels -/
def chartsTabContent : WidgetM Unit := do
  let colStyle : BoxStyle := { flexItem := some (FlexItem.growing 1) }
  flexRow' { FlexContainer.row 20 with alignItems := .flexStart } (style := {}) do
    column' (gap := 16) (style := colStyle) do
      barChartPanel
      lineChartPanel
      areaChartPanel
      pieChartPanel
      donutChartPanel
      scatterPlotPanel
      horizontalBarChartPanel
    column' (gap := 16) (style := colStyle) do
      bubbleChartPanel
      histogramPanel
      boxPlotPanel
      heatmapPanel
      stackedBarChartPanel
      groupedBarChartPanel
    column' (gap := 16) (style := colStyle) do
      stackedAreaChartPanel
      radarChartPanel
      candlestickChartPanel
      waterfallChartPanel
      gaugeChartPanel
      funnelChartPanel
      treemapChartPanel
      sankeyDiagramPanel

/-! ## Main Application -/

/-- Application state returned from createApp. -/
structure AppState where
  /-- Render function that samples all component state and returns the complete UI. -/
  render : ComponentRender

/-- Create the complete reactive showcase application.
    Sets up all components and their interactions using WidgetM. -/
def createApp (env : DemoEnv) : ReactiveM AppState := do
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
      heading1' "Reactive Showcase"
      row' (gap := 16) (style := {}) do
        caption' "FRP-powered widget demo"
        let _ ← dynWidget buttonClickCount fun count =>
          if count > 0 then caption' s!"(Clicks: {count})"
          else spacer' 0 0

      -- Tabbed content layout (fills remaining space)
      let contentStyle : BoxStyle := {
        flexItem := some (FlexItem.growing 1)
        width := .percent 1.0
        height := .percent 1.0
      }
      column' (gap := 0) (style := contentStyle) do
        let tabs : Array TabDef := #[
          { label := "Controls", content := controlsTabContent fireButtonClick },
          { label := "Input", content := inputTabContent env.fontCanopy },
          { label := "Layout", content := layoutTabContent },
          { label := "Data", content := dataTabContent env.fontCanopy },
          { label := "Feedback", content := feedbackTabContent env.fontCanopySmall
              fireModalOpen fireToastInfo fireToastSuccess fireToastWarning fireToastError },
          { label := "Charts", content := chartsTabContent }
        ]
        let _ ← tabView tabs 0

      -- Modal overlay (renders on top when open)
      let modalResult ← modal "Sample Modal" do
        bodyText' "This is a modal dialog."
        bodyText' "Click outside, press Escape, or click a button to close."

      -- Wire modal open trigger to modal's openModal
      let openAction ← Event.mapM (fun _ => modalResult.openModal) modalOpenTrigger
      performEvent_ openAction

      -- Toast manager (renders toast notifications)
      let toastMgr ← toastManager

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
