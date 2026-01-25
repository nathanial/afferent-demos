/-
  ReactiveShowcase App - Composes all components into the demo UI using WidgetM.
-/
import Reactive
import Afferent
import Afferent.Canopy
import Afferent.Canopy.Reactive
import Demos.Core.Demo
import Demos.Reactive.Showcase.Panels.Basic
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

      -- Three-column layout (fills remaining space)
      let contentStyle : BoxStyle := {
        flexItem := some (FlexItem.growing 1)
        width := .percent 1.0
        height := .percent 1.0
      }
      flexRow' { FlexContainer.row 20 with alignItems := .flexStart }
          (style := contentStyle) do
        -- Left column
        column' (gap := 16) (style := { flexItem := some (FlexItem.growing 1) }) do
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
        column' (gap := 16) (style := { flexItem := some (FlexItem.growing 1) }) do
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
        column' (gap := 16) (style := { flexItem := some (FlexItem.growing 1) }) do
          scrollContainerPanel theme
          separatorPanel theme
          cardPanel theme
          toolbarPanel theme
          sidebarPanel theme
          tooltipsPanel theme env.fontCanopySmall
          tablePanel theme
          dataGridPanel theme env.fontCanopy
          listBoxPanel theme
          virtualListPanel theme
          treeViewPanel theme

        -- Fourth column
        column' (gap := 16) (style := { flexItem := some (FlexItem.growing 1) }) do
          splitPanePanel theme
          datePickerPanel theme
          colorPickerPanel theme
          barChartPanel theme
          lineChartPanel theme
          areaChartPanel theme
          pieChartPanel theme

        -- Fifth column (Charts continued)
        column' (gap := 16) (style := { flexItem := some (FlexItem.growing 1) }) do
          donutChartPanel theme
          scatterPlotPanel theme
          horizontalBarChartPanel theme
          bubbleChartPanel theme
          histogramPanel theme
          boxPlotPanel theme
          heatmapPanel theme
          stackedBarChartPanel theme

        -- Sixth column (More charts)
        column' (gap := 16) (style := { flexItem := some (FlexItem.growing 1) }) do
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
