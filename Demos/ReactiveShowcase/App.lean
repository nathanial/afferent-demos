/-
  ReactiveShowcase App - Composes all components into the demo UI using WidgetM.
-/
import Reactive
import Afferent
import Afferent.Canopy
import Demos.Demo
import Demos.ReactiveShowcase.Types
import Demos.ReactiveShowcase.Inputs
import Demos.ReactiveShowcase.Component
import Demos.ReactiveShowcase.Components.Button
import Demos.ReactiveShowcase.Components.Checkbox
import Demos.ReactiveShowcase.Components.Switch
import Demos.ReactiveShowcase.Components.Slider
import Demos.ReactiveShowcase.Components.RadioGroup
import Demos.ReactiveShowcase.Components.Dropdown
import Demos.ReactiveShowcase.Components.TextInput
import Demos.ReactiveShowcase.Components.TextArea
import Demos.ReactiveShowcase.Components.TabView
import Demos.ReactiveShowcase.Components.Modal
import Demos.ReactiveShowcase.Components.ProgressBar
import Demos.ReactiveShowcase.Components.Toast

open Reactive Reactive.Host
open Afferent CanvasM
open Afferent.Arbor
open Afferent.Canopy
open Trellis

namespace Demos.ReactiveShowcase

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
  let (buttonClickTrigger, fireButtonClick) ← newTriggerEvent (t := Spider) (a := Unit)
  let buttonClickCount ← foldDyn (fun _ n => n + 1) 0 buttonClickTrigger

  -- 2. Modal open trigger (trigger button is in a panel, modal is at root)
  let (modalOpenTrigger, fireModalOpen) ← newTriggerEvent (t := Spider) (a := Unit)

  -- 3. Toast triggers (buttons in panel, toast manager at root)
  let (toastInfoTrigger, fireToastInfo) ← newTriggerEvent (t := Spider) (a := Unit)
  let (toastSuccessTrigger, fireToastSuccess) ← newTriggerEvent (t := Spider) (a := Unit)
  let (toastWarningTrigger, fireToastWarning) ← newTriggerEvent (t := Spider) (a := Unit)
  let (toastErrorTrigger, fireToastError) ← newTriggerEvent (t := Spider) (a := Unit)

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

      -- Two-column layout
      flexRow' { FlexContainer.row 20 with alignItems := .flexStart }
          (style := { flexItem := some (FlexItem.growing 1) }) do
        -- Left column
        column' (gap := 16) (style := {}) do
          -- Labels section (non-interactive)
          titledPanel' "Labels" .outlined theme do
            heading1' "Heading 1" theme
            heading2' "Heading 2" theme
            heading3' "Heading 3" theme
            bodyText' "Body text - normal paragraph content" theme
            caption' "Caption - small muted text" theme

          -- Buttons section
          titledPanel' "Buttons" .outlined theme do
            caption' "Click a button to increment the counter:" theme
            row' (gap := 8) (style := {}) do
              let c1 ← Components.button "Primary" theme .primary
              let c2 ← Components.button "Secondary" theme .secondary
              let c3 ← Components.button "Outline" theme .outline
              let c4 ← Components.button "Ghost" theme .ghost
              -- Wire all clicks to the pre-created trigger
              let merged ← Event.leftmostM [c1, c2, c3, c4]
              let fireAction ← Event.mapM (fun _ => fireButtonClick ()) merged
              performEvent_ fireAction

          -- Checkboxes section
          titledPanel' "Checkboxes" .outlined theme do
            caption' "Click to toggle:" theme
            row' (gap := 24) (style := {}) do
              let _ ← Components.checkbox "Option 1" theme false
              let _ ← Components.checkbox "Option 2" theme true
              pure ()

          -- Radio Buttons section
          titledPanel' "Radio Buttons" .outlined theme do
            caption' "Click to select one option:" theme
            let radioOptions : Array Components.RadioOption := #[
              { label := "Option 1", value := "option1" },
              { label := "Option 2", value := "option2" },
              { label := "Option 3", value := "option3" }
            ]
            let _ ← Components.radioGroup radioOptions theme "option1"
            pure ()

          -- Switches section
          titledPanel' "Switches" .outlined theme do
            caption' "Click to toggle:" theme
            row' (gap := 24) (style := {}) do
              let _ ← Components.switch (some "Notifications") theme false
              let _ ← Components.switch (some "Dark Mode") theme true
              pure ()

          -- Modal section
          titledPanel' "Modal" .outlined theme do
            caption' "Click button to open modal:" theme
            let triggerClick ← Components.button "Open Modal" theme .primary
            let fireAction ← Event.mapM (fun _ => fireModalOpen ()) triggerClick
            performEvent_ fireAction

          -- Toast section
          titledPanel' "Toasts" .outlined theme do
            caption' "Click to show notifications:" theme
            row' (gap := 8) (style := {}) do
              let infoClick ← Components.button "Info" theme .primary
              let successClick ← Components.button "Success" theme .primary
              let warnClick ← Components.button "Warning" theme .secondary
              let errorClick ← Components.button "Error" theme .secondary
              -- Wire clicks to toast triggers
              let infoAction ← Event.mapM (fun _ => fireToastInfo ()) infoClick
              let successAction ← Event.mapM (fun _ => fireToastSuccess ()) successClick
              let warnAction ← Event.mapM (fun _ => fireToastWarning ()) warnClick
              let errorAction ← Event.mapM (fun _ => fireToastError ()) errorClick
              performEvent_ infoAction
              performEvent_ successAction
              performEvent_ warnAction
              performEvent_ errorAction

        -- Right column
        column' (gap := 16) (style := {}) do
          -- Sliders section
          titledPanel' "Sliders" .outlined theme do
            caption' "Click to adjust value:" theme
            row' (gap := 24) (style := {}) do
              let _ ← Components.slider (some "Volume") theme 0.3
              let _ ← Components.slider (some "Brightness") theme 0.7
              pure ()

          -- Progress Bars section
          titledPanel' "Progress Bars" .outlined theme do
            caption' "Determinate and indeterminate progress:" theme
            column' (gap := 12) (style := {}) do
              let _ ← Components.progressBar theme 0.65 .primary (some "Download") true
              let _ ← Components.progressBar theme 0.3 .success (some "Upload") true
              let _ ← Components.progressBar theme 0.85 .warning none true
              let _ ← Components.indeterminateProgressBar theme .primary (some "Loading...")
              pure ()

          -- Dropdown section
          titledPanel' "Dropdown" .outlined theme do
            caption' "Click to open, select an option:" theme
            let dropdownOptions := #["Apple", "Banana", "Cherry", "Date", "Elderberry"]
            let _ ← Components.dropdown dropdownOptions theme 0
            pure ()

          -- Text Input section
          titledPanel' "Text Inputs" .outlined theme do
            caption' "Click to focus, then type:" theme
            let _ ← Components.textInput theme "Enter text here..." ""
            let _ ← Components.textInput theme "Type something..." "Hello, World!"
            pure ()

          -- Text Area section
          titledPanel' "Text Area" .outlined theme do
            caption' "Multi-line text with word wrapping:" theme
            let _ ← Components.textArea theme "Enter multi-line text..." {} env.fontCanopy
            pure ()

          -- Panels section
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

          -- TabView section
          titledPanel' "Tab View" .outlined theme do
            caption' "Click tabs to switch content:" theme
            let tabs : Array Components.TabDef := #[
              { label := "Overview", content := do
                  bodyText' "TabView organizes content into separate panels." theme
                  bodyText' "Click a tab to switch between panels." theme
              },
              { label := "Settings", content := do
                  caption' "Sample settings panel:" theme
                  row' (gap := 16) (style := {}) do
                    let _ ← Components.checkbox "Enable feature" theme false
                    pure ()
              },
              { label := "About", content := do
                  heading3' "Reactive Widgets" theme
                  caption' "Version 1.0.0" theme
              }
            ]
            let _ ← Components.tabView tabs theme 0
            pure ()

      -- Modal overlay (renders on top when open)
      let modalResult ← Components.modal "Sample Modal" theme do
        bodyText' "This is a modal dialog." theme
        bodyText' "Click outside, press Escape, or click a button to close." theme

      -- Wire modal open trigger to modal's openModal
      let openAction ← Event.mapM (fun _ => modalResult.openModal) modalOpenTrigger
      performEvent_ openAction

      -- Toast manager (renders toast notifications)
      let toastMgr ← Components.toastManager theme

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
