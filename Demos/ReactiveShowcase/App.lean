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

/-- Sliders panel - demonstrates slider input controls. -/
def slidersPanel (theme : Theme) : WidgetM Unit :=
  titledPanel' "Sliders" .outlined theme do
    caption' "Click to adjust value:" theme
    row' (gap := 24) (style := {}) do
      let _ ← slider (some "Volume") theme 0.3
      let _ ← slider (some "Brightness") theme 0.7
      pure ()

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

        -- Middle column
        column' (gap := 16) (style := {}) do
          slidersPanel theme
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
