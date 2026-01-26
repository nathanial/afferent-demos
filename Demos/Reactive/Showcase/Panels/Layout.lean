/-
  Layout Panels - Panels, tab views, split panes, and scroll containers.
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

/-- Panels panel - demonstrates different panel styles. -/
def panelsPanel : WidgetM Unit :=
  titledPanel' "Panels" .outlined do
    row' (gap := 12) (style := {}) do
      elevatedPanel' 12 do
        column' (gap := 4) (style := { minWidth := some 100 }) do
          heading3' "Elevated"
          caption' "Card-like"
      outlinedPanel' 12 do
        column' (gap := 4) (style := { minWidth := some 100 }) do
          heading3' "Outlined"
          caption' "Border only"
      filledPanel' 12 do
        column' (gap := 4) (style := { minWidth := some 100 }) do
          heading3' "Filled"
          caption' "Solid bg"

/-- Tab view panel - demonstrates tabbed content switching. -/
def tabViewPanel : WidgetM Unit :=
  titledPanel' "Tab View" .outlined do
    caption' "Click tabs to switch content:"
    let tabs : Array TabDef := #[
      { label := "Overview", content := do
          bodyText' "TabView organizes content into separate panels."
          bodyText' "Click a tab to switch between panels."
      },
      { label := "Settings", content := do
          caption' "Sample settings panel:"
          row' (gap := 16) (style := {}) do
            let _ ← checkbox "Enable feature" false
            pure ()
      },
      { label := "About", content := do
          heading3' "Reactive Widgets"
          caption' "Version 1.0.0"
      }
    ]
    let _ ← tabView tabs 0
    pure ()

/-- Split pane panel - demonstrates draggable split container. -/
def splitPanePanel : WidgetM Unit := do
  let theme ← getThemeW
  titledPanel' "Split Pane" .outlined do
    caption' "Drag the divider to resize panes:"
    let config : SplitPaneConfig := {
      orientation := .horizontal
      initialRatio := 0.4
      minPaneSize := 120
      handleThickness := 6
      width := some 420
      height := some 200
    }
    let ((_, _), result) ← splitPane config
      (column' (gap := 6) (style := {
        padding := EdgeInsets.uniform 12
        backgroundColor := some (theme.panel.background.withAlpha 0.2)
        width := .percent 1.0
        height := .percent 1.0
      }) do
        heading3' "Navigator"
        caption' "Left pane"
      )
      (column' (gap := 6) (style := {
        padding := EdgeInsets.uniform 12
        backgroundColor := some (theme.panel.background.withAlpha 0.1)
        width := .percent 1.0
        height := .percent 1.0
      }) do
        heading3' "Details"
        caption' "Right pane"
        bodyText' "Resize me with the handle."
      )
    let _ ← dynWidget result.ratio fun ratio => do
      let leftPct := (ratio * 100.0).floor.toUInt32
      let rightPct := ((1.0 - ratio) * 100.0).floor.toUInt32
      caption' s!"Split ratio: {leftPct}% / {rightPct}%"

/-- Scroll container panel - demonstrates scrollable content viewport. -/
def scrollContainerPanel : WidgetM Unit :=
  titledPanel' "Scroll Container" .outlined do
    caption' "Scroll with mouse wheel or trackpad:"
    row' (gap := 16) (style := {}) do
      -- Scrollable list of items
      outlinedPanel' 0 do
        let (_, scrollResult) ← vscrollContainer 150 do
          column' (gap := 4) (style := { padding := EdgeInsets.uniform 8 }) do
            for i in [1:21] do
              bodyText' s!"Item {i} - Scroll to see more"
            pure ()

        -- Display current scroll position
        column' (gap := 4) (style := { padding := EdgeInsets.uniform 8 }) do
          caption' "Scroll position:"
          let _ ← dynWidget scrollResult.scrollState fun state =>
            caption' s!"Y: {state.offsetY.floor.toUInt32}px"

/-- Separator panel - demonstrates horizontal and vertical dividers. -/
def separatorPanel : WidgetM Unit :=
  titledPanel' "Separator" .outlined do
    caption' "Horizontal and vertical dividers:"
    column' (gap := 0) (style := { width := .length 300 }) do
      bodyText' "Section 1"
      hseparator'
      bodyText' "Section 2"
      hseparator' 2 12
      bodyText' "Section 3 (thicker)"

    row' (gap := 0) (style := { height := .length 80, margin := { top := 12 } }) do
      column' (gap := 4) (style := { flexItem := some (FlexItem.growing 1) }) do
        caption' "Left"
      vseparator'
      column' (gap := 4) (style := { flexItem := some (FlexItem.growing 1) }) do
        caption' "Center"
      vseparator' 2 12
      column' (gap := 4) (style := { flexItem := some (FlexItem.growing 1) }) do
        caption' "Right"

/-- Card panel - demonstrates card containers with headers. -/
def cardPanel : WidgetM Unit :=
  titledPanel' "Card" .outlined do
    caption' "Cards with optional headers:"
    row' (gap := 12) (style := {}) do
      -- Simple elevated card
      elevatedCard' 12 do
        column' (gap := 4) (style := { minWidth := some 100 }) do
          heading3' "Elevated"
          caption' "Simple card"

      -- Outlined card
      outlinedCard' 12 do
        column' (gap := 4) (style := { minWidth := some 100 }) do
          heading3' "Outlined"
          caption' "Border only"

      -- Card with header
      cardWithHeader' "Settings" .elevated do
        caption' "Enable notifications"
        let _ ← checkbox "Email alerts" true
        let _ ← checkbox "Push alerts" false
        pure ()

/-- Toolbar panel - demonstrates horizontal action buttons. -/
def toolbarPanel : WidgetM Unit :=
  titledPanel' "Toolbar" .outlined do
    caption' "Horizontal action buttons:"

    -- Simple toolbar with default styling
    let result1 ← simpleToolbar #["New", "Open", "Save"] .filled
    let _ ← dynWidget (← Reactive.holdDyn "" result1.onAction) fun action =>
      if action.isEmpty then spacer' 0 0
      else caption' s!"Clicked: {action}"

    hseparator' 1 12

    -- Outlined variant
    caption' "Outlined variant:"
    let result2 ← simpleToolbar #["Cut", "Copy", "Paste"] .outlined
    let _ ← dynWidget (← Reactive.holdDyn "" result2.onAction) fun action =>
      if action.isEmpty then spacer' 0 0
      else caption' s!"Clicked: {action}"

    hseparator' 1 12

    -- Floating variant with custom actions
    caption' "Floating with custom buttons:"
    let actions : Array ToolbarAction := #[
      { id := "bold", label := "B", variant := .ghost },
      { id := "italic", label := "I", variant := .ghost },
      { id := "underline", label := "U", variant := .ghost },
      { id := "link", label := "Link", variant := .outline }
    ]
    let result3 ← toolbar actions .floating
    let _ ← dynWidget (← Reactive.holdDyn "" result3.onAction) fun action =>
      if action.isEmpty then spacer' 0 0
      else caption' s!"Action: {action}"

/-- Sidebar panel - demonstrates collapsible navigation sidebar. -/
def sidebarPanel : WidgetM Unit := do
  let theme ← getThemeW
  titledPanel' "Sidebar" .outlined do
    caption' "Collapsible sidebar with toggle:"

    let config : SidebarConfig := {
      width := 180
      collapsedWidth := 40
      initiallyCollapsed := false
      showToggle := true
    }

    let ((_, _), result) ← sidebar config
      (fun collapsed => do
        if collapsed then do
          caption' "•"
          caption' "•"
          caption' "•"
        else do
          heading3' "Navigation"
          hseparator' 1 4
          bodyText' "Dashboard"
          bodyText' "Projects"
          bodyText' "Settings"
      )
      (do
        column' (gap := 8) (style := {
          padding := EdgeInsets.uniform 12
          backgroundColor := some (theme.panel.background.withAlpha 0.1)
          width := .percent 1.0
          height := .length 150
        }) do
          heading3' "Main Content"
          bodyText' "Click the arrow to toggle sidebar."
      )

    let _ ← dynWidget result.isCollapsed fun collapsed =>
      caption' s!"Sidebar is {if collapsed then "collapsed" else "expanded"}"

end Demos.ReactiveShowcase
