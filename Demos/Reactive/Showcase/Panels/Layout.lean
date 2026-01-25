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

/-- Separator panel - demonstrates horizontal and vertical dividers. -/
def separatorPanel (theme : Theme) : WidgetM Unit :=
  titledPanel' "Separator" .outlined theme do
    caption' "Horizontal and vertical dividers:" theme
    column' (gap := 0) (style := { width := .length 300 }) do
      bodyText' "Section 1" theme
      hseparator' theme
      bodyText' "Section 2" theme
      hseparator' theme 2 12
      bodyText' "Section 3 (thicker)" theme

    row' (gap := 0) (style := { height := .length 80, margin := { top := 12 } }) do
      column' (gap := 4) (style := { flexItem := some (FlexItem.growing 1) }) do
        caption' "Left" theme
      vseparator' theme
      column' (gap := 4) (style := { flexItem := some (FlexItem.growing 1) }) do
        caption' "Center" theme
      vseparator' theme 2 12
      column' (gap := 4) (style := { flexItem := some (FlexItem.growing 1) }) do
        caption' "Right" theme

/-- Card panel - demonstrates card containers with headers. -/
def cardPanel (theme : Theme) : WidgetM Unit :=
  titledPanel' "Card" .outlined theme do
    caption' "Cards with optional headers:" theme
    row' (gap := 12) (style := {}) do
      -- Simple elevated card
      elevatedCard' theme 12 do
        column' (gap := 4) (style := { minWidth := some 100 }) do
          heading3' "Elevated" theme
          caption' "Simple card" theme

      -- Outlined card
      outlinedCard' theme 12 do
        column' (gap := 4) (style := { minWidth := some 100 }) do
          heading3' "Outlined" theme
          caption' "Border only" theme

      -- Card with header
      cardWithHeader' "Settings" .elevated theme do
        caption' "Enable notifications" theme
        let _ ← checkbox "Email alerts" theme true
        let _ ← checkbox "Push alerts" theme false
        pure ()

/-- Toolbar panel - demonstrates horizontal action buttons. -/
def toolbarPanel (theme : Theme) : WidgetM Unit :=
  titledPanel' "Toolbar" .outlined theme do
    caption' "Horizontal action buttons:" theme

    -- Simple toolbar with default styling
    let result1 ← simpleToolbar #["New", "Open", "Save"] theme .filled
    let _ ← dynWidget (← Reactive.holdDyn "" result1.onAction) fun action =>
      if action.isEmpty then spacer' 0 0
      else caption' s!"Clicked: {action}" theme

    hseparator' theme 1 12

    -- Outlined variant
    caption' "Outlined variant:" theme
    let result2 ← simpleToolbar #["Cut", "Copy", "Paste"] theme .outlined
    let _ ← dynWidget (← Reactive.holdDyn "" result2.onAction) fun action =>
      if action.isEmpty then spacer' 0 0
      else caption' s!"Clicked: {action}" theme

    hseparator' theme 1 12

    -- Floating variant with custom actions
    caption' "Floating with custom buttons:" theme
    let actions : Array ToolbarAction := #[
      { id := "bold", label := "B", variant := .ghost },
      { id := "italic", label := "I", variant := .ghost },
      { id := "underline", label := "U", variant := .ghost },
      { id := "link", label := "Link", variant := .outline }
    ]
    let result3 ← toolbar actions theme .floating
    let _ ← dynWidget (← Reactive.holdDyn "" result3.onAction) fun action =>
      if action.isEmpty then spacer' 0 0
      else caption' s!"Action: {action}" theme

/-- Sidebar panel - demonstrates collapsible navigation sidebar. -/
def sidebarPanel (theme : Theme) : WidgetM Unit :=
  titledPanel' "Sidebar" .outlined theme do
    caption' "Collapsible sidebar with toggle:" theme

    let config : SidebarConfig := {
      width := 180
      collapsedWidth := 40
      initiallyCollapsed := false
      showToggle := true
    }

    let ((_, _), result) ← sidebar config theme
      (fun collapsed => do
        if collapsed then do
          caption' "•" theme
          caption' "•" theme
          caption' "•" theme
        else do
          heading3' "Navigation" theme
          hseparator' theme 1 4
          bodyText' "Dashboard" theme
          bodyText' "Projects" theme
          bodyText' "Settings" theme
      )
      (do
        column' (gap := 8) (style := {
          padding := EdgeInsets.uniform 12
          backgroundColor := some (theme.panel.background.withAlpha 0.1)
          width := .percent 1.0
          height := .length 150
        }) do
          heading3' "Main Content" theme
          bodyText' "Click the arrow to toggle sidebar." theme
      )

    let _ ← dynWidget result.isCollapsed fun collapsed =>
      caption' s!"Sidebar is {if collapsed then "collapsed" else "expanded"}" theme

end Demos.ReactiveShowcase
