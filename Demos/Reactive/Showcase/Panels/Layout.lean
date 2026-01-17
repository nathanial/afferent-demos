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

end Demos.ReactiveShowcase
