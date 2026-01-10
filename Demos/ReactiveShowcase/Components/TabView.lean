/-
  TabView Component - Self-contained FRP tab view with switchable content.
-/
import Reactive
import Afferent
import Afferent.Canopy
import Demos.ReactiveShowcase.Types
import Demos.ReactiveShowcase.Component

open Reactive Reactive.Host
open Afferent CanvasM
open Afferent.Arbor
open Afferent.Canopy
open Trellis

namespace Demos.ReactiveShowcase.Components

/-- A tab definition with label and content builder. -/
structure TabDef where
  label : String
  content : ComponentRender

/-- TabView component output - exposes tab state and render function. -/
structure TabViewComponent where
  /-- Event that fires with the newly selected tab index. -/
  onTabChange : Event Spider Nat
  /-- Currently active tab index as a Dynamic. -/
  activeTab : Dynamic Spider Nat
  /-- Render function that samples state and returns the tab view widget. -/
  render : ComponentRender

/-- Create a self-contained tab view component.
    The component manages its own hover and active tab state. -/
def tabView (tabs : Array TabDef) (theme : Theme) (initialTab : Nat)
    : ReactiveM TabViewComponent := do
  -- Auto-generate names via registry
  let containerName ← registerComponent "tabview" (isInteractive := false)

  -- Generate names for each tab header
  let mut headerNames : Array String := #[]
  for _ in tabs do
    let name ← registerComponent "tab-header"
    headerNames := headerNames.push name
  let headerNameFn (i : Nat) : String := headerNames.getD i ""

  -- Get event streams
  let allClicks ← useAllClicks
  let allHovers ← useAllHovers

  -- Helper: find clicked tab index
  let findClickedTab (data : ClickData) : Option Nat :=
    (List.range tabs.size).findSome? fun i =>
      if hitWidget data (headerNameFn i) then some i else none

  -- Helper: find hovered tab index
  let findHoveredTab (data : HoverData) : Option Nat :=
    (List.range tabs.size).findSome? fun i =>
      if hitWidgetHover data (headerNameFn i) then some i else none

  -- Pure FRP: mapMaybeM + holdDyn for active tab
  let tabChanges ← Event.mapMaybeM findClickedTab allClicks
  let activeTab ← holdDyn initialTab tabChanges
  let onTabChange := tabChanges

  -- Pure FRP: mapM + holdDyn for hovered tab
  let hoverChanges ← Event.mapM findHoveredTab allHovers
  let hoveredTab ← holdDyn none hoverChanges

  -- Capture tabs for render closure
  let tabsRef := tabs

  -- Render function
  let render : ComponentRender := do
    let active ← activeTab.sample
    let hovered ← hoveredTab.sample

    -- Build tab definitions for visual
    let mut tabDefs : Array (String × WidgetBuilder) := #[]
    for tab in tabsRef do
      let content ← tab.content
      tabDefs := tabDefs.push (tab.label, content)

    pure (tabViewVisual containerName headerNameFn tabDefs active hovered theme)

  pure { onTabChange, activeTab, render }

end Demos.ReactiveShowcase.Components
