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

/-- A tab definition with label and WidgetM content builder. -/
structure TabDef where
  label : String
  content : WidgetM Unit

instance : Inhabited TabDef where
  default := { label := "", content := pure () }

/-- TabView result - events and dynamics. -/
structure TabViewResult where
  onTabChange : Event Spider Nat
  activeTab : Dynamic Spider Nat

/-- Create a tab view component using WidgetM.
    Emits the tab view widget and returns tab state. -/
def tabView (tabs : Array TabDef) (theme : Theme) (initialTab : Nat := 0)
    : WidgetM TabViewResult := do
  let containerName ← registerComponentW "tabview" (isInteractive := false)

  let mut headerNames : Array String := #[]
  for _ in tabs do
    let name ← registerComponentW "tab-header"
    headerNames := headerNames.push name
  let headerNameFn (i : Nat) : String := headerNames.getD i ""

  -- Pre-run all tab contents to get their renders
  let mut tabContentRenders : Array (Array ComponentRender) := #[]
  for tab in tabs do
    let (_, renders) ← runWidgetChildren tab.content
    tabContentRenders := tabContentRenders.push renders

  let allClicks ← useAllClicks
  let allHovers ← useAllHovers

  let findClickedTab (data : ClickData) : Option Nat :=
    (List.range tabs.size).findSome? fun i =>
      if hitWidget data (headerNameFn i) then some i else none

  let findHoveredTab (data : HoverData) : Option Nat :=
    (List.range tabs.size).findSome? fun i =>
      if hitWidgetHover data (headerNameFn i) then some i else none

  let tabChanges ← Event.mapMaybeM findClickedTab allClicks
  let activeTab ← holdDyn initialTab tabChanges
  let onTabChange := tabChanges

  let hoverChanges ← Event.mapM findHoveredTab allHovers
  let hoveredTab ← holdDyn none hoverChanges

  let tabsRef := tabs

  emit do
    let active ← activeTab.sample
    let hovered ← hoveredTab.sample

    let mut tabDefs : Array (String × WidgetBuilder) := #[]
    for i in [:tabsRef.size] do
      let tab := tabsRef[i]!
      let renders := tabContentRenders[i]!
      let contentWidgets ← renders.mapM id
      let content := column (gap := 0) (style := {}) contentWidgets
      tabDefs := tabDefs.push (tab.label, content)

    pure (tabViewVisual containerName headerNameFn tabDefs active hovered theme)

  pure { onTabChange, activeTab }

end Demos.ReactiveShowcase.Components
