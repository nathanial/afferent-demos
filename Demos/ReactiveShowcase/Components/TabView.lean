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
def tabView (containerName : String) (headerNameFn : Nat → String)
    (tabs : Array TabDef) (theme : Theme)
    (initialTab : Nat)
    : ReactiveM TabViewComponent := do
  let ctx ← liftSpider SpiderM.getTimelineCtx

  -- Create active tab state
  let (activeTab, setActiveTab) ← liftSpider <| SpiderM.liftIO <| Dynamic.new ctx initialTab

  -- Create hovered tab state (Option Nat)
  let (hoveredTab, setHoveredTab) ← liftSpider <| SpiderM.liftIO <| Dynamic.new ctx (none : Option Nat)

  -- Create onTabChange event
  let (onTabChange, fireTabChange) ← liftSpider <| newTriggerEvent (t := Spider) (a := Nat)

  -- Wire all clicks (check which tab header was clicked)
  let allClicks ← useAllClicks
  let _ ← liftSpider <| SpiderM.liftIO <| allClicks.subscribe fun data => do
    for i in [:tabs.size] do
      if hitWidget data (headerNameFn i) then
        setActiveTab i
        fireTabChange i
        break

  -- Wire hover events for tab headers
  let allHovers ← useAllHovers
  let _ ← liftSpider <| SpiderM.liftIO <| allHovers.subscribe fun data => do
    let mut hovered : Option Nat := none
    for i in [:tabs.size] do
      if hitWidgetHover data (headerNameFn i) then
        hovered := some i
        break
    setHoveredTab hovered

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
