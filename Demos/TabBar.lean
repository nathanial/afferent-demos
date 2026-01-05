/-
  TabBar Widget - A horizontal tab bar for demo navigation
-/
import Afferent
import Afferent.Arbor

open Afferent
open Afferent.Arbor
open Trellis (EdgeInsets)

namespace Demos

/-- Configuration for a single tab. -/
structure TabConfig where
  id : Nat
  label : String
  isActive : Bool := false
deriving Repr, Inhabited

/-- Styling configuration for the tab bar. -/
structure TabBarStyle where
  height : Float := 44
  backgroundColor : Color := Color.gray 0.08
  tabGap : Float := 2
  tabPadding : Float := 16
  activeColor : Color := Color.gray 0.22
  inactiveColor : Color := Color.gray 0.12
  hoverColor : Color := Color.gray 0.18
  activeTextColor : Color := Color.white
  inactiveTextColor : Color := Color.gray 0.6
  cornerRadius : Float := 4
deriving Repr, Inhabited

/-- Result of building a tabbar. -/
structure TabBarResult where
  widget : Widget
  tabIds : Array WidgetId
  finalId : Nat
deriving Inhabited

/-- Build a single tab widget. Returns the tab widget and the clickable widget ID. -/
def buildTab (config : TabConfig) (fontId : FontId) (style : TabBarStyle)
    (screenScale : Float) : WidgetBuilder := do
  let s := fun (v : Float) => v * screenScale
  let bgColor := if config.isActive then style.activeColor else style.inactiveColor
  let textColor := if config.isActive then style.activeTextColor else style.inactiveTextColor

  -- The tab is a center container (clickable) with text inside
  center (style := {
    backgroundColor := some bgColor
    padding := EdgeInsets.symmetric (s style.tabPadding) 0
    minHeight := some (s (style.height - 8))
    cornerRadius := s style.cornerRadius
  }) do
    text' config.label fontId textColor .center

/-- Build a tab bar from an array of tab configurations.
    Returns the widget, an array mapping tab indices to their widget IDs, and the final ID counter. -/
def buildTabBar (tabs : Array TabConfig) (fontId : FontId)
    (style : TabBarStyle := {}) (screenScale : Float := 1.0) : TabBarResult :=
  let s := fun (v : Float) => v * screenScale

  -- Build all tabs and collect their IDs
  -- Each tab uses 2 IDs: one for the center container, one for the text
  -- So tab at index i has clickable ID at: startId + 1 + (i * 2)
  -- The row itself takes ID 0
  let startId := 0
  let rowId := startId
  let tabWidgets := tabs.mapIdx fun idx config =>
    -- Build tab at known ID position
    let tabStartId := rowId + 1 + idx * 2
    buildFrom tabStartId (buildTab config fontId style screenScale)

  -- Collect tab IDs (the center container IDs)
  let tabIds := tabs.mapIdx fun idx _ =>
    rowId + 1 + idx * 2

  -- Build the row containing all tabs
  let rowWidget := Widget.flex rowId none
    (Trellis.FlexContainer.row (s style.tabGap))
    {
      backgroundColor := some style.backgroundColor
      padding := { top := s 4, right := s 8, bottom := s 4, left := s 8 }
      height := .length (s style.height)
    }
    tabWidgets

  let finalId := rowId + 1 + tabs.size * 2

  { widget := rowWidget, tabIds := tabIds, finalId := finalId }

/-- Find which tab was clicked given a widget ID.
    Returns Some tabIndex if a tab was clicked, None otherwise. -/
def findClickedTab (tabIds : Array WidgetId) (clickedId : WidgetId) : Option Nat :=
  tabIds.findIdx? (· == clickedId)

end Demos
