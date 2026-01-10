/-
  Dropdown Component - Self-contained FRP dropdown with open/close and selection.
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

/-- Dropdown result - events and dynamics. -/
structure DropdownResult where
  onSelect : Event Spider Nat
  selection : Dynamic Spider Nat
  isOpen : Dynamic Spider Bool

/-- Create a dropdown component using WidgetM.
    Emits the dropdown widget and returns selection state. -/
def dropdown (options : Array String) (theme : Theme) (initialSelection : Nat := 0)
    : WidgetM DropdownResult := do
  let containerName ← registerComponentW "dropdown" (isInteractive := false)
  let triggerName ← registerComponentW "dropdown-trigger"

  let mut optionNames : Array String := #[]
  for _ in options do
    let name ← registerComponentW "dropdown-option"
    optionNames := optionNames.push name
  let optionNameFn (i : Nat) : String := optionNames.getD i ""

  let isTriggerHovered ← useHover triggerName
  let triggerClicks ← useClick triggerName
  let allClicks ← useAllClicks
  let allHovers ← useAllHovers

  let findClickedOption (data : ClickData) : Option Nat :=
    (List.range options.size).findSome? fun i =>
      if hitWidget data (optionNameFn i) then some i else none

  let isClickOutside (data : ClickData) : Bool :=
    !hitWidget data containerName && !hitWidget data triggerName

  let findHoveredOption (data : HoverData) : Option Nat :=
    (List.range options.size).findSome? fun i =>
      if hitWidgetHover data (optionNameFn i) then some i else none

  let optionClicks ← Event.mapMaybeM findClickedOption allClicks
  let selection ← holdDyn initialSelection optionClicks
  let onSelect := optionClicks

  let isOpen ← SpiderM.fixDynM fun isOpenBehavior => do
    let toggleEvents ← Event.mapM (fun _ => fun open_ => !open_) triggerClicks
    let closeOnOption ← Event.mapM (fun _ => fun _ => false) optionClicks
    let outsideClicks ← Event.filterM isClickOutside allClicks
    let gatedOutside ← Event.gateM isOpenBehavior outsideClicks
    let closeOnOutside ← Event.mapM (fun _ => fun _ => false) gatedOutside
    let allTransitions ← Event.leftmostM [closeOnOption, closeOnOutside, toggleEvents]
    foldDyn (fun f s => f s) false allTransitions

  let hoverChanges ← Event.mapM findHoveredOption allHovers
  let gatedHover ← Event.gateM isOpen.current hoverChanges
  let closeEvents ← Event.filterM (fun open_ => !open_) isOpen.updated
  let resetHover ← Event.mapM (fun _ => (none : Option Nat)) closeEvents
  let mergedHover ← Event.mergeM gatedHover resetHover
  let hoveredOption ← holdDyn none mergedHover

  emit do
    let triggerHovered ← isTriggerHovered.sample
    let open_ ← isOpen.sample
    let sel ← selection.sample
    let hoverOpt ← hoveredOption.sample
    let triggerState : WidgetState := { hovered := triggerHovered, pressed := false, focused := false }
    pure (dropdownVisual containerName triggerName optionNameFn options sel open_ hoverOpt theme triggerState)

  pure { onSelect, selection, isOpen }

end Demos.ReactiveShowcase.Components
