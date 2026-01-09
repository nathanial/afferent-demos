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

/-- Dropdown component output - exposes selection and render function. -/
structure DropdownComponent where
  /-- Event that fires with the newly selected index. -/
  onSelect : Event Spider Nat
  /-- Currently selected index as a Dynamic. -/
  selection : Dynamic Spider Nat
  /-- Whether the dropdown is open as a Dynamic. -/
  isOpen : Dynamic Spider Bool
  /-- Render function that samples state and returns the dropdown widget. -/
  render : ComponentRender

/-- Create a self-contained dropdown component.
    The component manages its own hover, open/close, and selection state. -/
def dropdown (options : Array String) (theme : Theme) (initialSelection : Nat)
    : ReactiveM DropdownComponent := do
  -- Auto-generate names via registry
  let events ← getEvents
  let containerName ← liftSpider <| SpiderM.liftIO <| events.registry.register "dropdown" (isInteractive := false)
  let triggerName ← liftSpider <| SpiderM.liftIO <| events.registry.register "dropdown-trigger"

  -- Generate names for each option
  let mut optionNames : Array String := #[]
  for _ in options do
    let name ← liftSpider <| SpiderM.liftIO <| events.registry.register "dropdown-option"
    optionNames := optionNames.push name
  let optionNameFn (i : Nat) : String := optionNames.getD i ""

  -- Create trigger hover state
  let isTriggerHovered ← useHover triggerName

  -- Get event streams
  let triggerClicks ← useClick triggerName
  let allClicks ← useAllClicks
  let allHovers ← useAllHovers

  -- Helper: find clicked option index
  let findClickedOption (data : ClickData) : Option Nat :=
    (List.range options.size).findSome? fun i =>
      if hitWidget data (optionNameFn i) then some i else none

  -- Helper: check if click is outside dropdown
  let isClickOutside (data : ClickData) : Bool :=
    !hitWidget data containerName && !hitWidget data triggerName

  -- Helper: find hovered option
  let findHoveredOption (data : HoverData) : Option Nat :=
    (List.range options.size).findSome? fun i =>
      if hitWidgetHover data (optionNameFn i) then some i else none

  -- Extract option clicks
  let optionClicks ← liftSpider <| Event.mapMaybeM findClickedOption allClicks

  -- Selection: updated on option clicks
  let selection ← liftSpider <| holdDyn initialSelection optionClicks
  let onSelect := optionClicks

  -- isOpen: use fixDynM because click-outside depends on current isOpen state
  let isOpen ← liftSpider <| SpiderM.fixDynM fun isOpenBehavior => do
    -- Toggle on trigger click
    let toggleEvents ← Event.mapM (fun _ => fun open_ => !open_) triggerClicks

    -- Close on option click
    let closeOnOption ← Event.mapM (fun _ => fun _ => false) optionClicks

    -- Close on click-outside (gated by isOpen)
    let outsideClicks ← Event.filterM isClickOutside allClicks
    let gatedOutside ← Event.gateM isOpenBehavior outsideClicks
    let closeOnOutside ← Event.mapM (fun _ => fun _ => false) gatedOutside

    -- Merge all state transitions (leftmost wins on simultaneous)
    let allTransitions ← Event.leftmostM [closeOnOption, closeOnOutside, toggleEvents]

    -- Apply transitions to state
    foldDyn (fun f s => f s) false allTransitions

  -- Hovered option: gated by isOpen
  let hoverChanges ← liftSpider <| Event.mapM findHoveredOption allHovers
  let gatedHover ← liftSpider <| Event.gateM isOpen.current hoverChanges
  -- When closed, reset hover to none
  let closeEvents ← liftSpider <| Event.filterM (fun open_ => !open_) isOpen.updated
  let resetHover ← liftSpider <| Event.mapM (fun _ => (none : Option Nat)) closeEvents
  let mergedHover ← liftSpider <| Event.mergeM gatedHover resetHover
  let hoveredOption ← liftSpider <| holdDyn none mergedHover

  -- Render function samples all state
  let render : ComponentRender := do
    let triggerHovered ← isTriggerHovered.sample
    let open_ ← isOpen.sample
    let sel ← selection.sample
    let hoverOpt ← hoveredOption.sample
    let triggerState : WidgetState := { hovered := triggerHovered, pressed := false, focused := false }
    pure (dropdownVisual containerName triggerName optionNameFn options sel open_ hoverOpt theme triggerState)

  pure { onSelect, selection, isOpen, render }

end Demos.ReactiveShowcase.Components
