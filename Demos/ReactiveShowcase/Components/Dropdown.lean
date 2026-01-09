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
def dropdown (containerName : String) (triggerName : String)
    (optionNameFn : Nat → String) (options : Array String) (theme : Theme)
    (initialSelection : Nat)
    : ReactiveM DropdownComponent := do
  let ctx ← liftSpider SpiderM.getTimelineCtx

  -- Create trigger hover state
  let isTriggerHovered ← useHover triggerName

  -- Create open/close state
  let (isOpen, setOpen) ← liftSpider <| SpiderM.liftIO <| Dynamic.new ctx false

  -- Create selection state
  let (selection, setSelection) ← liftSpider <| SpiderM.liftIO <| Dynamic.new ctx initialSelection

  -- Create hovered option state
  let (hoveredOption, setHoveredOption) ← liftSpider <| SpiderM.liftIO <| Dynamic.new ctx (none : Option Nat)

  -- Create onSelect event
  let (onSelect, fireSelect) ← liftSpider <| newTriggerEvent (t := Spider) (a := Nat)

  -- Wire trigger clicks to toggle open
  let triggerClicks ← useClick triggerName
  let _ ← liftSpider <| SpiderM.liftIO <| triggerClicks.subscribe fun _ => do
    let current ← isOpen.sample
    setOpen (!current)

  -- Wire all clicks (handles option clicks and click-outside)
  let allClicks ← useAllClicks
  let _ ← liftSpider <| SpiderM.liftIO <| allClicks.subscribe fun data => do
    let open_ ← isOpen.sample
    -- Check if an option was clicked
    let mut clickedOption : Option Nat := none
    for i in [:options.size] do
      if hitWidget data (optionNameFn i) then
        clickedOption := some i
        break
    match clickedOption with
    | some i =>
        setSelection i
        setOpen false
        fireSelect i
    | none =>
        -- Check for click-outside to close
        if open_ then
          let clickedDropdown := hitWidget data containerName
          let clickedTrigger := hitWidget data triggerName
          if !clickedDropdown && !clickedTrigger then
            setOpen false

  -- Wire hover events for options
  let allHovers ← useAllHovers
  let _ ← liftSpider <| SpiderM.liftIO <| allHovers.subscribe fun data => do
    let open_ ← isOpen.sample
    if open_ then
      let mut hoveredOpt : Option Nat := none
      for i in [:options.size] do
        if hitWidgetHover data (optionNameFn i) then
          hoveredOpt := some i
          break
      setHoveredOption hoveredOpt
    else
      setHoveredOption none

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
