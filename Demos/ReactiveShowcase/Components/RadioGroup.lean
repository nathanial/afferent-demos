/-
  RadioGroup Component - Self-contained FRP radio button group.
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

/-- A single radio button option. -/
structure RadioOption where
  name : String
  label : String
  value : String

/-- RadioGroup component output - exposes selection and render function. -/
structure RadioGroupComponent where
  /-- Event that fires with the newly selected value. -/
  onSelect : Event Spider String
  /-- Currently selected value as a Dynamic. -/
  selected : Dynamic Spider String
  /-- Render function that samples state and returns all radio buttons. -/
  render : ComponentRender

/-- Create a self-contained radio group component.
    The component manages its own hover states and selection. -/
def radioGroup (options : Array RadioOption) (theme : Theme)
    (initialSelection : String)
    : ReactiveM RadioGroupComponent := do
  -- Create selection state using proper FRP pattern
  let (selectedEvent, fireSelected) ← liftSpider <| newTriggerEvent (t := Spider) (a := String)
  let selected ← liftSpider <| holdDyn initialSelection selectedEvent

  -- Create hovered option state
  let (hoveredEvent, fireHovered) ← liftSpider <| newTriggerEvent (t := Spider) (a := Option String)
  let hoveredOption ← liftSpider <| holdDyn none hoveredEvent

  -- onSelect is the same as selectedEvent for external consumers
  let onSelect := selectedEvent

  -- Wire all clicks (check which option was clicked)
  let allClicks ← useAllClicks
  let _ ← liftSpider <| SpiderM.liftIO <| allClicks.subscribe fun data => do
    for opt in options do
      if hitWidget data opt.name then
        fireSelected opt.value
        break

  -- Wire hover events for options
  let allHovers ← useAllHovers
  let _ ← liftSpider <| SpiderM.liftIO <| allHovers.subscribe fun data => do
    let mut hovered : Option String := none
    for opt in options do
      if hitWidgetHover data opt.name then
        hovered := some opt.name
        break
    fireHovered hovered

  -- Capture options for render closure
  let optionsRef := options

  -- Render function samples all states and returns radio buttons in a column
  let render : ComponentRender := do
    let selectedValue ← selected.sample
    let hoveredName ← hoveredOption.sample
    let mut builders : Array WidgetBuilder := #[]
    for opt in optionsRef do
      let isHovered := hoveredName == some opt.name
      let isSelected := selectedValue == opt.value
      let state : WidgetState := { hovered := isHovered, pressed := false, focused := false }
      builders := builders.push (radioButtonVisual opt.name opt.label theme isSelected state)
    pure (column (gap := 8) (style := {}) builders)

  pure { onSelect, selected, render }

end Demos.ReactiveShowcase.Components
