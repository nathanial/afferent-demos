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
  -- Get event streams
  let allClicks ← useAllClicks
  let allHovers ← useAllHovers

  -- Helper: find which option was clicked
  let findClickedOption (data : ClickData) : Option String :=
    options.findSome? fun opt => if hitWidget data opt.name then some opt.value else none

  -- Helper: find which option is hovered
  let findHoveredOption (data : HoverData) : Option String :=
    options.findSome? fun opt => if hitWidgetHover data opt.name then some opt.name else none

  -- Pure FRP: mapMaybeM extracts clicked option, holdDyn holds selection
  let selectionChanges ← liftSpider <| Event.mapMaybeM findClickedOption allClicks
  let selected ← liftSpider <| holdDyn initialSelection selectionChanges
  let onSelect := selectionChanges

  -- Pure FRP: mapM extracts hovered option, holdDyn holds current
  let hoverChanges ← liftSpider <| Event.mapM findHoveredOption allHovers
  let hoveredOption ← liftSpider <| holdDyn none hoverChanges

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
