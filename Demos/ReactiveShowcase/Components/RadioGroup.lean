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
  label : String
  value : String

/-- RadioGroup result - events and dynamics. -/
structure RadioGroupResult where
  onSelect : Event Spider String
  selected : Dynamic Spider String

/-- Create a radio group component using WidgetM.
    Emits the radio group widget and returns selection state. -/
def radioGroup (options : Array RadioOption) (theme : Theme) (initialSelection : String)
    : WidgetM RadioGroupResult := do
  let mut optionNames : Array String := #[]
  for _ in options do
    let name ← registerComponentW "radio"
    optionNames := optionNames.push name

  let allClicks ← useAllClicks
  let allHovers ← useAllHovers

  let findClickedOption (data : ClickData) : Option String :=
    (options.zip optionNames).findSome? fun (opt, name) =>
      if hitWidget data name then some opt.value else none

  let findHoveredOption (data : HoverData) : Option String :=
    optionNames.findSome? fun name =>
      if hitWidgetHover data name then some name else none

  let selectionChanges ← Event.mapMaybeM findClickedOption allClicks
  let selected ← holdDyn initialSelection selectionChanges
  let onSelect := selectionChanges

  let hoverChanges ← Event.mapM findHoveredOption allHovers
  let hoveredOption ← holdDyn none hoverChanges

  let optionsWithNames := options.zip optionNames

  emit do
    let selectedValue ← selected.sample
    let hoveredName ← hoveredOption.sample
    let mut builders : Array WidgetBuilder := #[]
    for (opt, name) in optionsWithNames do
      let isHovered := hoveredName == some name
      let isSelected := selectedValue == opt.value
      let state : WidgetState := { hovered := isHovered, pressed := false, focused := false }
      builders := builders.push (radioButtonVisual name opt.label theme isSelected state)
    pure (column (gap := 8) (style := {}) builders)

  pure { onSelect, selected }

end Demos.ReactiveShowcase.Components
