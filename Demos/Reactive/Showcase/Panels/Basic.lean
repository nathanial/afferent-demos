/-
  Basic UI Panels - Labels, buttons, checkboxes, radio buttons, and switches.
-/
import Reactive
import Afferent
import Afferent.Canopy
import Afferent.Canopy.Reactive

open Reactive Reactive.Host
open Afferent CanvasM
open Afferent.Arbor
open Afferent.Canopy
open Afferent.Canopy.Reactive
open Trellis

namespace Demos.ReactiveShowcase

/-- Labels panel - demonstrates text styling variants. -/
def labelsPanel (theme : Theme) : WidgetM Unit :=
  titledPanel' "Labels" .outlined theme do
    heading1' "Heading 1" theme
    heading2' "Heading 2" theme
    heading3' "Heading 3" theme
    bodyText' "Body text - normal paragraph content" theme
    caption' "Caption - small muted text" theme

/-- Buttons panel - demonstrates button variants with click counter.
    Returns the merged click event for external wiring. -/
def buttonsPanel (theme : Theme) : WidgetM (Reactive.Event Spider Unit) :=
  titledPanel' "Buttons" .outlined theme do
    caption' "Click a button to increment the counter:" theme
    row' (gap := 8) (style := {}) do
      let c1 ← button "Primary" theme .primary
      let c2 ← button "Secondary" theme .secondary
      let c3 ← button "Outline" theme .outline
      let c4 ← button "Ghost" theme .ghost
      Event.leftmostM [c1, c2, c3, c4]

/-- Click counter panel - demonstrates a button that shows its own click count.
    Uses lower-level hooks with dynWidget for dynamic label updates. -/
def clickCounterPanel (theme : Theme) : WidgetM Unit :=
  titledPanel' "Click Counter" .outlined theme do
    caption' "Button displays its own click count:" theme
    -- Register the button for event handling
    let name ← registerComponentW "counter-button"
    let isHovered ← useHover name
    let onClick ← useClick name
    -- Count clicks using foldDyn
    let clickCount ← Reactive.foldDyn (fun _ n => n + 1) 0 onClick
    -- Emit button with dynamic label based on count
    let combined ← Dynamic.zipWithM Prod.mk clickCount isHovered
    let _ ← dynWidget combined fun (count, hovered) => do
      let state : WidgetState := { hovered, pressed := false, focused := false }
      let label := if count == 0 then "Click me!" else s!"Clicked {count} times"
      emit (pure (buttonVisual name label theme .primary state))

/-- Checkboxes panel - demonstrates checkbox toggle behavior. -/
def checkboxesPanel (theme : Theme) : WidgetM Unit :=
  titledPanel' "Checkboxes" .outlined theme do
    caption' "Click to toggle:" theme
    row' (gap := 24) (style := {}) do
      let _ ← checkbox "Option 1" theme false
      let _ ← checkbox "Option 2" theme true
      pure ()

/-- Radio buttons panel - demonstrates single-selection radio group. -/
def radioButtonsPanel (theme : Theme) : WidgetM Unit :=
  titledPanel' "Radio Buttons" .outlined theme do
    caption' "Click to select one option:" theme
    let radioOptions : Array RadioOption := #[
      { label := "Option 1", value := "option1" },
      { label := "Option 2", value := "option2" },
      { label := "Option 3", value := "option3" }
    ]
    let _ ← radioGroup radioOptions theme "option1"
    pure ()

/-- Switches panel - demonstrates iOS-style toggle switches. -/
def switchesPanel (theme : Theme) : WidgetM Unit :=
  titledPanel' "Switches" .outlined theme do
    caption' "Click to toggle:" theme
    row' (gap := 24) (style := {}) do
      let _ ← switch (some "Notifications") theme false
      let _ ← switch (some "Dark Mode") theme true
      pure ()

end Demos.ReactiveShowcase
