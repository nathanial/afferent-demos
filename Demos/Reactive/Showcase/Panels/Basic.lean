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
def labelsPanel : WidgetM Unit :=
  titledPanel' "Labels" .outlined do
    heading1' "Heading 1"
    heading2' "Heading 2"
    heading3' "Heading 3"
    bodyText' "Body text - normal paragraph content"
    caption' "Caption - small muted text"

/-- Buttons panel - demonstrates button variants with click counter.
    Returns the merged click event for external wiring. -/
def buttonsPanel : WidgetM (Reactive.Event Spider Unit) :=
  titledPanel' "Buttons" .outlined do
    caption' "Click a button to increment the counter:"
    row' (gap := 8) (style := {}) do
      let c1 ← button "Primary" .primary
      let c2 ← button "Secondary" .secondary
      let c3 ← button "Outline" .outline
      let c4 ← button "Ghost" .ghost
      Event.leftmostM [c1, c2, c3, c4]

/-- Click counter panel - demonstrates a button that shows its own click count.
    Uses lower-level hooks with dynWidget for dynamic label updates. -/
def clickCounterPanel : WidgetM Unit := do
  let theme ← getThemeW
  titledPanel' "Click Counter" .outlined do
    caption' "Button displays its own click count:"
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
def checkboxesPanel : WidgetM Unit :=
  titledPanel' "Checkboxes" .outlined do
    caption' "Click to toggle:"
    row' (gap := 24) (style := {}) do
      let _ ← checkbox "Option 1" false
      let _ ← checkbox "Option 2" true
      pure ()

/-- Radio buttons panel - demonstrates single-selection radio group. -/
def radioButtonsPanel : WidgetM Unit :=
  titledPanel' "Radio Buttons" .outlined do
    caption' "Click to select one option:"
    let radioOptions : Array RadioOption := #[
      { label := "Option 1", value := "option1" },
      { label := "Option 2", value := "option2" },
      { label := "Option 3", value := "option3" }
    ]
    let _ ← radioGroup radioOptions "option1"
    pure ()

/-- Switches panel - demonstrates iOS-style toggle switches. -/
def switchesPanel : WidgetM Unit :=
  titledPanel' "Switches" .outlined do
    caption' "Click to toggle:"
    row' (gap := 24) (style := {}) do
      let _ ← switch (some "Notifications") false
      let _ ← switch (some "Dark Mode") true
      pure ()

end Demos.ReactiveShowcase
