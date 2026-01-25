/-
  Input Panels - Sliders, steppers, progress bars, dropdowns, and text inputs.
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

/-- Sliders panel - demonstrates slider input controls. -/
def slidersPanel (theme : Theme) : WidgetM Unit :=
  titledPanel' "Sliders" .outlined theme do
    caption' "Click to adjust value:" theme
    row' (gap := 24) (style := {}) do
      let _ ← slider (some "Volume") theme 0.3
      let _ ← slider (some "Brightness") theme 0.7
      pure ()

/-- Range slider panel - demonstrates dual-handle slider. -/
def rangeSliderPanel (theme : Theme) : WidgetM Unit :=
  titledPanel' "Range Slider" .outlined theme do
    caption' "Drag handles to select a range:" theme
    let result ← rangeSlider theme 0.2 0.8
    let combined ← Dynamic.zipWithM Prod.mk result.low result.high
    let _ ← dynWidget combined fun (low, high) => do
      let lowPct := (low * 100.0).floor.toUInt32
      let highPct := (high * 100.0).floor.toUInt32
      caption' s!"Range: {lowPct}% - {highPct}%" theme

/-- Stepper panel - demonstrates increment/decrement control. -/
def stepperPanel (theme : Theme) : WidgetM Unit :=
  titledPanel' "Stepper" .outlined theme do
    caption' "Click + or - to change value:" theme
    let config : StepperConfig := { min := 0, max := 20, step := 1, width := 160 }
    let result ← stepper theme 5 config
    let _ ← dynWidget result.value fun value =>
      caption' s!"Value: {value}" theme

/-- Progress bars panel - demonstrates determinate and indeterminate progress. -/
def progressBarsPanel (theme : Theme) : WidgetM Unit :=
  titledPanel' "Progress Bars" .outlined theme do
    caption' "Determinate and indeterminate progress:" theme
    column' (gap := 12) (style := {}) do
      let _ ← progressBar theme 0.65 .primary (some "Download") true
      let _ ← progressBar theme 0.3 .success (some "Upload") true
      let _ ← progressBar theme 0.85 .warning none true
      let _ ← progressBarIndeterminate theme .primary (some "Loading...")
      pure ()

/-- Dropdown panel - demonstrates dropdown selection. -/
def dropdownPanel (theme : Theme) : WidgetM Unit :=
  titledPanel' "Dropdown" .outlined theme do
    caption' "Click to open, select an option:" theme
    let dropdownOptions := #["Apple", "Banana", "Cherry", "Date", "Elderberry"]
    let _ ← dropdown dropdownOptions theme 0
    pure ()

/-- Dependent dropdowns panel - demonstrates dynWidget for dynamic widget rebuilding.
    The second dropdown's options change based on the first dropdown's selection. -/
def dependentDropdownsPanel (theme : Theme) : WidgetM Unit :=
  titledPanel' "Dependent Dropdowns" .outlined theme do
    caption' "Second dropdown options depend on first:" theme
    let categories := #["Fruits", "Vegetables", "Dairy"]
    let itemsForCategory (idx : Nat) : Array String :=
      match idx with
      | 0 => #["Apple", "Banana", "Cherry", "Orange"]
      | 1 => #["Carrot", "Broccoli", "Spinach", "Tomato"]
      | 2 => #["Milk", "Cheese", "Yogurt", "Butter"]
      | _ => #[]
    row' (gap := 16) (style := {}) do
      column' (gap := 4) (style := {}) do
        caption' "Category:" theme
        let catResult ← dropdown categories theme 0
        column' (gap := 4) (style := {}) do
          caption' "Item:" theme
          let _ ← dynWidget catResult.selection fun catIdx =>
            dropdown (itemsForCategory catIdx) theme 0
          pure ()

/-- Text inputs panel - demonstrates single-line text input. -/
def textInputsPanel (theme : Theme) (font : Afferent.Font) : WidgetM Unit :=
  titledPanel' "Text Inputs" .outlined theme do
    caption' "Click to focus, then type:" theme
    let _ ← textInput theme font "Enter text here..." ""
    let _ ← textInput theme font "Type something..." "Hello, World!"
    let _ ← passwordInput theme font "Enter password..." ""
    pure ()

/-- Text area panel - demonstrates multi-line text input. -/
def textAreaPanel (theme : Theme) (font : Afferent.Font) : WidgetM Unit :=
  titledPanel' "Text Area" .outlined theme do
    caption' "Multi-line text with word wrapping:" theme
    let _ ← textArea theme "Enter multi-line text..." {} font
    pure ()

/-- Search input panel - demonstrates search input with icon and clear button. -/
def searchInputPanel (theme : Theme) (font : Afferent.Font) : WidgetM Unit :=
  titledPanel' "Search Input" .outlined theme do
    caption' "Type to search, press Enter or click X to clear:" theme
    let result ← searchInput theme font "Search..." ""
    let _ ← dynWidget result.text fun text =>
      if text.isEmpty then
        caption' "No search query" theme
      else
        caption' s!"Searching for: \"{text}\"" theme

end Demos.ReactiveShowcase
