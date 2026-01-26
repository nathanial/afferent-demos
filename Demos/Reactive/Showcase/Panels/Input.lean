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
def slidersPanel : WidgetM Unit :=
  titledPanel' "Sliders" .outlined do
    caption' "Click to adjust value:"
    row' (gap := 24) (style := {}) do
      let _ ← slider (some "Volume") 0.3
      let _ ← slider (some "Brightness") 0.7
      pure ()

/-- Range slider panel - demonstrates dual-handle slider. -/
def rangeSliderPanel : WidgetM Unit :=
  titledPanel' "Range Slider" .outlined do
    caption' "Drag handles to select a range:"
    let result ← rangeSlider 0.2 0.8
    let combined ← Dynamic.zipWithM Prod.mk result.low result.high
    let _ ← dynWidget combined fun (low, high) => do
      let lowPct := (low * 100.0).floor.toUInt32
      let highPct := (high * 100.0).floor.toUInt32
      caption' s!"Range: {lowPct}% - {highPct}%"

/-- Stepper panel - demonstrates increment/decrement control. -/
def stepperPanel : WidgetM Unit :=
  titledPanel' "Stepper" .outlined do
    caption' "Click + or - to change value:"
    let config : StepperConfig := { min := 0, max := 20, step := 1, width := 160 }
    let result ← stepper 5 config
    let _ ← dynWidget result.value fun value =>
      caption' s!"Value: {value}"

/-- Progress bars panel - demonstrates determinate and indeterminate progress. -/
def progressBarsPanel : WidgetM Unit :=
  titledPanel' "Progress Bars" .outlined do
    caption' "Determinate and indeterminate progress:"
    column' (gap := 12) (style := {}) do
      let _ ← progressBar 0.65 .primary (some "Download") true
      let _ ← progressBar 0.3 .success (some "Upload") true
      let _ ← progressBar 0.85 .warning none true
      let _ ← progressBarIndeterminate .primary (some "Loading...")
      pure ()

/-- Dropdown panel - demonstrates dropdown selection. -/
def dropdownPanel : WidgetM Unit :=
  titledPanel' "Dropdown" .outlined do
    caption' "Click to open, select an option:"
    let dropdownOptions := #["Apple", "Banana", "Cherry", "Date", "Elderberry"]
    let _ ← dropdown dropdownOptions 0
    pure ()

/-- Dependent dropdowns panel - demonstrates dynWidget for dynamic widget rebuilding.
    The second dropdown's options change based on the first dropdown's selection. -/
def dependentDropdownsPanel : WidgetM Unit :=
  titledPanel' "Dependent Dropdowns" .outlined do
    caption' "Second dropdown options depend on first:"
    let categories := #["Fruits", "Vegetables", "Dairy"]
    let itemsForCategory (idx : Nat) : Array String :=
      match idx with
      | 0 => #["Apple", "Banana", "Cherry", "Orange"]
      | 1 => #["Carrot", "Broccoli", "Spinach", "Tomato"]
      | 2 => #["Milk", "Cheese", "Yogurt", "Butter"]
      | _ => #[]
    row' (gap := 16) (style := {}) do
      column' (gap := 4) (style := {}) do
        caption' "Category:"
        let catResult ← dropdown categories 0
        column' (gap := 4) (style := {}) do
          caption' "Item:"
          let _ ← dynWidget catResult.selection fun catIdx =>
            dropdown (itemsForCategory catIdx) 0
          pure ()

/-- Text inputs panel - demonstrates single-line text input. -/
def textInputsPanel (font : Afferent.Font) : WidgetM Unit :=
  titledPanel' "Text Inputs" .outlined do
    caption' "Click to focus, then type:"
    let _ ← textInput font "Enter text here..." ""
    let _ ← textInput font "Type something..." "Hello, World!"
    let _ ← passwordInput font "Enter password..." ""
    pure ()

/-- Text area panel - demonstrates multi-line text input. -/
def textAreaPanel (font : Afferent.Font) : WidgetM Unit :=
  titledPanel' "Text Area" .outlined do
    caption' "Multi-line text with word wrapping:"
    let _ ← textArea "Enter multi-line text..." {} font
    pure ()

/-- Search input panel - demonstrates search input with icon and clear button. -/
def searchInputPanel (font : Afferent.Font) : WidgetM Unit :=
  titledPanel' "Search Input" .outlined do
    caption' "Type to search, press Enter or click X to clear:"
    let result ← searchInput font "Search..." ""
    let _ ← dynWidget result.text fun text =>
      if text.isEmpty then
        caption' "No search query"
      else
        caption' s!"Searching for: \"{text}\""

/-- ComboBox panel - demonstrates filterable dropdown with text input. -/
def comboBoxPanel (font : Afferent.Font) : WidgetM Unit :=
  titledPanel' "ComboBox" .outlined do
    caption' "Type to filter options:"
    row' (gap := 24) (style := {}) do
      column' (gap := 8) (style := {}) do
        caption' "Countries:"
        let countries := #[
          "United States", "United Kingdom", "Canada", "Australia",
          "Germany", "France", "Japan", "China", "India", "Brazil"
        ]
        let result ← comboBox countries font "Search countries..."
        let _ ← dynWidget result.value fun val =>
          if val.isEmpty then
            caption' "No selection"
          else
            caption' s!"Selected: {val}"
      column' (gap := 8) (style := {}) do
        caption' "With free text:"
        let tags := #["bug", "feature", "docs", "enhancement", "help wanted"]
        let result2 ← comboBox tags font "Add tag..." "" { allowFreeText := true }
        let _ ← dynWidget result2.value fun val =>
          if val.isEmpty then
            caption' "No tag"
          else
            caption' s!"Tag: {val}"

end Demos.ReactiveShowcase
