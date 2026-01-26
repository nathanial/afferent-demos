/-
  Miscellaneous Panels - Color picker, date picker, and time picker components.
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

/-- ColorPicker panel - demonstrates HSV color picker widget. -/
def colorPickerPanel : WidgetM Unit :=
  titledPanel' "ColorPicker" .outlined do
    caption' "Click and drag to select color:"
    let result ← colorPicker Color.red
    -- Display the current color value
    let combined ← Dynamic.zipWith3M (fun a b c => (a, b, c)) result.color result.hsv result.alpha
    let _ ← dynWidget combined fun (color, hsv, alpha) => do
      let r := (color.r * 255).floor.toUInt8
      let g := (color.g * 255).floor.toUInt8
      let b := (color.b * 255).floor.toUInt8
      let a := (alpha * 100).floor.toUInt8
      caption' s!"RGB({r}, {g}, {b}) H:{(hsv.h * 360).floor.toUInt16}° α:{a}%"

/-- Date picker panel - demonstrates calendar-based date selection. -/
def datePickerPanel : WidgetM Unit :=
  titledPanel' "Date Picker" .outlined do
    caption' "Click a day to select a date:"
    let initial : DatePickerDate := { year := 2026, month := 1, day := 11 }
    let result ← datePicker initial {}
    let _ ← dynWidget result.selected fun sel =>
      match sel with
      | some date => caption' s!"Selected: {date.year}-{date.month}-{date.day}"
      | none => caption' "Selected: (none)"

/-- Time picker panel - demonstrates time selection with spinners. -/
def timePickerPanel : WidgetM Unit :=
  titledPanel' "Time Picker" .outlined do
    caption' "Click arrows to adjust time:"
    row' (gap := 24) (style := {}) do
      column' (gap := 8) (style := {}) do
        caption' "12-hour format:"
        let initial12 : TimeValue := { hours := 9, minutes := 30, seconds := 0 }
        let result12 ← timePicker initial12 { use24Hour := false }
        let _ ← dynWidget result12.value fun time =>
          caption' s!"Time: {time.format12}"
      column' (gap := 8) (style := {}) do
        caption' "24-hour (no seconds):"
        let initial24 : TimeValue := { hours := 14, minutes := 45, seconds := 0 }
        let result24 ← timePicker initial24 { use24Hour := true, showSeconds := false }
        let _ ← dynWidget result24.value fun time =>
          caption' s!"Time: {time.format24 (showSeconds := false)}"

end Demos.ReactiveShowcase
