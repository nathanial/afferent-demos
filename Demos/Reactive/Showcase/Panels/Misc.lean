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
def colorPickerPanel (theme : Theme) : WidgetM Unit :=
  titledPanel' "ColorPicker" .outlined theme do
    caption' "Click and drag to select color:" theme
    let result ← colorPicker theme Color.red
    -- Display the current color value
    let combined ← Dynamic.zipWith3M (fun a b c => (a, b, c)) result.color result.hsv result.alpha
    let _ ← dynWidget combined fun (color, hsv, alpha) => do
      let r := (color.r * 255).floor.toUInt8
      let g := (color.g * 255).floor.toUInt8
      let b := (color.b * 255).floor.toUInt8
      let a := (alpha * 100).floor.toUInt8
      caption' s!"RGB({r}, {g}, {b}) H:{(hsv.h * 360).floor.toUInt16}° α:{a}%" theme

/-- Date picker panel - demonstrates calendar-based date selection. -/
def datePickerPanel (theme : Theme) : WidgetM Unit :=
  titledPanel' "Date Picker" .outlined theme do
    caption' "Click a day to select a date:" theme
    let initial : DatePickerDate := { year := 2026, month := 1, day := 11 }
    let result ← datePicker theme initial {}
    let _ ← dynWidget result.selected fun sel =>
      match sel with
      | some date => caption' s!"Selected: {date.year}-{date.month}-{date.day}" theme
      | none => caption' "Selected: (none)" theme

/-- Time picker panel - demonstrates time selection with spinners. -/
def timePickerPanel (theme : Theme) : WidgetM Unit :=
  titledPanel' "Time Picker" .outlined theme do
    caption' "Click arrows to adjust time:" theme
    row' (gap := 24) (style := {}) do
      column' (gap := 8) (style := {}) do
        caption' "12-hour format:" theme
        let initial12 : TimeValue := { hours := 9, minutes := 30, seconds := 0 }
        let result12 ← timePicker theme initial12 { use24Hour := false }
        let _ ← dynWidget result12.value fun time =>
          caption' s!"Time: {time.format12}" theme
      column' (gap := 8) (style := {}) do
        caption' "24-hour (no seconds):" theme
        let initial24 : TimeValue := { hours := 14, minutes := 45, seconds := 0 }
        let result24 ← timePicker theme initial24 { use24Hour := true, showSeconds := false }
        let _ ← dynWidget result24.value fun time =>
          caption' s!"Time: {time.format24 (showSeconds := false)}" theme

end Demos.ReactiveShowcase
