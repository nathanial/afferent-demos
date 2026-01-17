/-
  Miscellaneous Panels - Color picker and date picker components.
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

end Demos.ReactiveShowcase
