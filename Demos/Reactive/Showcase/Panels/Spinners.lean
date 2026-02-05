/-
  Spinner Panels - Loading indicator animations.
-/
import Reactive
import Afferent
import Afferent.Canopy
import Afferent.Canopy.Reactive
import AfferentSpinners.Canopy.Widget.Display.Spinner

open Reactive Reactive.Host
open Afferent CanvasM
open Afferent.Arbor
open Afferent.Canopy
open Afferent.Canopy.Reactive
open Trellis

namespace Demos.ReactiveShowcase

/-- Helper to create a spinner demo with label. -/
private def spinnerDemo (label : String) (variant : AfferentSpinners.Canopy.SpinnerVariant) : WidgetM Unit := do
  flexColumn' { FlexContainer.column 8 with alignItems := .center } (style := {}) do
    AfferentSpinners.Canopy.spinner { variant, dims := { size := 48 } }
    caption' label

/-- Standard Spinners panel - common loading indicators. -/
def standardSpinnersPanel : WidgetM Unit :=
  titledPanel' "Standard Spinners" .outlined do
    caption' "Common loading indicator styles:"
    row' (gap := 32) (style := { padding := EdgeInsets.uniform 16 }) do
      spinnerDemo "Circle Dots" .circleDots
      spinnerDemo "Ring" .ring
      spinnerDemo "Bouncing Dots" .bouncingDots
      spinnerDemo "Bars" .bars
      spinnerDemo "Dual Ring" .dualRing

/-- Creative Spinners panel - unique animated indicators. -/
def creativeSpinnersPanel : WidgetM Unit :=
  titledPanel' "Creative Spinners" .outlined do
    caption' "Unique animated loading styles:"
    row' (gap := 32) (style := { padding := EdgeInsets.uniform 16 }) do
      spinnerDemo "Orbit" .orbit
      spinnerDemo "Pulse" .pulse
      spinnerDemo "Helix" .helix
      spinnerDemo "Wave" .wave
      spinnerDemo "Spiral" .spiral

/-- More Creative Spinners panel - additional unique styles. -/
def moreCreativeSpinnersPanel : WidgetM Unit :=
  titledPanel' "More Creative Spinners" .outlined do
    caption' "Additional creative loading styles:"
    row' (gap := 32) (style := { padding := EdgeInsets.uniform 16 }) do
      spinnerDemo "Clock" .clock
      spinnerDemo "Pendulum" .pendulum
      spinnerDemo "Ripple" .ripple
      spinnerDemo "Heartbeat" .heartbeat
      spinnerDemo "Gears" .gears

/-- Spinner Sizes panel - demonstrates size variations. -/
def spinnerSizesPanel : WidgetM Unit :=
  titledPanel' "Spinner Sizes" .outlined do
    caption' "Different sizes using ring spinner:"
    flexRow' { FlexContainer.row 32 with alignItems := .center } (style := { padding := EdgeInsets.uniform 16 }) do
      flexColumn' { FlexContainer.column 8 with alignItems := .center } (style := {}) do
        AfferentSpinners.Canopy.spinner { variant := .ring, dims := { size := 24 } }
        caption' "24px"
      flexColumn' { FlexContainer.column 8 with alignItems := .center } (style := {}) do
        AfferentSpinners.Canopy.spinner { variant := .ring, dims := { size := 40 } }
        caption' "40px"
      flexColumn' { FlexContainer.column 8 with alignItems := .center } (style := {}) do
        AfferentSpinners.Canopy.spinner { variant := .ring, dims := { size := 64 } }
        caption' "64px"
      flexColumn' { FlexContainer.column 8 with alignItems := .center } (style := {}) do
        AfferentSpinners.Canopy.spinner { variant := .ring, dims := { size := 96 } }
        caption' "96px"

/-- Spinner Colors panel - demonstrates color variations. -/
def spinnerColorsPanel : WidgetM Unit :=
  titledPanel' "Spinner Colors" .outlined do
    caption' "Custom colors:"
    row' (gap := 32) (style := { padding := EdgeInsets.uniform 16 }) do
      flexColumn' { FlexContainer.column 8 with alignItems := .center } (style := {}) do
        AfferentSpinners.Canopy.spinner { variant := .ring, color := some (Color.rgba 0.2 0.6 1.0 1.0), dims := { size := 48 } }
        caption' "Blue"
      flexColumn' { FlexContainer.column 8 with alignItems := .center } (style := {}) do
        AfferentSpinners.Canopy.spinner { variant := .ring, color := some (Color.rgba 0.2 0.8 0.4 1.0), dims := { size := 48 } }
        caption' "Green"
      flexColumn' { FlexContainer.column 8 with alignItems := .center } (style := {}) do
        AfferentSpinners.Canopy.spinner { variant := .ring, color := some (Color.rgba 1.0 0.5 0.2 1.0), dims := { size := 48 } }
        caption' "Orange"
      flexColumn' { FlexContainer.column 8 with alignItems := .center } (style := {}) do
        AfferentSpinners.Canopy.spinner { variant := .ring, color := some (Color.rgba 0.9 0.2 0.4 1.0), dims := { size := 48 } }
        caption' "Red"
      flexColumn' { FlexContainer.column 8 with alignItems := .center } (style := {}) do
        AfferentSpinners.Canopy.spinner { variant := .ring, color := some (Color.rgba 0.7 0.4 0.9 1.0), dims := { size := 48 } }
        caption' "Purple"

/-- Spinner Speeds panel - demonstrates speed variations. -/
def spinnerSpeedsPanel : WidgetM Unit :=
  titledPanel' "Spinner Speeds" .outlined do
    caption' "Different animation speeds:"
    row' (gap := 32) (style := { padding := EdgeInsets.uniform 16 }) do
      flexColumn' { FlexContainer.column 8 with alignItems := .center } (style := {}) do
        AfferentSpinners.Canopy.spinner { variant := .ring, speed := 0.5, dims := { size := 48 } }
        caption' "0.5x"
      flexColumn' { FlexContainer.column 8 with alignItems := .center } (style := {}) do
        AfferentSpinners.Canopy.spinner { variant := .ring, speed := 1.0, dims := { size := 48 } }
        caption' "1.0x"
      flexColumn' { FlexContainer.column 8 with alignItems := .center } (style := {}) do
        AfferentSpinners.Canopy.spinner { variant := .ring, speed := 1.5, dims := { size := 48 } }
        caption' "1.5x"
      flexColumn' { FlexContainer.column 8 with alignItems := .center } (style := {}) do
        AfferentSpinners.Canopy.spinner { variant := .ring, speed := 2.0, dims := { size := 48 } }
        caption' "2.0x"

end Demos.ReactiveShowcase
