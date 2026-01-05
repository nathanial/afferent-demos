/-
  Counter Widget - Counter with buttons using Arbor.
-/
import Afferent
import Afferent.Arbor
import Trellis

open Afferent
open Afferent.Arbor
open Trellis (EdgeInsets)

namespace Demos

/-- Counter state. -/
structure CounterState where
  value : Int := 0
deriving Repr

namespace CounterState

def initial : CounterState := {}

def increment (s : CounterState) : CounterState :=
  { s with value := s.value + 1 }

def decrement (s : CounterState) : CounterState :=
  { s with value := s.value - 1 }

def reset (s : CounterState) : CounterState :=
  { s with value := 0 }

end CounterState

def counterDecrementName : String := "counter-decrement"
def counterResetName : String := "counter-reset"
def counterIncrementName : String := "counter-increment"

/-- Counter content at fixed design size (no manual scaling).
    This is the actual counter UI that will be scaled by its container. -/
def counterContent (fontId : FontId) (smallFontId : FontId) (counter : Int) : WidgetBuilder := do
  column (gap := 30) (style := { backgroundColor := some (Color.gray 0.15), padding := EdgeInsets.uniform 40 }) #[
    text' "Counter Demo" fontId Color.white .center,
    center (style := { backgroundColor := some (Color.gray 0.25), minWidth := some 200, minHeight := some 80, cornerRadius := 12 }) do
      text' (toString counter) fontId Color.white .center,
    row (gap := 20) {} #[
      namedCenter counterDecrementName (style := {
        backgroundColor := some (Color.hsv 0.0 0.6 0.5)
        minWidth := some 80
        minHeight := some 50
        cornerRadius := 8
      }) do
        text' "-" fontId Color.white .center,
      namedCenter counterResetName (style := {
        backgroundColor := some (Color.hsv 0.6 0.6 0.5)
        minWidth := some 100
        minHeight := some 50
        cornerRadius := 8
      }) do
        text' "Reset" smallFontId Color.white .center,
      namedCenter counterIncrementName (style := {
        backgroundColor := some (Color.hsv 0.33 0.6 0.5)
        minWidth := some 80
        minHeight := some 50
        cornerRadius := 8
      }) do
        text' "+" fontId Color.white .center
    ],
    text' "Click the buttons to change the counter" smallFontId (Color.gray 0.6) .center
  ]

/-- Counter widget that uses contentScale to automatically fit its container.
    The counter is designed at a fixed size and scales down (or up) to fit. -/
def counterWidget (fontId : FontId) (smallFontId : FontId) (counter : Int) (_screenScale : Float)
    : WidgetBuilder := do
  -- Use contentScale to automatically scale the counter to fit the container
  -- The container fills the available space, and the content scales to fit
  center (style := {
    width := .percent 1.0
    height := .percent 1.0
    flexItem := some (Trellis.FlexItem.growing 1)
    -- Enable content scaling: contain mode keeps aspect ratio, allows upscale for larger screens
    contentScale := some { mode := .contain, allowUpscale := true }
  }) do
    counterContent fontId smallFontId counter

end Demos
