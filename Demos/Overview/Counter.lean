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

def counterWidget (fontId : FontId) (smallFontId : FontId) (counter : Int) (screenScale : Float)
    : WidgetBuilder := do
  let s := fun (v : Float) => v * screenScale
  center (style := {
    width := .percent 1.0
    height := .percent 1.0
    flexItem := some (Trellis.FlexItem.growing 1)
  }) do
    column (gap := s 30) (style := { backgroundColor := some (Color.gray 0.15), padding := EdgeInsets.uniform (s 40) }) #[
      text' "Counter Demo" fontId Color.white .center,
      center (style := { backgroundColor := some (Color.gray 0.25), minWidth := some (s 200), minHeight := some (s 80), cornerRadius := s 12 }) do
        text' (toString counter) fontId Color.white .center,
      row (gap := s 20) {} #[
        namedCenter counterDecrementName (style := {
          backgroundColor := some (Color.hsv 0.0 0.6 0.5)
          minWidth := some (s 80)
          minHeight := some (s 50)
          cornerRadius := s 8
        }) do
          text' "-" fontId Color.white .center,
        namedCenter counterResetName (style := {
          backgroundColor := some (Color.hsv 0.6 0.6 0.5)
          minWidth := some (s 100)
          minHeight := some (s 50)
          cornerRadius := s 8
        }) do
          text' "Reset" smallFontId Color.white .center,
        namedCenter counterIncrementName (style := {
          backgroundColor := some (Color.hsv 0.33 0.6 0.5)
          minWidth := some (s 80)
          minHeight := some (s 50)
          cornerRadius := s 8
        }) do
          text' "+" fontId Color.white .center
      ],
      text' "Click the buttons to change the counter" smallFontId (Color.gray 0.6) .center
    ]

end Demos
