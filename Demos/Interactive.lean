/-
  Interactive Demo - Counter with buttons using Arbor
-/
import Afferent
import Afferent.Arbor
import Trellis

open Afferent
open Afferent.Arbor
open Trellis (EdgeInsets)

namespace Demos

/-- Button widget IDs for hit testing. -/
structure CounterWidgetIds where
  increment : WidgetId
  decrement : WidgetId
  reset : WidgetId
deriving Repr

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

def counterButtonIds (rootId : WidgetId) : CounterWidgetIds :=
  { decrement := rootId + 6
    reset := rootId + 8
    increment := rootId + 10 }

def counterWidget (fontId : FontId) (smallFontId : FontId) (counter : Int) (screenScale : Float)
    : WidgetBuilder := do
  let s := fun (v : Float) => v * screenScale
  center (style := {
    width := .percent 1.0
    height := .percent 1.0
    flexItem := some (Trellis.FlexItem.growing 1)
  }) do
    column (gap := s 30) (style := { backgroundColor := some (Color.gray 0.15), padding := EdgeInsets.uniform (s 40) }) #[
      text' "Interactive Counter Demo" fontId Color.white .center,
      center (style := { backgroundColor := some (Color.gray 0.25), minWidth := some (s 200), minHeight := some (s 80), cornerRadius := s 12 }) do
        text' (toString counter) fontId Color.white .center,
      row (gap := s 20) {} #[
        center (style := {
          backgroundColor := some (Color.hsv 0.0 0.6 0.5)
          minWidth := some (s 80)
          minHeight := some (s 50)
          cornerRadius := s 8
        }) do
          text' "-" fontId Color.white .center,
        center (style := {
          backgroundColor := some (Color.hsv 0.6 0.6 0.5)
          minWidth := some (s 100)
          minHeight := some (s 50)
          cornerRadius := s 8
        }) do
          text' "Reset" smallFontId Color.white .center,
        center (style := {
          backgroundColor := some (Color.hsv 0.33 0.6 0.5)
          minWidth := some (s 80)
          minHeight := some (s 50)
          cornerRadius := s 8
        }) do
          text' "+" fontId Color.white .center
      ],
      text' "Click the buttons to change the counter" smallFontId (Color.gray 0.6) .center
    ]

def handleCounterClick (state : CounterState) (rootId : WidgetId) (hitPath : Array WidgetId)
    (button : UInt8) : CounterState :=
  if button != 0 then
    state
  else
    let ids := counterButtonIds rootId
    if hitPath.any (· == ids.increment) then
      state.increment
    else if hitPath.any (· == ids.decrement) then
      state.decrement
    else if hitPath.any (· == ids.reset) then
      state.reset
    else
      state

end Demos
