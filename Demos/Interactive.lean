/-
  Interactive Demo - Counter with buttons using Arbor
-/
import Afferent
import Afferent.Widget
import Afferent.Arbor
import Afferent.Arbor.App.UI
import Trellis

open Afferent CanvasM
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

/-- Messages emitted by the Arbor UI. -/
inductive CounterMsg where
  | increment
  | decrement
  | reset
deriving Repr, Inhabited

def applyCounterMsg (s : CounterState) : CounterMsg → CounterState
  | .increment => s.increment
  | .decrement => s.decrement
  | .reset => s.reset

/-- Build the counter UI widget tree.
    Returns (widget, widgetIds) where widgetIds contains the button IDs for event handling. -/
def buildCounterWidget (fontId : FontId) (smallFontId : FontId) (counter : Int) (screenScale : Float)
    : Widget × CounterWidgetIds :=
  let s := fun (v : Float) => v * screenScale

  -- Build widget with explicit starting ID
  let widget := Afferent.Arbor.buildFrom 0 do
    center (style := { width := .percent 1.0, height := .percent 1.0 }) do
      column (gap := s 30) (style := { backgroundColor := some (Color.gray 0.15), padding := EdgeInsets.uniform (s 40) }) #[
        -- Title (ID 2)
        text' "Interactive Counter Demo" fontId Color.white .center,

        -- Counter display (ID 3, inner text ID 4)
        center (style := { backgroundColor := some (Color.gray 0.25), minWidth := some (s 200), minHeight := some (s 80), cornerRadius := s 12 }) do
          text' (toString counter) fontId Color.white .center,

        -- Button row (ID 5)
        row (gap := s 20) {} #[
          -- Decrement button "-" (ID 6)
          center (style := {
            backgroundColor := some (Color.hsv 0.0 0.6 0.5)  -- Red
            minWidth := some (s 80)
            minHeight := some (s 50)
            cornerRadius := s 8
          }) do
            text' "-" fontId Color.white .center,

          -- Reset button (ID 8)
          center (style := {
            backgroundColor := some (Color.hsv 0.6 0.6 0.5)  -- Blue
            minWidth := some (s 100)
            minHeight := some (s 50)
            cornerRadius := s 8
          }) do
            text' "Reset" smallFontId Color.white .center,

          -- Increment button "+" (ID 10)
          center (style := {
            backgroundColor := some (Color.hsv 0.33 0.6 0.5)  -- Green
            minWidth := some (s 80)
            minHeight := some (s 50)
            cornerRadius := s 8
          }) do
            text' "+" fontId Color.white .center
        ],

        -- Instructions (ID 12)
        text' "Click the buttons to change the counter" smallFontId (Color.gray 0.6) .center
      ]

  -- Widget IDs based on tree structure:
  -- 0: center (root)
  -- 1: column
  -- 2: title text
  -- 3: counter display center
  -- 4: counter value text
  -- 5: button row
  -- 6: decrement center
  -- 7: decrement text
  -- 8: reset center
  -- 9: reset text
  -- 10: increment center
  -- 11: increment text
  -- 12: instructions text
  let ids : CounterWidgetIds := {
    decrement := 6
    reset := 8
    increment := 10
  }

  (widget, ids)

def buttonClickHandler (msg : CounterMsg) : Handler CounterMsg :=
  fun _ ev =>
    match ev with
    | .mouseClick e =>
        if e.button == .left then
          { msgs := #[msg], stopPropagation := true }
        else
          {}
    | _ => {}

def buildCounterUI (fontId : FontId) (smallFontId : FontId) (counter : Int) (screenScale : Float)
    : UI CounterMsg :=
  let (widget, ids) := buildCounterWidget fontId smallFontId counter screenScale
  let handlers :=
    (HandlerRegistry.empty _)
      |>.insert ids.decrement (buttonClickHandler .decrement)
      |>.insert ids.reset (buttonClickHandler .reset)
      |>.insert ids.increment (buttonClickHandler .increment)
  { widget, handlers }

def stepInteractiveDemoFrame (c : Canvas) (reg : FontRegistry) (fontId : FontId) (smallFontId : FontId)
    (width height screenScale : Float) (contentOffsetX contentOffsetY : Float)
    (state : CounterState) (fontMedium : Font)
    : IO (Canvas × CounterState) := do
  let mut nextState := state
  let ui := buildCounterUI fontId smallFontId nextState.value screenScale
  let measureResult ← Afferent.runWithFonts reg (Afferent.Arbor.measureWidget ui.widget width height)
  let layouts := Trellis.layout measureResult.node width height
  let measuredWidget := measureResult.widget
  let click ← FFI.Window.getClick c.ctx.window
  match click with
  | some ce =>
    FFI.Window.clearClick c.ctx.window
    let localX := ce.x - contentOffsetX
    let localY := ce.y - contentOffsetY
    let ev : Event := .mouseClick {
      x := localX
      y := localY
      button := MouseButton.fromCode ce.button
      modifiers := Modifiers.fromBitmask ce.modifiers
    }
    let (_, msgs) := Afferent.Arbor.dispatchEvent ev measuredWidget layouts ui.handlers
    for msg in msgs do
      nextState := applyCounterMsg nextState msg
  | none => pure ()

  let c ← run' c do
    resetTransform
    let commands := Afferent.Arbor.collectCommandsWithDebug measuredWidget layouts
    Afferent.Widget.executeCommands reg commands
    setFillColor Color.white
    fillTextXY
      "Interactive Counter Demo - Click the buttons! (Space to advance)"
      (20 * screenScale) (30 * screenScale) fontMedium
  pure (c, nextState)

end Demos
