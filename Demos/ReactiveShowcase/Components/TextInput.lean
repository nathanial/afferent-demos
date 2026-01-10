/-
  TextInput Component - Self-contained FRP text input with focus and keyboard handling.
-/
import Reactive
import Afferent
import Afferent.Canopy
import Demos.ReactiveShowcase.Types
import Demos.ReactiveShowcase.Component

open Reactive Reactive.Host
open Afferent CanvasM
open Afferent.Arbor
open Afferent.Canopy
open Trellis

namespace Demos.ReactiveShowcase.Components

/-- TextInput result - events and dynamics. -/
structure TextInputResult where
  onChange : Event Spider String
  onFocus : Event Spider Unit
  onBlur : Event Spider Unit
  text : Dynamic Spider String
  isFocused : Dynamic Spider Bool

/-- Create a text input component using WidgetM.
    Emits the text input widget and returns text state. -/
def textInput (theme : Theme) (placeholder : String) (initialValue : String := "")
    : WidgetM TextInputResult := do
  let name ← registerComponentW "text-input" (isInput := true)
  let events ← getEventsW
  let focusedInput := events.registry.focusedInput
  let fireFocusedInput := events.registry.fireFocus

  let clicks ← useClick name
  let keyEvents ← useKeyboard

  let isFocused ← Dynamic.mapM (· == some name) focusedInput

  let focusChanges ← Dynamic.changesM focusedInput
  let focusEvents ← Event.filterM
    (fun (old, new) => old != some name && new == some name) focusChanges
  let onFocus ← Event.mapM (fun _ => ()) focusEvents
  let blurEvents ← Event.filterM
    (fun (old, new) => old == some name && new != some name) focusChanges
  let onBlur ← Event.mapM (fun _ => ()) blurEvents

  let notFocused ← Dynamic.mapM (· != some name) focusedInput
  let focusClicks ← Event.gateM notFocused.current clicks
  let focusAction ← Event.mapM (fun _ => fireFocusedInput (some name)) focusClicks
  performEvent_ focusAction

  let gatedKeys ← Event.gateM isFocused.current keyEvents
  let initialState : TextInputState := {
    value := initialValue
    cursor := initialValue.length
    cursorPixelX := 0.0
  }
  let textState ← foldDyn
    (fun keyData state => TextInput.handleKeyPress keyData.event state none)
    initialState gatedKeys

  let textChanges ← Dynamic.changesM textState
  let valueChanges ← Event.mapMaybeM
    (fun (old, new) => if old.value != new.value then some new.value else none)
    textChanges
  let onChange := valueChanges

  let text ← Dynamic.mapM (·.value) textState

  emit do
    let state ← textState.sample
    let focused ← focusedInput.sample
    let isFoc := focused == some name
    pure (textInputVisual name theme { state with focused := isFoc } placeholder)

  pure { onChange, onFocus, onBlur, text, isFocused }

/-- TextArea result - events and dynamics. -/
structure TextAreaResult where
  onChange : Event Spider String
  onFocus : Event Spider Unit
  onBlur : Event Spider Unit
  text : Dynamic Spider String
  isFocused : Dynamic Spider Bool

/-- Create a text area component using WidgetM.
    Emits the text area widget and returns text state. -/
def textArea (theme : Theme) (placeholder : String) (initialState : TextAreaState)
    (font : Afferent.Font) (width : Float := 280) (height : Float := 120)
    : WidgetM TextAreaResult := do
  let name ← registerComponentW "text-area" (isInput := true)
  let events ← getEventsW
  let focusedInput := events.registry.focusedInput
  let fireFocusedInput := events.registry.fireFocus

  let clicks ← useClick name
  let keyEvents ← useKeyboard

  let isFocused ← Dynamic.mapM (· == some name) focusedInput

  let focusChanges ← Dynamic.changesM focusedInput
  let focusEvents ← Event.filterM
    (fun (old, new) => old != some name && new == some name) focusChanges
  let onFocus ← Event.mapM (fun _ => ()) focusEvents
  let blurEvents ← Event.filterM
    (fun (old, new) => old == some name && new != some name) focusChanges
  let onBlur ← Event.mapM (fun _ => ()) blurEvents

  let notFocused ← Dynamic.mapM (· != some name) focusedInput
  let focusClicks ← Event.gateM notFocused.current clicks
  let focusAction ← Event.mapM (fun _ => fireFocusedInput (some name)) focusClicks
  performEvent_ focusAction

  let gatedKeys ← Event.gateM isFocused.current keyEvents
  let padding : Float := 8.0
  let contentWidth := width - padding * 2
  let viewportHeight := height - padding * 2
  let textState ← foldDynM
    (fun keyData state => do
      let updated := TextArea.handleKeyPress keyData.event state none
      let renderedState ← TextArea.computeRenderState font updated contentWidth padding
      pure (TextArea.scrollToCursor renderedState viewportHeight))
    initialState gatedKeys

  let textChanges ← Dynamic.changesM textState
  let valueChanges ← Event.mapMaybeM
    (fun (old, new) => if old.value != new.value then some new.value else none)
    textChanges
  let onChange := valueChanges

  let text ← Dynamic.mapM (·.value) textState

  emit do
    let state ← textState.sample
    let focused ← focusedInput.sample
    let isFoc := focused == some name
    pure (textAreaVisual name theme { state with focused := isFoc } placeholder width height)

  pure { onChange, onFocus, onBlur, text, isFocused }

end Demos.ReactiveShowcase.Components
