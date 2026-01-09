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

/-- TextInput component output - exposes text state and render function. -/
structure TextInputComponent where
  /-- Event that fires with the new text value on change. -/
  onChange : Event Spider String
  /-- Event that fires when focus is gained. -/
  onFocus : Event Spider Unit
  /-- Event that fires when focus is lost. -/
  onBlur : Event Spider Unit
  /-- Current text value as a Dynamic. -/
  text : Dynamic Spider String
  /-- Whether this input is focused as a Dynamic. -/
  isFocused : Dynamic Spider Bool
  /-- Render function that samples state and returns the input widget. -/
  render : ComponentRender

/-- Create a self-contained text input component.
    The component manages its own hover, focus, and text state.
    Pass a shared focusedInput Dynamic to coordinate focus across multiple inputs.
    The fireFocusedInput function is the trigger for the shared focusedInput Dynamic. -/
def textInput (name : String) (theme : Theme) (placeholder : String)
    (initialValue : String)
    (focusedInput : Dynamic Spider (Option String))
    (fireFocusedInput : Option String → IO Unit)
    : ReactiveM TextInputComponent := do
  -- Get event streams
  let clicks ← useClick name
  let keyEvents ← useKeyboard

  -- Derive isFocused from shared focusedInput
  let isFocused ← liftSpider <| Dynamic.mapM (· == some name) focusedInput

  -- Detect focus/blur from focusedInput changes
  let focusChanges ← liftSpider <| Dynamic.changesM focusedInput
  let focusEvents ← liftSpider <| Event.filterM
    (fun (old, new) => old != some name && new == some name) focusChanges
  let onFocus ← liftSpider <| Event.mapM (fun _ => ()) focusEvents
  let blurEvents ← liftSpider <| Event.filterM
    (fun (old, new) => old == some name && new != some name) focusChanges
  let onBlur ← liftSpider <| Event.mapM (fun _ => ()) blurEvents

  -- Focus click: gate by not-already-focused, then call external fireFocusedInput
  -- Pure FRP: map to IO action and use performEvent_
  let notFocused ← liftSpider <| Dynamic.mapM (· != some name) focusedInput
  let focusClicks ← liftSpider <| Event.gateM notFocused.current clicks
  let focusAction ← liftSpider <| Event.mapM (fun _ => fireFocusedInput (some name)) focusClicks
  liftSpider <| performEvent_ focusAction

  -- Text state: gate keyboard by focused, accumulate with foldDyn
  let gatedKeys ← liftSpider <| Event.gateM isFocused.current keyEvents
  let initialState : TextInputState := {
    value := initialValue
    cursor := initialValue.length
    cursorPixelX := 0.0
  }
  let textState ← liftSpider <| foldDyn
    (fun keyData state => TextInput.handleKeyPress keyData.event state none)
    initialState gatedKeys

  -- onChange: detect when text value changes
  let textChanges ← liftSpider <| Dynamic.changesM textState
  let valueChanges ← liftSpider <| Event.mapMaybeM
    (fun (old, new) => if old.value != new.value then some new.value else none)
    textChanges
  let onChange := valueChanges

  -- Create text Dynamic (just the value string)
  let text ← liftSpider <| Dynamic.mapM (·.value) textState

  -- Render function
  let render : ComponentRender := do
    let state ← textState.sample
    let focused ← focusedInput.sample
    let isFoc := focused == some name
    pure (textInputVisual name theme { state with focused := isFoc } placeholder)

  pure { onChange, onFocus, onBlur, text, isFocused, render }

/-- TextArea component output - exposes text state and render function. -/
structure TextAreaComponent where
  /-- Event that fires with the new text value on change. -/
  onChange : Event Spider String
  /-- Event that fires when focus is gained. -/
  onFocus : Event Spider Unit
  /-- Event that fires when focus is lost. -/
  onBlur : Event Spider Unit
  /-- Current text value as a Dynamic. -/
  text : Dynamic Spider String
  /-- Whether this textarea is focused as a Dynamic. -/
  isFocused : Dynamic Spider Bool
  /-- Render function that samples state and returns the textarea widget. -/
  render : ComponentRender

/-- Create a self-contained text area component.
    Similar to textInput but with multi-line support.
    Requires a Font for text measurement during render state computation.
    The fireFocusedInput function is the trigger for the shared focusedInput Dynamic. -/
def textArea (name : String) (theme : Theme) (placeholder : String)
    (initialState : TextAreaState)
    (focusedInput : Dynamic Spider (Option String))
    (fireFocusedInput : Option String → IO Unit)
    (font : Afferent.Font)
    (width : Float := 280) (height : Float := 120)
    : ReactiveM TextAreaComponent := do
  -- Get event streams
  let clicks ← useClick name
  let keyEvents ← useKeyboard

  -- Derive isFocused from shared focusedInput
  let isFocused ← liftSpider <| Dynamic.mapM (· == some name) focusedInput

  -- Detect focus/blur from focusedInput changes
  let focusChanges ← liftSpider <| Dynamic.changesM focusedInput
  let focusEvents ← liftSpider <| Event.filterM
    (fun (old, new) => old != some name && new == some name) focusChanges
  let onFocus ← liftSpider <| Event.mapM (fun _ => ()) focusEvents
  let blurEvents ← liftSpider <| Event.filterM
    (fun (old, new) => old == some name && new != some name) focusChanges
  let onBlur ← liftSpider <| Event.mapM (fun _ => ()) blurEvents

  -- Focus click: gate by not-already-focused, then call external fireFocusedInput
  -- Pure FRP: map to IO action and use performEvent_
  let notFocused ← liftSpider <| Dynamic.mapM (· != some name) focusedInput
  let focusClicks ← liftSpider <| Event.gateM notFocused.current clicks
  let focusAction ← liftSpider <| Event.mapM (fun _ => fireFocusedInput (some name)) focusClicks
  liftSpider <| performEvent_ focusAction

  -- Text state: gate keyboard by focused, use foldDynM for IO operations (font measurement)
  let gatedKeys ← liftSpider <| Event.gateM isFocused.current keyEvents
  let padding : Float := 8.0
  let contentWidth := width - padding * 2
  let viewportHeight := height - padding * 2
  let textState ← liftSpider <| foldDynM
    (fun keyData state => do
      let updated := TextArea.handleKeyPress keyData.event state none
      let renderedState ← TextArea.computeRenderState font updated contentWidth padding
      pure (TextArea.scrollToCursor renderedState viewportHeight))
    initialState gatedKeys

  -- onChange: detect when text value changes
  let textChanges ← liftSpider <| Dynamic.changesM textState
  let valueChanges ← liftSpider <| Event.mapMaybeM
    (fun (old, new) => if old.value != new.value then some new.value else none)
    textChanges
  let onChange := valueChanges

  -- Create text Dynamic (just the value string)
  let text ← liftSpider <| Dynamic.mapM (·.value) textState

  -- Render function
  let render : ComponentRender := do
    let state ← textState.sample
    let focused ← focusedInput.sample
    let isFoc := focused == some name
    pure (textAreaVisual name theme { state with focused := isFoc } placeholder width height)

  pure { onChange, onFocus, onBlur, text, isFocused, render }

end Demos.ReactiveShowcase.Components
