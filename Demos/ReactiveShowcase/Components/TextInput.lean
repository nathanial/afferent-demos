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
    Pass a shared focusedInput Dynamic to coordinate focus across multiple inputs. -/
def textInput (name : String) (theme : Theme) (placeholder : String)
    (initialValue : String)
    (focusedInput : Dynamic Spider (Option String))
    (setFocusedInput : Option String → IO Unit)
    : ReactiveM TextInputComponent := do
  let ctx ← liftSpider SpiderM.getTimelineCtx

  -- Create internal text/cursor state
  let (textState, setTextState) ← liftSpider <| SpiderM.liftIO <| Dynamic.new ctx ({
    value := initialValue
    cursor := initialValue.length
    cursorPixelX := 0.0  -- Will be updated on first render
  } : TextInputState)

  -- Create events
  let (onChange, fireChange) ← liftSpider <| newTriggerEvent (t := Spider) (a := String)
  let (onFocus, fireFocus) ← liftSpider <| newTriggerEvent (t := Spider) (a := Unit)
  let (onBlur, fireBlur) ← liftSpider <| newTriggerEvent (t := Spider) (a := Unit)

  -- Track previous focus state for blur detection
  let (wasFocused, setWasFocused) ← liftSpider <| SpiderM.liftIO <| Dynamic.new ctx false

  -- Wire click to focus
  let clicks ← useClick name
  let _ ← liftSpider <| SpiderM.liftIO <| clicks.subscribe fun _ => do
    let prev ← focusedInput.sample
    if prev != some name then
      setFocusedInput (some name)
      fireFocus ()

  -- Wire keyboard events (when focused)
  let keyEvents ← useKeyboard
  let _ ← liftSpider <| SpiderM.liftIO <| keyEvents.subscribe fun keyData => do
    let focused ← focusedInput.sample
    if focused == some name then
      let current ← textState.sample
      let updated := TextInput.handleKeyPress keyData.event current none
      setTextState updated
      if updated.value != current.value then
        fireChange updated.value

  -- Wire blur detection
  let allClicks ← useAllClicks
  let _ ← liftSpider <| SpiderM.liftIO <| allClicks.subscribe fun _ => do
    let wasFoc ← wasFocused.sample
    let currentFoc ← focusedInput.sample
    if wasFoc && currentFoc != some name then
      setWasFocused false
      fireBlur ()
    else if currentFoc == some name then
      setWasFocused true

  -- Create derived isFocused Dynamic
  let isFocused ← liftSpider <| Dynamic.mapM (· == some name) focusedInput

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
    Similar to textInput but with multi-line support. -/
def textArea (name : String) (theme : Theme) (placeholder : String)
    (initialState : TextAreaState)
    (focusedInput : Dynamic Spider (Option String))
    (setFocusedInput : Option String → IO Unit)
    (width : Float := 280) (height : Float := 120)
    : ReactiveM TextAreaComponent := do
  let ctx ← liftSpider SpiderM.getTimelineCtx

  -- Create internal text state
  let (textState, setTextState) ← liftSpider <| SpiderM.liftIO <| Dynamic.new ctx initialState

  -- Create events
  let (onChange, fireChange) ← liftSpider <| newTriggerEvent (t := Spider) (a := String)
  let (onFocus, fireFocus) ← liftSpider <| newTriggerEvent (t := Spider) (a := Unit)
  let (onBlur, fireBlur) ← liftSpider <| newTriggerEvent (t := Spider) (a := Unit)

  -- Track previous focus state for blur detection
  let (wasFocused, setWasFocused) ← liftSpider <| SpiderM.liftIO <| Dynamic.new ctx false

  -- Wire click to focus
  let clicks ← useClick name
  let _ ← liftSpider <| SpiderM.liftIO <| clicks.subscribe fun _ => do
    let prev ← focusedInput.sample
    if prev != some name then
      setFocusedInput (some name)
      fireFocus ()

  -- Wire keyboard events (when focused)
  let keyEvents ← useKeyboard
  let _ ← liftSpider <| SpiderM.liftIO <| keyEvents.subscribe fun keyData => do
    let focused ← focusedInput.sample
    if focused == some name then
      let current ← textState.sample
      let updated := TextArea.handleKeyPress keyData.event current none
      setTextState updated
      if updated.value != current.value then
        fireChange updated.value

  -- Wire blur detection
  let allClicks ← useAllClicks
  let _ ← liftSpider <| SpiderM.liftIO <| allClicks.subscribe fun _ => do
    let wasFoc ← wasFocused.sample
    let currentFoc ← focusedInput.sample
    if wasFoc && currentFoc != some name then
      setWasFocused false
      fireBlur ()
    else if currentFoc == some name then
      setWasFocused true

  -- Create derived isFocused Dynamic
  let isFocused ← liftSpider <| Dynamic.mapM (· == some name) focusedInput

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
