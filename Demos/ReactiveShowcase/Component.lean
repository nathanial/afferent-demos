/-
  ReactiveShowcase - Component Infrastructure
  React-like component helpers for self-contained widget definitions.
-/
import Reactive
import Afferent
import Afferent.Canopy
import Demos.Demo
import Demos.ReactiveShowcase.Types
import Demos.ReactiveShowcase.Inputs
import Demos.CanopyShowcase

open Reactive Reactive.Host
open Afferent.Canopy

namespace Demos.ReactiveShowcase

/-! ## The ReactiveM Monad

Like React's context, ReactiveM carries the event streams implicitly.
Components use hooks that access this context without explicit parameters.
-/

/-- ReactiveM is SpiderM with implicit access to ReactiveEvents.
    This is analogous to how React components access context through hooks. -/
abbrev ReactiveM := ReaderT ReactiveEvents SpiderM

/-- Explicit ForIn instance for ReactiveM to avoid issues with derived instances.
    This properly threads through both the ReactiveEvents context and SpiderEnv. -/
instance [ForIn SpiderM ρ α] : ForIn ReactiveM ρ α where
  forIn x init f := fun ctx => ForIn.forIn x init fun a b => f a b ctx

/-- Run a ReactiveM computation with the given events context. -/
def ReactiveM.run (events : ReactiveEvents) (m : ReactiveM α) : SpiderM α :=
  ReaderT.run m events

/-- Get the events from the implicit context. -/
def getEvents : ReactiveM ReactiveEvents := read

/-- Lift SpiderM into ReactiveM. -/
def liftSpider (m : SpiderM α) : ReactiveM α := fun _ => m

/-! ## Type Aliases -/

/-- A component's render function - samples its internal dynamics and produces a widget. -/
abbrev ComponentRender := IO Afferent.Arbor.WidgetBuilder

/-! ## Hit Testing Helpers -/

/-- Find a widget ID by name in the widget tree.
    Uses a depth limit to ensure termination. -/
def findWidgetIdByName (widget : Afferent.Arbor.Widget)
    (target : String) (maxDepth : Nat := 100) : Option Afferent.Arbor.WidgetId :=
  go widget target maxDepth
where
  go (widget : Afferent.Arbor.Widget) (target : String) (fuel : Nat)
      : Option Afferent.Arbor.WidgetId :=
    match fuel with
    | 0 => none  -- Depth limit reached
    | fuel' + 1 =>
      let widgetName := Afferent.Arbor.Widget.name? widget
      match widgetName with
      | some name =>
          if name == target then
            some (Afferent.Arbor.Widget.id widget)
          else
            findInChildren widget target fuel'
      | none =>
          findInChildren widget target fuel'
  findInChildren (widget : Afferent.Arbor.Widget) (target : String) (fuel : Nat)
      : Option Afferent.Arbor.WidgetId :=
    let children := Afferent.Arbor.Widget.children widget
    let rec loop (idx : Nat) : Option Afferent.Arbor.WidgetId :=
      if idx >= children.size then
        none
      else
        match children[idx]? with
        | some child =>
            match go child target fuel with
            | some result => some result
            | none => loop (idx + 1)
        | none => loop (idx + 1)
    loop 0

/-- Check if a named widget is in the hit path. -/
def hitPathHasNamedWidget (widget : Afferent.Arbor.Widget)
    (hitPath : Array Afferent.Arbor.WidgetId) (name : String) : Bool :=
  match findWidgetIdByName widget name with
  | some wid => hitPath.any (· == wid)
  | none => false

/-- Check if a widget name is in the hit path (for ClickData). -/
def hitWidget (data : ClickData) (name : String) : Bool :=
  hitPathHasNamedWidget data.widget data.hitPath name

/-- Check if a widget name is in the hit path (for HoverData). -/
def hitWidgetHover (data : HoverData) (name : String) : Bool :=
  hitPathHasNamedWidget data.widget data.hitPath name

/-- Calculate slider value from click position given the slider's layout. -/
def calculateSliderValue (clickX : Float) (layouts : Trellis.LayoutResult)
    (widget : Afferent.Arbor.Widget) (sliderName : String) : Option Float :=
  match findWidgetIdByName widget sliderName with
  | some wid =>
      match layouts.get wid with
      | some layout =>
          let rect := layout.contentRect
          let dims := Afferent.Canopy.Slider.defaultDimensions
          let relativeX := clickX - rect.x
          let trackWidth := dims.trackWidth
          let value := relativeX / trackWidth
          let clampedValue := if value < 0.0 then 0.0 else if value > 1.0 then 1.0 else value
          some clampedValue
      | none => none
  | none => none

/-! ## Component Hooks

These are like React hooks - they access the event context implicitly
and set up subscriptions automatically.
-/

/-- Create a hover state Dynamic for a widget (like React's useState + useEffect for hover).
    Returns a Dynamic that is true when the mouse is over the widget. -/
def useHover (name : String) : ReactiveM (Dynamic Spider Bool) := do
  let events ← getEvents
  liftSpider do
    -- Pure FRP: map hover events to bool, hold latest value
    let hoverChanges ← Event.mapM (fun data => hitWidgetHover data name) events.hoverEvent
    holdDyn false hoverChanges

/-- Create a click event for a widget that fires Unit (like React's onClick handler).
    Returns an Event that fires when the widget is clicked. -/
def useClick (name : String) : ReactiveM (Event Spider Unit) := do
  let events ← getEvents
  liftSpider do
    let clicks ← Event.filterM (fun data => hitWidget data name) events.clickEvent
    Event.mapM (fun _ => ()) clicks

/-- Subscribe to animation frames. Returns an Event that fires each frame with delta time. -/
def useAnimationFrame : ReactiveM (Event Spider Float) := do
  let events ← getEvents
  pure events.animationFrame

/-- Subscribe to key events. Returns the raw key event stream. -/
def useKeyboard : ReactiveM (Event Spider KeyData) := do
  let events ← getEvents
  pure events.keyEvent

/-- Create click event with full data (for sliders that need position). -/
def useClickData (name : String) : ReactiveM (Event Spider ClickData) := do
  let events ← getEvents
  liftSpider do
    Event.filterM (fun data => hitWidget data name) events.clickEvent

/-- Subscribe to all click events (for focus management). -/
def useAllClicks : ReactiveM (Event Spider ClickData) := do
  let events ← getEvents
  pure events.clickEvent

/-- Subscribe to all hover events (for position tracking). -/
def useAllHovers : ReactiveM (Event Spider HoverData) := do
  let events ← getEvents
  pure events.hoverEvent

/-- Set up automatic focus clearing when clicking non-input interactive widgets.
    Call this after all components have been created. -/
def ComponentRegistry.setupFocusClearing (reg : ComponentRegistry) : ReactiveM Unit := do
  let inputs ← liftSpider <| SpiderM.liftIO reg.inputNames.get
  let interactives ← liftSpider <| SpiderM.liftIO reg.interactiveNames.get

  let isInputClick (data : ClickData) : Bool :=
    inputs.any (fun name => hitWidget data name)
  let isNonInputInteractiveClick (data : ClickData) : Bool :=
    interactives.any (fun name => hitWidget data name)

  let allClicks ← useAllClicks
  let nonInputClicks ← liftSpider <| Event.filterM
    (fun data => !isInputClick data && isNonInputInteractiveClick data) allClicks
  let clearAction ← liftSpider <| Event.mapM (fun _ => reg.fireFocus none) nonInputClicks
  liftSpider <| performEvent_ clearAction

end Demos.ReactiveShowcase
