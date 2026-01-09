/-
  ReactiveShowcase - State Structures
  Reactive state using Dynamics and snapshot for rendering.
-/
import Reactive
import Afferent.Canopy

open Reactive Reactive.Host

namespace Demos.ReactiveShowcase

/-- Full reactive state - all fields are Dynamics that update automatically. -/
structure ReactiveShowcaseState where
  /-- Button click counter. -/
  buttonClickCount : Dynamic Spider Nat
  /-- First checkbox state. -/
  checkbox1 : Dynamic Spider Bool
  /-- Second checkbox state. -/
  checkbox2 : Dynamic Spider Bool
  /-- Selected radio button value. -/
  radioSelection : Dynamic Spider String
  /-- First switch state. -/
  switch1 : Dynamic Spider Bool
  /-- Second switch state. -/
  switch2 : Dynamic Spider Bool
  /-- First switch animation progress (0.0 = off, 1.0 = on). -/
  switch1Anim : Dynamic Spider Float
  /-- Second switch animation progress. -/
  switch2Anim : Dynamic Spider Float
  /-- First slider value (0.0-1.0). -/
  slider1 : Dynamic Spider Float
  /-- Second slider value. -/
  slider2 : Dynamic Spider Float
  /-- Currently dragging slider name. -/
  draggingSlider : Dynamic Spider (Option String)
  /-- Dropdown selected index. -/
  dropdown1Selection : Dynamic Spider Nat
  /-- Whether dropdown is open. -/
  dropdown1Open : Dynamic Spider Bool
  /-- Currently hovered dropdown option. -/
  dropdown1HoveredOption : Dynamic Spider (Option Nat)
  /-- Active tab index. -/
  activeTab : Dynamic Spider Nat
  /-- Hovered tab index. -/
  hoveredTab : Dynamic Spider (Option Nat)
  /-- Whether modal is open. -/
  modalOpen : Dynamic Spider Bool
  /-- Currently focused input name. -/
  focusedInput : Dynamic Spider (Option String)
  /-- Widget interaction states (hover, focus, press). -/
  widgetStates : Dynamic Spider Afferent.Canopy.WidgetStates
  /-- First text input state. -/
  textInput1 : Dynamic Spider Afferent.Canopy.TextInputState
  /-- Second text input state. -/
  textInput2 : Dynamic Spider Afferent.Canopy.TextInputState
  /-- Text area state. -/
  textArea : Dynamic Spider Afferent.Canopy.TextAreaState

/-- Snapshot of state for rendering (all values sampled at a point in time). -/
structure ReactiveShowcaseSnapshot where
  buttonClickCount : Nat
  checkbox1 : Bool
  checkbox2 : Bool
  radioSelection : String
  switch1 : Bool
  switch2 : Bool
  switch1Anim : Float
  switch2Anim : Float
  slider1 : Float
  slider2 : Float
  draggingSlider : Option String
  dropdown1Selection : Nat
  dropdown1Open : Bool
  dropdown1HoveredOption : Option Nat
  activeTab : Nat
  hoveredTab : Option Nat
  modalOpen : Bool
  focusedInput : Option String
  widgetStates : Afferent.Canopy.WidgetStates
  textInput1 : Afferent.Canopy.TextInputState
  textInput2 : Afferent.Canopy.TextInputState
  textArea : Afferent.Canopy.TextAreaState

/-- Sample all dynamics to create a snapshot for rendering. -/
def ReactiveShowcaseState.snapshot (s : ReactiveShowcaseState) : IO ReactiveShowcaseSnapshot := do
  pure {
    buttonClickCount := ← s.buttonClickCount.sample
    checkbox1 := ← s.checkbox1.sample
    checkbox2 := ← s.checkbox2.sample
    radioSelection := ← s.radioSelection.sample
    switch1 := ← s.switch1.sample
    switch2 := ← s.switch2.sample
    switch1Anim := ← s.switch1Anim.sample
    switch2Anim := ← s.switch2Anim.sample
    slider1 := ← s.slider1.sample
    slider2 := ← s.slider2.sample
    draggingSlider := ← s.draggingSlider.sample
    dropdown1Selection := ← s.dropdown1Selection.sample
    dropdown1Open := ← s.dropdown1Open.sample
    dropdown1HoveredOption := ← s.dropdown1HoveredOption.sample
    activeTab := ← s.activeTab.sample
    hoveredTab := ← s.hoveredTab.sample
    modalOpen := ← s.modalOpen.sample
    focusedInput := ← s.focusedInput.sample
    widgetStates := ← s.widgetStates.sample
    textInput1 := ← s.textInput1.sample
    textInput2 := ← s.textInput2.sample
    textArea := ← s.textArea.sample
  }

end Demos.ReactiveShowcase
