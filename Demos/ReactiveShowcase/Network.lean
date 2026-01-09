/-
  ReactiveShowcase - Reactive Network Setup
  Wires all widget subscriptions to global event streams.
-/
import Reactive
import Afferent
import Afferent.Canopy
import Demos.Demo
import Demos.ReactiveShowcase.Types
import Demos.ReactiveShowcase.Inputs
import Demos.ReactiveShowcase.State
import Demos.CanopyShowcase

open Reactive Reactive.Host
open Afferent.Canopy

namespace Demos.ReactiveShowcase

-- Re-export widget names from CanopyShowcase
open Demos in
def btnPrimaryName := Demos.btnPrimaryName
def btnSecondaryName := Demos.btnSecondaryName
def btnOutlineName := Demos.btnOutlineName
def btnGhostName := Demos.btnGhostName
def checkbox1Name := Demos.checkbox1Name
def checkbox2Name := Demos.checkbox2Name
def textInput1Name := Demos.textInput1Name
def textInput2Name := Demos.textInput2Name
def textAreaName := Demos.textAreaName
def radio1Name := Demos.radio1Name
def radio2Name := Demos.radio2Name
def radio3Name := Demos.radio3Name
def switch1Name := Demos.switch1Name
def switch2Name := Demos.switch2Name
def slider1Name := Demos.slider1Name
def slider2Name := Demos.slider2Name
def dropdown1Name := Demos.dropdown1Name
def dropdown1TriggerName := Demos.dropdown1TriggerName
def dropdown1OptionName := Demos.dropdown1OptionName
def dropdown1Options := Demos.dropdown1Options
def tabHeaderName := Demos.tabHeaderName
def tabLabels := Demos.tabLabels
def tabSettingsCheckboxName := Demos.tabSettingsCheckboxName
def modalTriggerName := Demos.modalTriggerName
def modalName := Demos.modalName
def modalBackdropName := Demos.modalBackdropName
def modalCloseName := Demos.modalCloseName
def modalConfirmName := Demos.modalConfirmName
def modalCancelName := Demos.modalCancelName
def tabViewName := Demos.tabViewName

-- Helper Functions

/-- Find a widget ID by name in the widget tree. -/
private partial def findWidgetIdByName (widget : Afferent.Arbor.Widget)
    (target : String) : Option Afferent.Arbor.WidgetId :=
  let widgetName := Afferent.Arbor.Widget.name? widget
  match widgetName with
  | some name =>
      if name == target then
        some (Afferent.Arbor.Widget.id widget)
      else
        findInChildren widget target
  | none =>
      findInChildren widget target
where
  findInChildren (widget : Afferent.Arbor.Widget) (target : String)
      : Option Afferent.Arbor.WidgetId :=
    let children := Afferent.Arbor.Widget.children widget
    let rec loop (idx : Nat) : Option Afferent.Arbor.WidgetId :=
      if idx >= children.size then
        none
      else
        match children[idx]? with
        | some child =>
            match findWidgetIdByName child target with
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

/-- Create a filtered click event for a specific widget. -/
def widgetClicks (events : ReactiveEvents) (name : String)
    : SpiderM (Event Spider ClickData) :=
  Event.filterM (fun data => hitWidget data name) events.clickEvent

-- Network Setup

/-- Setup the complete reactive network for the showcase.
    Creates all dynamics and wires up all widget subscriptions. -/
def setupNetwork (events : ReactiveEvents) (env : DemoEnv)
    : SpiderM ReactiveShowcaseState := do
  -- Get timeline context for creating dynamics
  let ctx ← SpiderM.getTimelineCtx

  -- Create state dynamics with initial values
  -- Each Dynamic.new returns (Dynamic, updateFn) tuple
  let (buttonClicksDyn, updateButtonClicks) ← SpiderM.liftIO <| Dynamic.new ctx (0 : Nat)
  let (checkbox1Dyn, updateCheckbox1) ← SpiderM.liftIO <| Dynamic.new ctx false
  let (checkbox2Dyn, updateCheckbox2) ← SpiderM.liftIO <| Dynamic.new ctx true
  let (radioSelectionDyn, updateRadioSelection) ← SpiderM.liftIO <| Dynamic.new ctx "option1"
  let (switch1Dyn, updateSwitch1) ← SpiderM.liftIO <| Dynamic.new ctx false
  let (switch2Dyn, updateSwitch2) ← SpiderM.liftIO <| Dynamic.new ctx true
  let (switch1AnimDyn, updateSwitch1Anim) ← SpiderM.liftIO <| Dynamic.new ctx (0.0 : Float)
  let (switch2AnimDyn, updateSwitch2Anim) ← SpiderM.liftIO <| Dynamic.new ctx (1.0 : Float)
  let (slider1Dyn, updateSlider1) ← SpiderM.liftIO <| Dynamic.new ctx (0.3 : Float)
  let (slider2Dyn, updateSlider2) ← SpiderM.liftIO <| Dynamic.new ctx (0.7 : Float)
  let (draggingSliderDyn, _updateDraggingSlider) ← SpiderM.liftIO <| Dynamic.new ctx (none : Option String)
  let (dropdown1SelectionDyn, updateDropdown1Selection) ← SpiderM.liftIO <| Dynamic.new ctx (0 : Nat)
  let (dropdown1OpenDyn, updateDropdown1Open) ← SpiderM.liftIO <| Dynamic.new ctx false
  let (dropdown1HoveredOptionDyn, updateDropdown1HoveredOption) ← SpiderM.liftIO <| Dynamic.new ctx (none : Option Nat)
  let (activeTabDyn, updateActiveTab) ← SpiderM.liftIO <| Dynamic.new ctx (0 : Nat)
  let (hoveredTabDyn, updateHoveredTab) ← SpiderM.liftIO <| Dynamic.new ctx (none : Option Nat)
  let (modalOpenDyn, updateModalOpen) ← SpiderM.liftIO <| Dynamic.new ctx false
  let (focusedInputDyn, updateFocusedInput) ← SpiderM.liftIO <| Dynamic.new ctx (none : Option String)
  let (widgetStatesDyn, updateWidgetStates) ← SpiderM.liftIO <| Dynamic.new ctx ({} : WidgetStates)

  -- Text inputs with initial values
  let (textInput1Dyn, updateTextInput1) ← SpiderM.liftIO <| Dynamic.new ctx ({ value := "" } : TextInputState)

  -- Compute initial cursor position for pre-filled text input
  let initialText := "Hello, World!"
  let (cursorX, _) ← SpiderM.liftIO <| env.fontCanopy.measureText initialText
  let textInput2Initial : TextInputState := {
    value := initialText
    cursor := initialText.length
    cursorPixelX := cursorX
  }
  let (textInput2Dyn, updateTextInput2) ← SpiderM.liftIO <| Dynamic.new ctx textInput2Initial

  -- Text area with initial value
  let textAreaInitial := CanopyShowcaseState.initial.textAreaState
  let (textAreaDyn, updateTextArea) ← SpiderM.liftIO <| Dynamic.new ctx textAreaInitial

  -- Wire Button Clicks
  -- All 4 buttons increment the click counter
  for name in [btnPrimaryName, btnSecondaryName, btnOutlineName, btnGhostName] do
    let clicks ← widgetClicks events name
    let _ ← SpiderM.liftIO <| clicks.subscribe fun _ => do
      let current ← buttonClicksDyn.sample
      updateButtonClicks (current + 1)

  -- Wire Checkboxes
  let cb1Clicks ← widgetClicks events checkbox1Name
  let _ ← SpiderM.liftIO <| cb1Clicks.subscribe fun _ => do
    let current ← checkbox1Dyn.sample
    updateCheckbox1 (!current)

  let cb2Clicks ← widgetClicks events checkbox2Name
  let _ ← SpiderM.liftIO <| cb2Clicks.subscribe fun _ => do
    let current ← checkbox2Dyn.sample
    updateCheckbox2 (!current)

  -- Settings tab checkbox also toggles checkbox1
  let tabCbClicks ← widgetClicks events tabSettingsCheckboxName
  let _ ← SpiderM.liftIO <| tabCbClicks.subscribe fun _ => do
    let current ← checkbox1Dyn.sample
    updateCheckbox1 (!current)

  -- Wire Radio Buttons
  for (name, value) in [(radio1Name, "option1"), (radio2Name, "option2"), (radio3Name, "option3")] do
    let clicks ← widgetClicks events name
    let _ ← SpiderM.liftIO <| clicks.subscribe fun _ => do
      updateRadioSelection value

  -- Wire Switches
  let sw1Clicks ← widgetClicks events switch1Name
  let _ ← SpiderM.liftIO <| sw1Clicks.subscribe fun _ => do
    let current ← switch1Dyn.sample
    updateSwitch1 (!current)

  let sw2Clicks ← widgetClicks events switch2Name
  let _ ← SpiderM.liftIO <| sw2Clicks.subscribe fun _ => do
    let current ← switch2Dyn.sample
    updateSwitch2 (!current)

  -- Wire Sliders
  let slider1Clicks ← widgetClicks events slider1Name
  let _ ← SpiderM.liftIO <| slider1Clicks.subscribe fun data => do
    match calculateSliderValue data.click.x data.layouts data.widget slider1Name with
    | some v => updateSlider1 v
    | none => pure ()

  let slider2Clicks ← widgetClicks events slider2Name
  let _ ← SpiderM.liftIO <| slider2Clicks.subscribe fun data => do
    match calculateSliderValue data.click.x data.layouts data.widget slider2Name with
    | some v => updateSlider2 v
    | none => pure ()

  -- Wire Dropdown
  let dropdownTriggerClicks ← widgetClicks events dropdown1TriggerName
  let _ ← SpiderM.liftIO <| dropdownTriggerClicks.subscribe fun _ => do
    let current ← dropdown1OpenDyn.sample
    updateDropdown1Open (!current)

  -- Wire dropdown options
  for i in [:dropdown1Options.size] do
    let optClicks ← widgetClicks events (dropdown1OptionName i)
    let _ ← SpiderM.liftIO <| optClicks.subscribe fun _ => do
      updateDropdown1Selection i
      updateDropdown1Open false

  -- Click outside dropdown closes it
  let _ ← SpiderM.liftIO <| events.clickEvent.subscribe fun data => do
    let isOpen ← dropdown1OpenDyn.sample
    if isOpen then
      let clickedDropdown := hitWidget data dropdown1Name
      let clickedTrigger := hitWidget data dropdown1TriggerName
      if !clickedDropdown && !clickedTrigger then
        updateDropdown1Open false

  -- Wire Tabs
  for i in [:tabLabels.size] do
    let tabClicks ← widgetClicks events (tabHeaderName i)
    let _ ← SpiderM.liftIO <| tabClicks.subscribe fun _ => do
      updateActiveTab i

  -- Wire Modal
  let modalTriggerClicks ← widgetClicks events modalTriggerName
  let _ ← SpiderM.liftIO <| modalTriggerClicks.subscribe fun _ => do
    updateModalOpen true

  for name in [modalCloseName, modalConfirmName, modalCancelName] do
    let clicks ← widgetClicks events name
    let _ ← SpiderM.liftIO <| clicks.subscribe fun _ => do
      updateModalOpen false

  -- Backdrop click closes modal (if not clicking the modal dialog itself)
  let _ ← SpiderM.liftIO <| events.clickEvent.subscribe fun data => do
    let isOpen ← modalOpenDyn.sample
    if isOpen then
      let clickedBackdrop := hitWidget data modalBackdropName
      let clickedModal := hitWidget data modalName
      if clickedBackdrop && !clickedModal then
        updateModalOpen false

  -- Wire Focus Management
  let input1Clicks ← widgetClicks events textInput1Name
  let _ ← SpiderM.liftIO <| input1Clicks.subscribe fun _ => do
    updateFocusedInput (some textInput1Name)

  let input2Clicks ← widgetClicks events textInput2Name
  let _ ← SpiderM.liftIO <| input2Clicks.subscribe fun _ => do
    updateFocusedInput (some textInput2Name)

  let textAreaClicks ← widgetClicks events textAreaName
  let _ ← SpiderM.liftIO <| textAreaClicks.subscribe fun _ => do
    updateFocusedInput (some textAreaName)

  -- Clicking non-input widgets clears focus
  let _ ← SpiderM.liftIO <| events.clickEvent.subscribe fun data => do
    let clickedInput1 := hitWidget data textInput1Name
    let clickedInput2 := hitWidget data textInput2Name
    let clickedTextArea := hitWidget data textAreaName
    if !clickedInput1 && !clickedInput2 && !clickedTextArea then
      -- Only clear if clicking on an interactive widget (not empty space)
      let clickedButton := hitWidget data btnPrimaryName || hitWidget data btnSecondaryName ||
                          hitWidget data btnOutlineName || hitWidget data btnGhostName
      let clickedCheckbox := hitWidget data checkbox1Name || hitWidget data checkbox2Name
      let clickedRadio := hitWidget data radio1Name || hitWidget data radio2Name || hitWidget data radio3Name
      let clickedSwitch := hitWidget data switch1Name || hitWidget data switch2Name
      let clickedSlider := hitWidget data slider1Name || hitWidget data slider2Name
      if clickedButton || clickedCheckbox || clickedRadio || clickedSwitch || clickedSlider then
        updateFocusedInput none

  -- Wire Keyboard Events
  let _ ← SpiderM.liftIO <| events.keyEvent.subscribe fun keyData => do
    -- Escape closes modal
    let isModalOpen ← modalOpenDyn.sample
    if isModalOpen && keyData.event.key == .escape && keyData.event.isPress then
      updateModalOpen false
    else
      -- Route to focused input
      let focused ← focusedInputDyn.sample
      match focused with
      | some name =>
          if name == textInput1Name then
            let current ← textInput1Dyn.sample
            let updated := TextInput.handleKeyPress keyData.event current none
            updateTextInput1 updated
          else if name == textInput2Name then
            let current ← textInput2Dyn.sample
            let updated := TextInput.handleKeyPress keyData.event current none
            updateTextInput2 updated
          else if name == textAreaName then
            let current ← textAreaDyn.sample
            let updated := TextArea.handleKeyPress keyData.event current none
            updateTextArea updated
      | none => pure ()

  -- Wire Hover Events
  let _ ← SpiderM.liftIO <| events.hoverEvent.subscribe fun data => do
    let mut ws ← widgetStatesDyn.sample

    -- Update hover state for all interactive widgets
    ws := ws.setHovered btnPrimaryName (hitWidgetHover data btnPrimaryName)
    ws := ws.setHovered btnSecondaryName (hitWidgetHover data btnSecondaryName)
    ws := ws.setHovered btnOutlineName (hitWidgetHover data btnOutlineName)
    ws := ws.setHovered btnGhostName (hitWidgetHover data btnGhostName)
    ws := ws.setHovered checkbox1Name (hitWidgetHover data checkbox1Name)
    ws := ws.setHovered checkbox2Name (hitWidgetHover data checkbox2Name)
    ws := ws.setHovered tabSettingsCheckboxName (hitWidgetHover data tabSettingsCheckboxName)
    ws := ws.setHovered radio1Name (hitWidgetHover data radio1Name)
    ws := ws.setHovered radio2Name (hitWidgetHover data radio2Name)
    ws := ws.setHovered radio3Name (hitWidgetHover data radio3Name)
    ws := ws.setHovered switch1Name (hitWidgetHover data switch1Name)
    ws := ws.setHovered switch2Name (hitWidgetHover data switch2Name)
    ws := ws.setHovered slider1Name (hitWidgetHover data slider1Name)
    ws := ws.setHovered slider2Name (hitWidgetHover data slider2Name)
    ws := ws.setHovered dropdown1TriggerName (hitWidgetHover data dropdown1TriggerName)
    ws := ws.setHovered modalTriggerName (hitWidgetHover data modalTriggerName)
    ws := ws.setHovered modalCloseName (hitWidgetHover data modalCloseName)
    ws := ws.setHovered modalConfirmName (hitWidgetHover data modalConfirmName)
    ws := ws.setHovered modalCancelName (hitWidgetHover data modalCancelName)

    updateWidgetStates ws

    -- Update dropdown hovered option
    let isDropdownOpen ← dropdown1OpenDyn.sample
    if isDropdownOpen then
      let mut hoveredOpt : Option Nat := none
      for i in [:dropdown1Options.size] do
        if hitWidgetHover data (dropdown1OptionName i) then
          hoveredOpt := some i
          break
      updateDropdown1HoveredOption hoveredOpt
    else
      updateDropdown1HoveredOption none

    -- Update hovered tab
    let mut hoveredTab : Option Nat := none
    for i in [:tabLabels.size] do
      if hitWidgetHover data (tabHeaderName i) then
        hoveredTab := some i
        break
    updateHoveredTab hoveredTab

  -- Wire Animation Frame
  let _ ← SpiderM.liftIO <| events.animationFrame.subscribe fun dt => do
    let animSpeed := 8.0
    let rawFactor := animSpeed * dt
    let lerpFactor := if rawFactor > 1.0 then 1.0 else rawFactor

    -- Animate switch 1
    let s1 ← switch1Dyn.sample
    let anim1 ← switch1AnimDyn.sample
    let target1 := if s1 then 1.0 else 0.0
    let diff1 := target1 - anim1
    let newAnim1 := if diff1.abs < 0.01 then target1 else anim1 + diff1 * lerpFactor
    updateSwitch1Anim newAnim1

    -- Animate switch 2
    let s2 ← switch2Dyn.sample
    let anim2 ← switch2AnimDyn.sample
    let target2 := if s2 then 1.0 else 0.0
    let diff2 := target2 - anim2
    let newAnim2 := if diff2.abs < 0.01 then target2 else anim2 + diff2 * lerpFactor
    updateSwitch2Anim newAnim2

  -- Return the complete state
  pure {
    buttonClickCount := buttonClicksDyn
    checkbox1 := checkbox1Dyn
    checkbox2 := checkbox2Dyn
    radioSelection := radioSelectionDyn
    switch1 := switch1Dyn
    switch2 := switch2Dyn
    switch1Anim := switch1AnimDyn
    switch2Anim := switch2AnimDyn
    slider1 := slider1Dyn
    slider2 := slider2Dyn
    draggingSlider := draggingSliderDyn
    dropdown1Selection := dropdown1SelectionDyn
    dropdown1Open := dropdown1OpenDyn
    dropdown1HoveredOption := dropdown1HoveredOptionDyn
    activeTab := activeTabDyn
    hoveredTab := hoveredTabDyn
    modalOpen := modalOpenDyn
    focusedInput := focusedInputDyn
    widgetStates := widgetStatesDyn
    textInput1 := textInput1Dyn
    textInput2 := textInput2Dyn
    textArea := textAreaDyn
  }

end Demos.ReactiveShowcase
