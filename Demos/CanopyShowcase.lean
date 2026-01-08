/-
  Canopy Widget Showcase Demo
  Demonstrates the Canopy widget library components with interactivity.
-/
import Afferent
import Afferent.Canopy
import Demos.Demo
import Trellis

open Afferent CanvasM
open Afferent.Arbor
open Afferent.Canopy
open Trellis

namespace Demos

/-! ## State and Messages -/

/-- State for the Canopy showcase demo. -/
structure CanopyShowcaseState where
  /-- Widget interaction states (hover, focus, press). -/
  widgetStates : WidgetStates := {}
  /-- First checkbox state. -/
  checkbox1 : Bool := false
  /-- Second checkbox state. -/
  checkbox2 : Bool := true
  /-- First text input state. -/
  textInput1State : TextInputState := { value := "" }
  /-- Second text input state. -/
  textInput2State : TextInputState := { value := "Hello, World!", cursor := 13 }
  /-- Currently focused input name. -/
  focusedInput : Option String := none
  /-- Button click counter (for demo). -/
  buttonClickCount : Nat := 0
  /-- Selected radio button value. -/
  radioSelection : String := "option1"
  /-- First switch state. -/
  switch1 : Bool := false
  /-- Second switch state. -/
  switch2 : Bool := true
  /-- First switch animation progress (0.0 = off, 1.0 = on). -/
  switch1Anim : Float := 0.0
  /-- Second switch animation progress (0.0 = off, 1.0 = on). -/
  switch2Anim : Float := 1.0
  /-- First slider value (0.0-1.0). -/
  slider1 : Float := 0.3
  /-- Second slider value (0.0-1.0). -/
  slider2 : Float := 0.7
  /-- Currently dragging slider (for drag-to-change). -/
  draggingSlider : Option String := none

namespace CanopyShowcaseState

def initial : CanopyShowcaseState := {}

end CanopyShowcaseState

/-! ## Widget Names -/

def btnPrimaryName := "canopy-btn-primary"
def btnSecondaryName := "canopy-btn-secondary"
def btnOutlineName := "canopy-btn-outline"
def btnGhostName := "canopy-btn-ghost"
def checkbox1Name := "canopy-cb1"
def checkbox2Name := "canopy-cb2"
def textInput1Name := "canopy-input1"
def textInput2Name := "canopy-input2"
def radio1Name := "canopy-radio1"
def radio2Name := "canopy-radio2"
def radio3Name := "canopy-radio3"
def switch1Name := "canopy-switch1"
def switch2Name := "canopy-switch2"
def slider1Name := "canopy-slider1"
def slider2Name := "canopy-slider2"

/-! ## Visual Widget Builders -/

/-- Build a visual button (renders state from CanopyShowcaseState). -/
def demoButton (name : String) (labelText : String) (theme : Theme)
    (variant : ButtonVariant) (state : WidgetState := {}) : WidgetBuilder := do
  let colors := Button.variantColors theme variant
  let bgColor := Button.backgroundColor colors state
  let fgColor := Button.foregroundColor colors state
  let bw := Button.borderWidth variant

  let style : BoxStyle := {
    backgroundColor := some bgColor
    borderColor := if bw > 0 then some colors.border else none
    borderWidth := bw
    cornerRadius := theme.cornerRadius
    padding := EdgeInsets.symmetric theme.padding (theme.padding * 0.6)
  }

  namedCenter name (style := style) do
    text' labelText theme.font fgColor .center

/-- Build a visual checkbox (renders state from CanopyShowcaseState). -/
def demoCheckbox (name : String) (labelText : String) (theme : Theme)
    (checked : Bool) (state : WidgetState := {}) : WidgetBuilder := do
  let colors := theme.input
  let boxSize : Float := 20.0
  let boxBg := if checked then theme.primary.background else colors.background
  let borderColor := if state.focused then colors.borderFocused else colors.border

  let checkboxBox : WidgetBuilder := do
    if checked then
      custom (Checkbox.boxSpec checked state.hovered theme boxSize) {
        minWidth := some boxSize
        minHeight := some boxSize
        cornerRadius := 4
        borderColor := some borderColor
        borderWidth := if state.focused then 2 else 1
        backgroundColor := some boxBg
      }
    else
      box {
        minWidth := some boxSize
        minHeight := some boxSize
        cornerRadius := 4
        borderColor := some borderColor
        borderWidth := if state.focused then 2 else 1
        backgroundColor := some boxBg
      }

  -- Use custom flex container with alignItems := .center to prevent stretching
  let wid ← freshId
  let props : Trellis.FlexContainer := { Trellis.FlexContainer.row 8 with alignItems := .center }
  let checkBox ← checkboxBox
  let label ← text' labelText theme.font theme.text .left
  pure (.flex wid (some name) props {} #[checkBox, label])


/-! ## Main Widget -/

/-- Build the Canopy showcase widget with interactive elements. -/
def canopyShowcaseWidget (fontId : FontId) (smallFontId : FontId)
    (screenScale : Float := 1.0) (state : CanopyShowcaseState) : WidgetBuilder := do
  let s := fun (v : Float) => v * screenScale

  -- Create theme with the provided fonts
  let theme : Theme := { Theme.dark with font := fontId, smallFont := smallFontId }

  -- Get widget states
  let primaryState := state.widgetStates.get btnPrimaryName
  let secondaryState := state.widgetStates.get btnSecondaryName
  let outlineState := state.widgetStates.get btnOutlineName
  let ghostState := state.widgetStates.get btnGhostName
  let cb1State := state.widgetStates.get checkbox1Name
  let cb2State := state.widgetStates.get checkbox2Name
  let radio1State := state.widgetStates.get radio1Name
  let radio2State := state.widgetStates.get radio2Name
  let radio3State := state.widgetStates.get radio3Name
  let switch1State := state.widgetStates.get switch1Name
  let switch2State := state.widgetStates.get switch2Name
  let slider1State := state.widgetStates.get slider1Name
  let slider2State := state.widgetStates.get slider2Name

  column (gap := s 20) (style := {
    backgroundColor := some (Color.gray 0.1)
    padding := EdgeInsets.uniform (s 24)
    width := .percent 1.0
    height := .percent 1.0
    flexItem := some (FlexItem.growing 1)
  }) #[
    -- Title
    heading1 "Canopy Widgets" theme,
    row (gap := s 16) (style := {}) #[
      caption "A foundational widget library for Afferent" theme,
      if state.buttonClickCount > 0 then
        caption s!"(Clicks: {state.buttonClickCount})" theme
      else
        spacer 0 0
    ],

    -- Labels section (non-interactive)
    titledPanel "Labels" .outlined theme do
      column (gap := s 8) (style := {}) #[
        heading1 "Heading 1" theme,
        heading2 "Heading 2" theme,
        heading3 "Heading 3" theme,
        bodyText "Body text - normal paragraph content" theme,
        caption "Caption - small muted text" theme
      ],

    -- Buttons section (interactive)
    titledPanel "Buttons" .outlined theme do
      column (gap := s 12) (style := {}) #[
        caption "Click a button to increment the counter:" theme,
        row (gap := s 8) (style := {}) #[
          demoButton btnPrimaryName "Primary" theme .primary primaryState,
          demoButton btnSecondaryName "Secondary" theme .secondary secondaryState,
          demoButton btnOutlineName "Outline" theme .outline outlineState,
          demoButton btnGhostName "Ghost" theme .ghost ghostState
        ]
      ],

    -- Checkboxes section (interactive)
    titledPanel "Checkboxes" .outlined theme do
      column (gap := s 8) (style := {}) #[
        caption "Click to toggle:" theme,
        row (gap := s 24) (style := {}) #[
          demoCheckbox checkbox1Name "Option 1" theme state.checkbox1 cb1State,
          demoCheckbox checkbox2Name "Option 2" theme state.checkbox2 cb2State
        ]
      ],

    -- Radio Buttons section (interactive)
    titledPanel "Radio Buttons" .outlined theme do
      column (gap := s 8) (style := {}) #[
        caption "Click to select one option:" theme,
        column (gap := s 8) (style := {}) #[
          radioButtonVisual radio1Name "Option 1" theme (state.radioSelection == "option1") radio1State,
          radioButtonVisual radio2Name "Option 2" theme (state.radioSelection == "option2") radio2State,
          radioButtonVisual radio3Name "Option 3" theme (state.radioSelection == "option3") radio3State
        ]
      ],

    -- Switches section (interactive, animated)
    titledPanel "Switches" .outlined theme do
      column (gap := s 8) (style := {}) #[
        caption "Click to toggle:" theme,
        row (gap := s 24) (style := {}) #[
          animatedSwitchVisual switch1Name (some "Notifications") theme state.switch1Anim switch1State,
          animatedSwitchVisual switch2Name (some "Dark Mode") theme state.switch2Anim switch2State
        ]
      ],

    -- Sliders section (interactive)
    titledPanel "Sliders" .outlined theme do
      column (gap := s 8) (style := {}) #[
        caption "Click to adjust value:" theme,
        row (gap := s 24) (style := {}) #[
          sliderVisual slider1Name (some "Volume") theme state.slider1 slider1State,
          sliderVisual slider2Name (some "Brightness") theme state.slider2 slider2State
        ]
      ],

    -- Text Input section (interactive - click to focus, type to edit)
    titledPanel "Text Inputs" .outlined theme do
      column (gap := s 12) (style := {}) #[
        caption "Click to focus, then type:" theme,
        textInputVisual textInput1Name theme
          { state.textInput1State with focused := state.focusedInput == some textInput1Name }
          "Enter text here...",
        textInputVisual textInput2Name theme
          { state.textInput2State with focused := state.focusedInput == some textInput2Name }
          "Type something..."
      ],

    -- Panels section (non-interactive)
    titledPanel "Panels" .outlined theme do
      row (gap := s 12) (style := {}) #[
        elevatedPanel theme (s 12) do
          column (gap := s 4) (style := { minWidth := some (s 100) }) #[
            heading3 "Elevated" theme,
            caption "Card-like" theme
          ],
        outlinedPanel theme (s 12) do
          column (gap := s 4) (style := { minWidth := some (s 100) }) #[
            heading3 "Outlined" theme,
            caption "Border only" theme
          ],
        filledPanel theme (s 12) do
          column (gap := s 4) (style := { minWidth := some (s 100) }) #[
            heading3 "Filled" theme,
            caption "Solid bg" theme
          ]
      ]
  ]

end Demos
