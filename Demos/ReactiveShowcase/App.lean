/-
  ReactiveShowcase App - Composes all components into the demo UI.
-/
import Reactive
import Afferent
import Afferent.Canopy
import Demos.Demo
import Demos.ReactiveShowcase.Types
import Demos.ReactiveShowcase.Inputs
import Demos.ReactiveShowcase.Component
import Demos.ReactiveShowcase.Components.Button
import Demos.ReactiveShowcase.Components.Checkbox
import Demos.ReactiveShowcase.Components.Switch
import Demos.ReactiveShowcase.Components.Slider
import Demos.ReactiveShowcase.Components.RadioGroup
import Demos.ReactiveShowcase.Components.Dropdown
import Demos.ReactiveShowcase.Components.TextInput
import Demos.ReactiveShowcase.Components.TabView
import Demos.ReactiveShowcase.Components.Modal
import Demos.CanopyShowcase  -- For visual helpers

open Reactive Reactive.Host
open Afferent CanvasM
open Afferent.Arbor
open Afferent.Canopy
open Trellis

namespace Demos.ReactiveShowcase

/-- Application state returned from createApp. -/
structure AppState where
  /-- Render function that samples all component state and returns the complete UI. -/
  render : ComponentRender

/-- Create the complete reactive showcase application.
    Sets up all components and their interactions.
    Uses ReactiveM - the events context is implicit, not passed explicitly. -/
def createApp (env : DemoEnv) : ReactiveM AppState := do
  -- DEBUG: Adding components incrementally
  let theme : Theme := { Theme.dark with font := env.fontCanopyId, smallFont := env.fontCanopySmallId }

  -- Test: buttons + switch + dropdown (known working)
  let primaryBtn ← Components.button btnPrimaryName "Primary" theme .primary
  let switch1 ← Components.switch switch1Name (some "Test Switch") theme false
  let dropdown1 ← Components.dropdown dropdown1Name dropdown1TriggerName
      dropdown1OptionName dropdown1Options theme 0

  -- Add radioGroup to test
  let radioOptions : Array Components.RadioOption := #[
    { name := radio1Name, label := "Option 1", value := "option1" },
    { name := radio2Name, label := "Option 2", value := "option2" },
    { name := radio3Name, label := "Option 3", value := "option3" }
  ]
  let radioGroup ← Components.radioGroup radioOptions theme "option1"

  -- Add TabView
  let tabs : Array Components.TabDef := #[
    { label := "Tab 1", content := pure (bodyText "Content 1" theme) },
    { label := "Tab 2", content := pure (bodyText "Content 2" theme) },
    { label := "Tab 3", content := pure (bodyText "Content 3" theme) }
  ]
  let tabView ← Components.tabView tabViewName tabHeaderName tabs theme 0

  -- Add checkboxes
  let checkbox1 ← Components.checkbox checkbox1Name "Option 1" theme false
  let checkbox2 ← Components.checkbox checkbox2Name "Option 2" theme true

  -- Add sliders
  let slider1 ← Components.slider slider1Name (some "Volume") theme 0.3
  let slider2 ← Components.slider slider2Name (some "Brightness") theme 0.7

  -- Add text inputs (need shared focus state)
  let ctx ← liftSpider SpiderM.getTimelineCtx
  let (focusedInput, setFocusedInput) ← liftSpider <| SpiderM.liftIO <| Dynamic.new ctx (none : Option String)
  let textInput1 ← Components.textInput textInput1Name theme "Enter text..." "" focusedInput setFocusedInput
  let textInput2 ← Components.textInput textInput2Name theme "Type something..." "Hello, World!" focusedInput setFocusedInput

  -- Add modal
  let modalContent : ComponentRender := pure (bodyText "Modal content here" theme)
  let modal ← Components.modal modalName modalBackdropName modalCloseName "Test Modal" theme modalContent
  let modalTrigger ← Components.button modalTriggerName "Open Modal" theme .primary
  let _ ← liftSpider <| SpiderM.liftIO <| modalTrigger.onClick.subscribe fun _ => modal.openModal

  let render : ComponentRender := do
    let btn1 ← primaryBtn.render
    let sw1 ← switch1.render
    let dd1 ← dropdown1.render
    let radios ← radioGroup.render
    let tvw ← tabView.render
    let cb1 ← checkbox1.render
    let cb2 ← checkbox2.render
    let sl1 ← slider1.render
    let sl2 ← slider2.render
    let ti1 ← textInput1.render
    let ti2 ← textInput2.render
    let modalTrig ← modalTrigger.render
    let modalWidget ← modal.render
    pure (column (gap := 20) (style := {
      backgroundColor := some (Color.gray 0.1)
      padding := EdgeInsets.uniform 24
      width := .percent 1.0
      height := .percent 1.0
    }) #[
      heading1 "Reactive Showcase - TextInput2 Test" theme,
      row (gap := 8) (style := {}) #[btn1, sw1],
      dd1,
      radios,
      tvw,
      row (gap := 16) (style := {}) #[cb1, cb2],
      row (gap := 16) (style := {}) #[sl1, sl2],
      ti1,
      ti2,
      modalTrig,
      modalWidget
    ])

  pure { render }

/-  FULL VERSION - COMMENTED OUT FOR DEBUGGING
def createAppFull (env : DemoEnv) : ReactiveM AppState := do
  let ctx ← liftSpider SpiderM.getTimelineCtx

  -- Create theme
  let theme : Theme := { Theme.dark with font := env.fontCanopyId, smallFont := env.fontCanopySmallId }

  -- Create shared state for click counter
  let (buttonClickCount, incClickCount) ← liftSpider <| SpiderM.liftIO <| Dynamic.new ctx (0 : Nat)

  -- Create shared focus state for text inputs
  let (focusedInput, setFocusedInput) ← liftSpider <| SpiderM.liftIO <| Dynamic.new ctx (none : Option String)

  -- Create buttons (no events parameter - hooks access context implicitly)
  let primaryBtn ← Components.button btnPrimaryName "Primary" theme .primary
  let secondaryBtn ← Components.button btnSecondaryName "Secondary" theme .secondary
  let outlineBtn ← Components.button btnOutlineName "Outline" theme .outline
  let ghostBtn ← Components.button btnGhostName "Ghost" theme .ghost

  -- Wire button clicks to increment counter
  for btn in [primaryBtn, secondaryBtn, outlineBtn, ghostBtn] do
    let _ ← liftSpider <| SpiderM.liftIO <| btn.onClick.subscribe fun _ => do
      let n ← buttonClickCount.sample
      incClickCount (n + 1)

  -- Create checkboxes
  let checkbox1 ← Components.checkbox checkbox1Name "Option 1" theme false
  let checkbox2 ← Components.checkbox checkbox2Name "Option 2" theme true

  -- Create radio group
  let radioOptions : Array Components.RadioOption := #[
    { name := radio1Name, label := "Option 1", value := "option1" },
    { name := radio2Name, label := "Option 2", value := "option2" },
    { name := radio3Name, label := "Option 3", value := "option3" }
  ]
  let radioGroup ← Components.radioGroup radioOptions theme "option1"

  -- Create switches
  let switch1 ← Components.switch switch1Name (some "Notifications") theme false
  let switch2 ← Components.switch switch2Name (some "Dark Mode") theme true

  -- Create sliders
  let slider1 ← Components.slider slider1Name (some "Volume") theme 0.3
  let slider2 ← Components.slider slider2Name (some "Brightness") theme 0.7

  -- Create dropdown
  let dropdown1 ← Components.dropdown dropdown1Name dropdown1TriggerName
      dropdown1OptionName dropdown1Options theme 0

  -- Create text inputs (pass shared focus state)
  let textInput1 ← Components.textInput textInput1Name theme "Enter text here..." ""
      focusedInput setFocusedInput
  let textInput2 ← Components.textInput textInput2Name theme "Type something..." "Hello, World!"
      focusedInput setFocusedInput

  -- Create text area
  let textArea ← Components.textArea textAreaName theme "Enter multi-line text..."
      CanopyShowcaseState.initial.textAreaState focusedInput setFocusedInput

  -- Wire clicking non-input widgets to clear focus
  let allClicks ← useAllClicks
  let _ ← liftSpider <| SpiderM.liftIO <| allClicks.subscribe fun data => do
    let clickedInput1 := hitWidget data textInput1Name
    let clickedInput2 := hitWidget data textInput2Name
    let clickedTextArea := hitWidget data textAreaName
    if !clickedInput1 && !clickedInput2 && !clickedTextArea then
      -- Only clear if clicking on an interactive widget
      let clickedButton := hitWidget data btnPrimaryName || hitWidget data btnSecondaryName ||
                          hitWidget data btnOutlineName || hitWidget data btnGhostName
      let clickedCheckbox := hitWidget data checkbox1Name || hitWidget data checkbox2Name
      let clickedRadio := hitWidget data radio1Name || hitWidget data radio2Name || hitWidget data radio3Name
      let clickedSwitch := hitWidget data switch1Name || hitWidget data switch2Name
      let clickedSlider := hitWidget data slider1Name || hitWidget data slider2Name
      if clickedButton || clickedCheckbox || clickedRadio || clickedSwitch || clickedSlider then
        setFocusedInput none

  -- Create tab settings checkbox
  let tabSettingsCheckbox ← Components.checkbox tabSettingsCheckboxName "Enable feature" theme false

  -- Create tab view
  let tabs : Array Components.TabDef := #[
    { label := "Overview", content := pure (
        column (gap := 8) (style := {}) #[
          bodyText "TabView organizes content into separate panels." theme,
          bodyText "Click a tab to switch between panels." theme
        ])
    },
    { label := "Settings", content := do
        let cb ← tabSettingsCheckbox.render
        pure (column (gap := 8) (style := {}) #[
          caption "Sample settings panel:" theme,
          row (gap := 16) (style := {}) #[cb]
        ])
    },
    { label := "About", content := pure (
        column (gap := 4) (style := {}) #[
          heading3 "Reactive Showcase" theme,
          caption "Powered by data/reactive FRP" theme
        ])
    }
  ]
  let tabView ← Components.tabView tabViewName tabHeaderName tabs theme 0

  -- Create hover dynamics for modal buttons
  let confirmHovered ← useHover modalConfirmName
  let cancelHovered ← useHover modalCancelName

  -- Create modal content that samples the hover states
  let modalContent : ComponentRender := do
    let confirmH ← confirmHovered.sample
    let cancelH ← cancelHovered.sample

    let confirmState : WidgetState := { hovered := confirmH, pressed := false, focused := false }
    let cancelState : WidgetState := { hovered := cancelH, pressed := false, focused := false }

    -- Build button visuals inline
    let confirmBtn := Demos.demoButton modalConfirmName "Confirm" theme .primary confirmState
    let cancelBtn := Demos.demoButton modalCancelName "Cancel" theme .outline cancelState

    pure (column (gap := 16) (style := {}) #[
      bodyText "This is a modal dialog." theme,
      bodyText "Click outside, press Escape, or click a button to close." theme,
      row (gap := 12) (style := {}) #[confirmBtn, cancelBtn]
    ])

  let modal ← Components.modal modalName modalBackdropName modalCloseName
      "Sample Modal" theme modalContent

  -- Wire modal buttons to close
  let confirmClicks ← useClick modalConfirmName
  let cancelClicks ← useClick modalCancelName
  let _ ← liftSpider <| SpiderM.liftIO <| confirmClicks.subscribe fun _ => modal.closeModal
  let _ ← liftSpider <| SpiderM.liftIO <| cancelClicks.subscribe fun _ => modal.closeModal

  -- Create modal trigger button
  let modalTrigger ← Components.button modalTriggerName "Open Modal" theme .primary
  let _ ← liftSpider <| SpiderM.liftIO <| modalTrigger.onClick.subscribe fun _ => modal.openModal

  -- Note: Escape key handling is inside the Modal component itself

  -- Create the main render function
  -- Phase 1 (IO): Sample dynamics and get component renders
  -- Phase 2 (WidgetBuilder): Compose all widgets
  let render : ComponentRender := do
    -- IO phase: sample dynamics
    let clickCount ← buttonClickCount.sample

    -- IO phase: get all component renders (each returns WidgetBuilder)
    let btn1 ← primaryBtn.render
    let btn2 ← secondaryBtn.render
    let btn3 ← outlineBtn.render
    let btn4 ← ghostBtn.render
    let cb1 ← checkbox1.render
    let cb2 ← checkbox2.render
    let radios ← radioGroup.render
    let sw1 ← switch1.render
    let sw2 ← switch2.render
    let sl1 ← slider1.render
    let sl2 ← slider2.render
    let dd1 ← dropdown1.render
    let ti1 ← textInput1.render
    let ti2 ← textInput2.render
    let ta ← textArea.render
    let tabs ← tabView.render
    let modalTrig ← modalTrigger.render
    let modalWidget ← modal.render

    -- WidgetBuilder phase: compose all widgets
    pure (column (gap := 20) (style := {
      backgroundColor := some (Color.gray 0.1)
      padding := EdgeInsets.uniform 24
      width := .percent 1.0
      height := .percent 1.0
      flexItem := some (FlexItem.growing 1)
    }) #[
      -- Title
      heading1 "Reactive Showcase" theme,
      row (gap := 16) (style := {}) #[
        caption "FRP-powered widget demo using data/reactive" theme,
        if clickCount > 0 then
          caption s!"(Clicks: {clickCount})" theme
        else
          spacer 0 0
      ],

      -- Two-column layout
      flexRow { Trellis.FlexContainer.row 20 with alignItems := .flexStart }
          (style := { flexItem := some (FlexItem.growing 1) }) #[
        -- Left column
        column (gap := 16) (style := {}) #[
          -- Labels section
          titledPanel "Labels" .outlined theme (
            column (gap := 8) (style := {}) #[
              heading1 "Heading 1" theme,
              heading2 "Heading 2" theme,
              heading3 "Heading 3" theme,
              bodyText "Body text - normal paragraph content" theme,
              caption "Caption - small muted text" theme
            ]),

          -- Buttons section
          titledPanel "Buttons" .outlined theme (
            column (gap := 12) (style := {}) #[
              caption "Click a button to increment the counter:" theme,
              row (gap := 8) (style := {}) #[btn1, btn2, btn3, btn4]
            ]),

          -- Checkboxes section
          titledPanel "Checkboxes" .outlined theme (
            column (gap := 8) (style := {}) #[
              caption "Click to toggle:" theme,
              row (gap := 24) (style := {}) #[cb1, cb2]
            ]),

          -- Radio buttons section
          titledPanel "Radio Buttons" .outlined theme (
            column (gap := 8) (style := {}) #[
              caption "Click to select one option:" theme,
              radios
            ]),

          -- Switches section
          titledPanel "Switches" .outlined theme (
            column (gap := 8) (style := {}) #[
              caption "Click to toggle:" theme,
              row (gap := 24) (style := {}) #[sw1, sw2]
            ]),

          -- Modal section
          titledPanel "Modal" .outlined theme (
            column (gap := 8) (style := {}) #[
              caption "Click button to open modal:" theme,
              modalTrig
            ])
        ],

        -- Right column
        column (gap := 16) (style := {}) #[
          -- Sliders section
          titledPanel "Sliders" .outlined theme (
            column (gap := 8) (style := {}) #[
              caption "Click to adjust value:" theme,
              row (gap := 24) (style := {}) #[sl1, sl2]
            ]),

          -- Dropdown section
          titledPanel "Dropdown" .outlined theme (
            column (gap := 8) (style := {}) #[
              caption "Click to open, select an option:" theme,
              dd1
            ]),

          -- Text inputs section
          titledPanel "Text Inputs" .outlined theme (
            column (gap := 12) (style := {}) #[
              caption "Click to focus, then type:" theme,
              ti1,
              ti2
            ]),

          -- Text area section
          titledPanel "Text Area" .outlined theme (
            column (gap := 8) (style := {}) #[
              caption "Multi-line text with word wrapping:" theme,
              ta
            ]),

          -- Panels section
          titledPanel "Panels" .outlined theme (
            row (gap := 12) (style := {}) #[
              elevatedPanel theme 12 (
                column (gap := 4) (style := { minWidth := some 100 }) #[
                  heading3 "Elevated" theme,
                  caption "Card-like" theme
                ]),
              outlinedPanel theme 12 (
                column (gap := 4) (style := { minWidth := some 100 }) #[
                  heading3 "Outlined" theme,
                  caption "Border only" theme
                ]),
              filledPanel theme 12 (
                column (gap := 4) (style := { minWidth := some 100 }) #[
                  heading3 "Filled" theme,
                  caption "Solid bg" theme
                ])
            ]),

          -- Tab view section
          titledPanel "Tab View" .outlined theme (
            column (gap := 8) (style := {}) #[
              caption "Click tabs to switch content:" theme,
              tabs
            ])
        ]
      ],

      -- Modal overlay (renders on top when open)
      modalWidget
    ])

  pure { render }
-/

end Demos.ReactiveShowcase
