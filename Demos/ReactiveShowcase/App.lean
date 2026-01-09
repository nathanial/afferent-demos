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
  let ctx ← liftSpider SpiderM.getTimelineCtx
  let theme : Theme := { Theme.dark with font := env.fontCanopyId, smallFont := env.fontCanopySmallId }

  -- Create shared state for click counter
  let (buttonClickCount, incClickCount) ← liftSpider <| SpiderM.liftIO <| Dynamic.new ctx (0 : Nat)

  -- Create shared focus state for text inputs
  let (focusedInput, setFocusedInput) ← liftSpider <| SpiderM.liftIO <| Dynamic.new ctx (none : Option String)

  -- Create buttons
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

  -- Create text inputs
  let textInput1 ← Components.textInput textInput1Name theme "Enter text here..." ""
      focusedInput setFocusedInput
  let textInput2 ← Components.textInput textInput2Name theme "Type something..." "Hello, World!"
      focusedInput setFocusedInput

  -- Create tab view (simple version for testing)
  let tabs : Array Components.TabDef := #[
    { label := "Tab 1", content := pure (bodyText "Content 1" theme) },
    { label := "Tab 2", content := pure (bodyText "Content 2" theme) },
    { label := "Tab 3", content := pure (bodyText "Content 3" theme) }
  ]
  let tabView ← Components.tabView tabViewName tabHeaderName tabs theme 0

  -- Create modal (simple version for testing)
  let modalContent : ComponentRender := pure (bodyText "Modal content" theme)
  let modal ← Components.modal modalName modalBackdropName modalCloseName
      "Test Modal" theme modalContent
  let modalTrigger ← Components.button modalTriggerName "Open Modal" theme .primary
  let _ ← liftSpider <| SpiderM.liftIO <| modalTrigger.onClick.subscribe fun _ => modal.openModal

  -- Create the main render function
  let render : ComponentRender := do
    let clickCount ← buttonClickCount.sample
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
    let tvw ← tabView.render
    let modalTrig ← modalTrigger.render
    let modalWidget ← modal.render

    pure (column (gap := 20) (style := {
      backgroundColor := some (Color.gray 0.1)
      padding := EdgeInsets.uniform 24
      width := .percent 1.0
      height := .percent 1.0
    }) #[
      heading1 "Reactive Showcase" theme,
      caption s!"(Clicks: {clickCount})" theme,
      row (gap := 8) (style := {}) #[btn1, btn2, btn3, btn4],
      row (gap := 16) (style := {}) #[cb1, cb2],
      radios,
      row (gap := 16) (style := {}) #[sw1, sw2],
      row (gap := 16) (style := {}) #[sl1, sl2],
      dd1,
      ti1,
      ti2,
      tvw,
      modalTrig,
      modalWidget
    ])

  pure { render }

end Demos.ReactiveShowcase
