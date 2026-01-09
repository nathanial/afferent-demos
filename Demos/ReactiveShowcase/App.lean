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
  let theme : Theme := { Theme.dark with font := env.fontCanopyId, smallFont := env.fontCanopySmallId }
  let events ← getEvents

  -- All 4 button variants (no names needed!)
  let primaryBtn ← Components.button "Primary" theme .primary
  let secondaryBtn ← Components.button "Secondary" theme .secondary
  let outlineBtn ← Components.button "Outline" theme .outline
  let ghostBtn ← Components.button "Ghost" theme .ghost

  -- Merge all button clicks using proper FRP combinators
  let clicks12 ← liftSpider <| Event.mergeM primaryBtn.onClick secondaryBtn.onClick
  let clicks123 ← liftSpider <| Event.mergeM clicks12 outlineBtn.onClick
  let allButtonClicks ← liftSpider <| Event.mergeM clicks123 ghostBtn.onClick

  -- Click counter using foldDyn (proper FRP - no manual subscriptions!)
  let buttonClickCount ← liftSpider <| foldDyn (fun _ n => n + 1) 0 allButtonClicks

  -- Switches
  let switch1 ← Components.switch (some "Notifications") theme false
  let switch2 ← Components.switch (some "Dark Mode") theme true

  -- Dropdown
  let dropdownOptions := #["Option 1", "Option 2", "Option 3"]
  let dropdown1 ← Components.dropdown dropdownOptions theme 0

  -- Radio group (no names needed in options!)
  let radioOptions : Array Components.RadioOption := #[
    { label := "Option 1", value := "option1" },
    { label := "Option 2", value := "option2" },
    { label := "Option 3", value := "option3" }
  ]
  let radioGroup ← Components.radioGroup radioOptions theme "option1"

  -- TabView
  let tabs : Array Components.TabDef := #[
    { label := "Tab 1", content := pure (bodyText "Content 1" theme) },
    { label := "Tab 2", content := pure (bodyText "Content 2" theme) },
    { label := "Tab 3", content := pure (bodyText "Content 3" theme) }
  ]
  let tabView ← Components.tabView tabs theme 0

  -- Checkboxes
  let checkbox1 ← Components.checkbox "Option 1" theme false
  let checkbox2 ← Components.checkbox "Option 2" theme true

  -- Sliders
  let slider1 ← Components.slider (some "Volume") theme 0.3
  let slider2 ← Components.slider (some "Brightness") theme 0.7

  -- Text inputs (focus coordination is automatic via registry!)
  -- TEMPORARILY DISABLED FOR DEBUGGING:
  -- let textInput1 ← Components.textInput theme "Enter text..." ""
  -- let textInput2 ← Components.textInput theme "Type something..." "Hello, World!"
  -- let textArea ← Components.textArea theme "Enter multi-line text..." {} env.fontCanopy
  pure ()

  -- Modal
  let modalContent : ComponentRender := pure (bodyText "Modal content here" theme)
  let modal ← Components.modal "Test Modal" theme modalContent
  let modalTrigger ← Components.button "Open Modal" theme .primary
  -- Pure FRP: map to IO action and use performEvent_
  let openModalAction ← liftSpider <| Event.mapM (fun _ => modal.openModal) modalTrigger.onClick
  liftSpider <| performEvent_ openModalAction

  -- Automatic focus clearing based on registry (replaces all manual hit testing!)
  -- TEMPORARILY DISABLED FOR DEBUGGING:
  -- events.registry.setupFocusClearing
  pure ()

  let render : ComponentRender := do
    let clickCount ← buttonClickCount.sample
    let btn1 ← primaryBtn.render
    let btn2 ← secondaryBtn.render
    let btn3 ← outlineBtn.render
    let btn4 ← ghostBtn.render
    let sw1 ← switch1.render
    let sw2 ← switch2.render
    let dd1 ← dropdown1.render
    let radios ← radioGroup.render
    let tvw ← tabView.render
    let cb1 ← checkbox1.render
    let cb2 ← checkbox2.render
    let sl1 ← slider1.render
    let sl2 ← slider2.render
    -- TEMPORARILY DISABLED FOR DEBUGGING:
    -- let ti1 ← textInput1.render
    -- let ti2 ← textInput2.render
    -- let ta ← textArea.render
    let ti1 := spacer 0 0
    let ti2 := spacer 0 0
    let ta := spacer 0 0
    let modalTrig ← modalTrigger.render
    let modalWidget ← modal.render
    pure (column (gap := 20) (style := {
      backgroundColor := some (Color.gray 0.1)
      padding := EdgeInsets.uniform 24
      width := .percent 1.0
      height := .percent 1.0
    }) #[
      heading1 "Reactive Showcase" theme,
      row (gap := 16) (style := {}) #[
        caption "FRP-powered widget demo" theme,
        if clickCount > 0 then caption s!"(Clicks: {clickCount})" theme else spacer 0 0
      ],
      row (gap := 8) (style := {}) #[btn1, btn2, btn3, btn4],
      row (gap := 24) (style := {}) #[sw1, sw2],
      dd1,
      radios,
      tvw,
      row (gap := 16) (style := {}) #[cb1, cb2],
      row (gap := 16) (style := {}) #[sl1, sl2],
      ti1,
      ti2,
      ta,
      modalTrig,
      modalWidget
    ])

  pure { render }

end Demos.ReactiveShowcase
