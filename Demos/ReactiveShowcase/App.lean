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

  -- All 4 button variants
  let primaryBtn ← Components.button "Primary" theme .primary
  let secondaryBtn ← Components.button "Secondary" theme .secondary
  let outlineBtn ← Components.button "Outline" theme .outline
  let ghostBtn ← Components.button "Ghost" theme .ghost

  -- Merge all button clicks using proper FRP combinators
  let clicks12 ← Event.mergeM primaryBtn.onClick secondaryBtn.onClick
  let clicks123 ← Event.mergeM clicks12 outlineBtn.onClick
  let allButtonClicks ← Event.mergeM clicks123 ghostBtn.onClick

  -- Click counter using foldDyn (proper FRP - no manual subscriptions!)
  let buttonClickCount ← foldDyn (fun _ n => n + 1) 0 allButtonClicks

  -- Checkboxes
  let checkbox1 ← Components.checkbox "Option 1" theme false
  let checkbox2 ← Components.checkbox "Option 2" theme true

  -- Radio group
  let radioOptions : Array Components.RadioOption := #[
    { label := "Option 1", value := "option1" },
    { label := "Option 2", value := "option2" },
    { label := "Option 3", value := "option3" }
  ]
  let radioGroup ← Components.radioGroup radioOptions theme "option1"

  -- Switches
  let switch1 ← Components.switch (some "Notifications") theme false
  let switch2 ← Components.switch (some "Dark Mode") theme true

  -- Modal
  let modalContent : ComponentRender := do
    pure (column (gap := 16) (style := {}) #[
      bodyText "This is a modal dialog." theme,
      bodyText "Click outside, press Escape, or click a button to close." theme
    ])
  let modal ← Components.modal "Sample Modal" theme modalContent
  let modalTrigger ← Components.button "Open Modal" theme .primary
  let openModalAction ← Event.mapM (fun _ => modal.openModal) modalTrigger.onClick
  performEvent_ openModalAction

  -- Sliders
  let slider1 ← Components.slider (some "Volume") theme 0.3
  let slider2 ← Components.slider (some "Brightness") theme 0.7

  -- Dropdown
  let dropdownOptions := #["Apple", "Banana", "Cherry", "Date", "Elderberry"]
  let dropdown1 ← Components.dropdown dropdownOptions theme 0

  -- Text inputs
  let textInput1 ← Components.textInput theme "Enter text here..." ""
  let textInput2 ← Components.textInput theme "Type something..." "Hello, World!"
  let textArea ← Components.textArea theme "Enter multi-line text..." {} env.fontCanopy

  -- TabView with rich content
  -- Note: Settings tab checkbox is independent (not shared with main checkbox1)
  let settingsCheckbox ← Components.checkbox "Enable feature" theme false
  let tabs : Array Components.TabDef := #[
    { label := "Overview", content := do
        pure (column (gap := 8) (style := {}) #[
          bodyText "TabView organizes content into separate panels." theme,
          bodyText "Click a tab to switch between panels." theme
        ])
    },
    { label := "Settings", content := do
        pure (column (gap := 8) (style := {}) #[
          caption "Sample settings panel:" theme,
          row (gap := 16) (style := {}) #[← settingsCheckbox.render]
        ])
    },
    { label := "About", content := do
        pure (column (gap := 4) (style := {}) #[
          heading3 "Reactive Widgets" theme,
          caption "Version 1.0.0" theme
        ])
    }
  ]
  let tabView ← Components.tabView tabs theme 0

  -- Automatic focus clearing based on registry
  events.registry.setupFocusClearing

  let render : ComponentRender := do
    let clickCount ← buttonClickCount.sample
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
        caption "FRP-powered widget demo" theme,
        if clickCount > 0 then caption s!"(Clicks: {clickCount})" theme else spacer 0 0
      ],

      -- Two-column layout
      flexRow { FlexContainer.row 20 with alignItems := .flexStart }
          (style := { flexItem := some (FlexItem.growing 1) }) #[
        -- Left column
        column (gap := 16) (style := {}) #[
          -- Labels section (non-interactive, no IO needed)
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
              row (gap := 8) (style := {}) #[← primaryBtn.render, ← secondaryBtn.render, ← outlineBtn.render, ← ghostBtn.render]
            ]),

          -- Checkboxes section
          titledPanel "Checkboxes" .outlined theme (
            column (gap := 8) (style := {}) #[
              caption "Click to toggle:" theme,
              row (gap := 24) (style := {}) #[← checkbox1.render, ← checkbox2.render]
            ]),

          -- Radio Buttons section
          titledPanel "Radio Buttons" .outlined theme (
            column (gap := 8) (style := {}) #[
              caption "Click to select one option:" theme,
              ← radioGroup.render
            ]),

          -- Switches section
          titledPanel "Switches" .outlined theme (
            column (gap := 8) (style := {}) #[
              caption "Click to toggle:" theme,
              row (gap := 24) (style := {}) #[← switch1.render, ← switch2.render]
            ]),

          -- Modal section
          titledPanel "Modal" .outlined theme (
            column (gap := 8) (style := {}) #[
              caption "Click button to open modal:" theme,
              ← modalTrigger.render
            ])
        ],

        -- Right column
        column (gap := 16) (style := {}) #[
          -- Sliders section
          titledPanel "Sliders" .outlined theme (
            column (gap := 8) (style := {}) #[
              caption "Click to adjust value:" theme,
              row (gap := 24) (style := {}) #[← slider1.render, ← slider2.render]
            ]),

          -- Dropdown section
          titledPanel "Dropdown" .outlined theme (
            column (gap := 8) (style := {}) #[
              caption "Click to open, select an option:" theme,
              ← dropdown1.render
            ]),

          -- Text Input section
          titledPanel "Text Inputs" .outlined theme (
            column (gap := 12) (style := {}) #[
              caption "Click to focus, then type:" theme,
              ← textInput1.render,
              ← textInput2.render
            ]),

          -- Text Area section
          titledPanel "Text Area" .outlined theme (
            column (gap := 8) (style := {}) #[
              caption "Multi-line text with word wrapping:" theme,
              ← textArea.render
            ]),

          -- Panels section (non-interactive)
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

          -- TabView section
          titledPanel "Tab View" .outlined theme (
            column (gap := 8) (style := {}) #[
              caption "Click tabs to switch content:" theme,
              ← tabView.render
            ])
        ]
      ],

      -- Modal overlay (renders on top when open)
      ← modal.render
    ])

  pure { render }

end Demos.ReactiveShowcase
