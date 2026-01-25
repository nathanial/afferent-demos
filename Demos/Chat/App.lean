/-
  Chat Demo App - Showcases the Chat widget with AI conversation streaming.
-/
import Reactive
import Oracle.Reactive.Client
import Afferent
import Afferent.Canopy
import Afferent.Canopy.Reactive
import Afferent.Canopy.Widget.Chat
import Demos.Core.Demo

open Reactive Reactive.Host
open Afferent CanvasM
open Afferent.Arbor
open Afferent.Canopy
open Afferent.Canopy.Reactive
open Afferent.Canopy.Chat
open Oracle.Reactive
open Trellis

namespace Demos.ChatDemo

/-- Application state returned from create. -/
structure AppState where
  /-- Render function that samples all component state and returns the complete UI. -/
  render : ComponentRender

/-- Create the chat demo application.
    Sets up the reactive chat widget with Oracle API integration. -/
def createApp (env : DemoEnv) : ReactiveM AppState := do
  let theme : Theme := { Theme.dark with font := env.fontCanopyId, smallFont := env.fontCanopySmallId }
  let events ← getEvents

  -- Try to get API key from environment
  let apiKeyOpt ← (IO.getEnv "OPENROUTER_API_KEY" : IO (Option String))

  let (_, render) ← runWidget do
    let rootStyle : BoxStyle := {
      backgroundColor := some (Color.gray 0.1)
      padding := EdgeInsets.uniform 24
      width := .percent 1.0
      height := .percent 1.0
      flexItem := some (FlexItem.growing 1)
    }

    column' (gap := 20) (style := rootStyle) do
      -- Title
      heading1' "Chat Widget Demo" theme
      caption' "AI conversation interface with streaming support" theme

      -- Content area
      let contentStyle : BoxStyle := {
        flexItem := some (FlexItem.growing 1)
        width := .percent 1.0
        height := .percent 1.0
      }

      column' (gap := 0) (style := contentStyle) do
        match apiKeyOpt with
        | some apiKey =>
          -- Create the chat widget with API key
          let client := ReactiveClient.withModel apiKey "anthropic/claude-sonnet-4"
          let chatConfig : ChatWidgetConfig := {
            width := 800
            height := 600
            maxMessageWidth := 0.8
            autoScroll := true
            inputPlaceholder := "Type a message..."
            systemPrompt := some "You are a helpful assistant."
          }
          let _ ← chatWidget client theme env.fontCanopy chatConfig
          pure ()
        | none =>
          -- Show error message if no API key
          column' (gap := 16) (style := { padding := EdgeInsets.uniform 32 }) do
            heading2' "API Key Required" theme
            bodyText' "Set the OPENROUTER_API_KEY environment variable to use this demo." theme
            caption' "Example: export OPENROUTER_API_KEY=your-key-here" theme

  -- Set up automatic focus clearing
  events.registry.setupFocusClearing

  pure { render }

end Demos.ChatDemo
