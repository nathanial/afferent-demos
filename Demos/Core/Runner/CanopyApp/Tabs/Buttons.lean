/-
  Demo Runner - Canopy app buttons tab content.
-/
import Reactive
import Afferent
import Afferent.Canopy
import Afferent.Canopy.Reactive
import Demos.Core.Demo
import Trellis

open Reactive Reactive.Host
open Afferent
open Afferent.Arbor
open Afferent.Canopy
open Afferent.Canopy.Reactive
open Trellis

namespace Demos

def buttonsTabContent (env : DemoEnv) : WidgetM Unit := do
  let scrollConfig : ScrollContainerConfig := {
    width := env.windowWidthF
    height := env.windowHeightF
    fillWidth := true
    fillHeight := true
    scrollbarVisibility := .always
  }

  let _ ← scrollContainer scrollConfig do
    column' (gap := 24) (style := { width := .percent 1.0 }) do
      heading2' "Standard"
      row' (gap := 12) (style := { flexItem := some (FlexItem.growing 0) }) do
        let _ ← button "Primary" .primary
        let _ ← button "Secondary" .secondary
        let _ ← button "Outline" .outline
        let _ ← button "Ghost" .ghost
        let _ ← dangerButton "Danger"
        let _ ← successButton "Success"

      heading2' "Icons"
      row' (gap := 12) (style := { flexItem := some (FlexItem.growing 0) }) do
        let _ ← iconButton "+" .secondary
        let _ ← iconLabelButton "Add" "+"
        let _ ← iconLabelButton "Next" ">" (iconPosition := .trailing)

      heading2' "FAB"
      row' (gap := 12) (style := { flexItem := some (FlexItem.growing 0) }) do
        let _ ← fabButton "+"
        let _ ← miniFabButton "+"
        let _ ← extendedFabButton "Create" "+"

      heading2' "Shapes"
      row' (gap := 12) (style := { flexItem := some (FlexItem.growing 0) }) do
        let _ ← pillButton "Pill"
        let _ ← compactButton "Compact"
        let _ ← compactButton "Compact" .secondary (icon := some "+")
        let _ ← linkButton "Link"
        let _ ← linkButtonWithIcon "Docs" ">"

      heading2' "Toggle"
      column' (gap := 8) (style := {}) do
        row' (gap := 12) (style := {}) do
          let _ ← toggleButton "Toggle"
          let _ ← toggleButton "Active" (initialOn := true)
        row' (gap := 12) (style := {}) do
          let group ← toggleGroup #["Day", "Week", "Month"] 0
          let _ ← dynWidget group.selection fun idx =>
            caption' s!"ToggleGroup selection: {idx}"
          pure ()

      heading2' "Split & Dropdown"
      row' (gap := 12) (style := { flexItem := some (FlexItem.growing 0) }) do
        let _ ← splitButton "Export"
        let _ ← dropdownButton #["Option A", "Option B", "Option C"] 0

      heading2' "Loading"
      column' (gap := 8) (style := {}) do
        let loadingToggle ← toggleButton "Loading" (initialOn := true)
        row' (gap := 12) (style := {}) do
          let _ ← loadingButton "Save" loadingToggle.isOn
          let _ ← loadingButton "Submit" loadingToggle.isOn .secondary
        pure ()

  pure ()

end Demos
