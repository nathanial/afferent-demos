/-
  Display Panels - Badges, chips, avatars, and links.
-/
import Reactive
import Afferent
import Afferent.Canopy
import Afferent.Canopy.Reactive

open Reactive Reactive.Host
open Afferent CanvasM
open Afferent.Arbor
open Afferent.Canopy
open Afferent.Canopy.Reactive
open Trellis

namespace Demos.ReactiveShowcase

/-- Badge panel - demonstrates badge variants. -/
def badgePanel (theme : Theme) : WidgetM Unit :=
  titledPanel' "Badge" .outlined theme do
    caption' "Status indicators with color variants:" theme
    row' (gap := 8) (style := {}) do
      badge' "New" theme .primary
      badge' "Beta" theme .secondary
      badge' "Success" theme .success
      badge' "Warning" theme .warning
      badge' "Error" theme .error
      badge' "Info" theme .info

    spacer' 0 8

    caption' "Notification counts:" theme
    row' (gap := 8) (style := {}) do
      badgeCount' 3 theme .primary
      badgeCount' 42 theme .error
      badgeCount' 100 theme .success  -- Shows 99+

/-- Chip panel - demonstrates chip variants and removal. -/
def chipPanel (theme : Theme) : WidgetM Unit :=
  titledPanel' "Chip" .outlined theme do
    caption' "Filled chips:" theme
    row' (gap := 8) (style := {}) do
      simpleChip "React" theme .filled
      simpleChip "TypeScript" theme .filled
      simpleChip "Lean 4" theme .filled

    spacer' 0 8

    caption' "Outlined chips:" theme
    row' (gap := 8) (style := {}) do
      simpleChip "Frontend" theme .outlined
      simpleChip "Backend" theme .outlined

    spacer' 0 8

    caption' "Removable chips (click × to remove):" theme
    row' (gap := 8) (style := {}) do
      let result1 ← chip "Removable" theme .filled true
      let result2 ← chip "Also Removable" theme .outlined true
      -- Log removal events for demo
      match result1.onRemove with
      | some evt => performEvent_ (← Event.mapM (fun _ => IO.println "Chip 1 removed") evt)
      | none => pure ()
      match result2.onRemove with
      | some evt => performEvent_ (← Event.mapM (fun _ => IO.println "Chip 2 removed") evt)
      | none => pure ()

/-- Avatar panel - demonstrates avatar sizes and labels. -/
def avatarPanel (theme : Theme) : WidgetM Unit :=
  titledPanel' "Avatar" .outlined theme do
    caption' "Avatar sizes:" theme
    flexRow' { FlexContainer.row 16 with alignItems := .center } (style := {}) do
      avatar' "S" theme .small
      avatar' "M" theme .medium
      avatar' "L" theme .large

    spacer' 0 8

    caption' "Avatars with labels:" theme
    column' (gap := 8) (style := {}) do
      avatarWithLabel' "JD" "John Doe" theme .medium
      avatarWithLabel' "AS" "Alice Smith" theme .medium
      avatarWithLabel' "BC" "Bob Clark" theme .medium (some (Color.fromRgb8 139 92 246))

/-- Link panel - demonstrates clickable links with hover effect. -/
def linkPanel (theme : Theme) : WidgetM Unit :=
  titledPanel' "Link" .outlined theme do
    caption' "Clickable links with hover effect:" theme
    column' (gap := 8) (style := {}) do
      let click1 ← link "Documentation" theme
      let click2 ← link "GitHub Repository" theme (some (Color.fromRgb8 139 92 246))
      let click3 ← linkWithIcon "External Link" "↗" theme

      -- Track click count
      let allClicks ← Event.leftmostM [click1, click2, click3]
      let clickCount ← Reactive.foldDyn (fun _ n => n + 1) 0 allClicks

      spacer' 0 4

      let _ ← dynWidget clickCount fun count =>
        if count > 0 then caption' s!"Links clicked: {count}" theme
        else spacer' 0 0

end Demos.ReactiveShowcase
