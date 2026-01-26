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
def badgePanel : WidgetM Unit :=
  titledPanel' "Badge" .outlined do
    caption' "Status indicators with color variants:"
    row' (gap := 8) (style := {}) do
      badge' "New" .primary
      badge' "Beta" .secondary
      badge' "Success" .success
      badge' "Warning" .warning
      badge' "Error" .error
      badge' "Info" .info

    spacer' 0 8

    caption' "Notification counts:"
    row' (gap := 8) (style := {}) do
      badgeCount' 3 .primary
      badgeCount' 42 .error
      badgeCount' 100 .success  -- Shows 99+

/-- Chip panel - demonstrates chip variants and removal. -/
def chipPanel : WidgetM Unit :=
  titledPanel' "Chip" .outlined do
    caption' "Filled chips:"
    row' (gap := 8) (style := {}) do
      simpleChip "React" .filled
      simpleChip "TypeScript" .filled
      simpleChip "Lean 4" .filled

    spacer' 0 8

    caption' "Outlined chips:"
    row' (gap := 8) (style := {}) do
      simpleChip "Frontend" .outlined
      simpleChip "Backend" .outlined

    spacer' 0 8

    caption' "Removable chips (click × to remove):"
    row' (gap := 8) (style := {}) do
      let result1 ← chip "Removable" .filled true
      let result2 ← chip "Also Removable" .outlined true
      -- Log removal events for demo
      match result1.onRemove with
      | some evt => performEvent_ (← Event.mapM (fun _ => IO.println "Chip 1 removed") evt)
      | none => pure ()
      match result2.onRemove with
      | some evt => performEvent_ (← Event.mapM (fun _ => IO.println "Chip 2 removed") evt)
      | none => pure ()

/-- Avatar panel - demonstrates avatar sizes and labels. -/
def avatarPanel : WidgetM Unit :=
  titledPanel' "Avatar" .outlined do
    caption' "Avatar sizes:"
    flexRow' { FlexContainer.row 16 with alignItems := .center } (style := {}) do
      avatar' "S" .small
      avatar' "M" .medium
      avatar' "L" .large

    spacer' 0 8

    caption' "Avatars with labels:"
    column' (gap := 8) (style := {}) do
      avatarWithLabel' "JD" "John Doe" .medium
      avatarWithLabel' "AS" "Alice Smith" .medium
      avatarWithLabel' "BC" "Bob Clark" .medium (some (Color.fromRgb8 139 92 246))

/-- Link panel - demonstrates clickable links with hover effect. -/
def linkPanel : WidgetM Unit :=
  titledPanel' "Link" .outlined do
    caption' "Clickable links with hover effect:"
    column' (gap := 8) (style := {}) do
      let click1 ← link "Documentation"
      let click2 ← link "GitHub Repository" (some (Color.fromRgb8 139 92 246))
      let click3 ← linkWithIcon "External Link" "↗"

      -- Track click count
      let allClicks ← Event.leftmostM [click1, click2, click3]
      let clickCount ← Reactive.foldDyn (fun _ n => n + 1) 0 allClicks

      spacer' 0 4

      let _ ← dynWidget clickCount fun count =>
        if count > 0 then caption' s!"Links clicked: {count}"
        else spacer' 0 0

end Demos.ReactiveShowcase
