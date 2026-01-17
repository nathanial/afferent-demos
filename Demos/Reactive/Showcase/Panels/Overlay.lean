/-
  Overlay Panels - Modal triggers, toasts, menus, menu bars, and tooltips.
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

/-- Modal trigger panel - button to open the modal dialog.
    Returns the click event for external wiring. -/
def modalTriggerPanel (theme : Theme) : WidgetM (Reactive.Event Spider Unit) :=
  titledPanel' "Modal" .outlined theme do
    caption' "Click button to open modal:" theme
    button "Open Modal" theme .primary

/-- Toast triggers panel - buttons to show different toast notifications.
    Takes fire functions for each toast type. -/
def toastsPanel (theme : Theme)
    (fireInfo fireSuccess fireWarning fireError : Unit → IO Unit) : WidgetM Unit :=
  titledPanel' "Toasts" .outlined theme do
    caption' "Click to show notifications:" theme
    row' (gap := 8) (style := {}) do
      let infoClick ← button "Info" theme .primary
      let successClick ← button "Success" theme .primary
      let warnClick ← button "Warning" theme .secondary
      let errorClick ← button "Error" theme .secondary
      -- Wire clicks to toast triggers
      let infoAction ← Event.mapM (fun _ => fireInfo ()) infoClick
      let successAction ← Event.mapM (fun _ => fireSuccess ()) successClick
      let warnAction ← Event.mapM (fun _ => fireWarning ()) warnClick
      let errorAction ← Event.mapM (fun _ => fireError ()) errorClick
      performEvent_ infoAction
      performEvent_ successAction
      performEvent_ warnAction
      performEvent_ errorAction

/-- Menu panel - demonstrates trigger-based menu with actions and separators. -/
def menuPanel (theme : Theme) : WidgetM Unit :=
  titledPanel' "Menu" .outlined theme do
    caption' "Click button to open menu (hover submenus):" theme
    row' (gap := 16) (style := {}) do
      let items := #[
        MenuItem.action "Cut",
        MenuItem.action "Copy",
        MenuItem.action "Paste",
        MenuItem.separator,
        MenuItem.submenu "Format" #[
          MenuItem.action "Bold",
          MenuItem.action "Italic",
          MenuItem.action "Underline",
          MenuItem.separator,
          MenuItem.submenu "Text Color" #[
            MenuItem.action "Red",
            MenuItem.action "Green",
            MenuItem.action "Blue"
          ]
        ],
        MenuItem.separator,
        MenuItem.action "Delete" (enabled := false)
      ]
      let (_, menuResult) ← menu items theme (trigger := do
        let _ ← button "Actions" theme .primary
        pure ())
      -- Show when selection happens (path is now an array)
      let _ ← performEvent_ (← Event.mapM (fun path => do
        IO.println s!"Menu item selected at path: {path.toList}"
      ) menuResult.onSelect)
      pure ()

/-- MenuBar panel - demonstrates horizontal menu bar with multiple dropdown menus. -/
def menuBarPanel (theme : Theme) : WidgetM Unit :=
  titledPanel' "MenuBar" .outlined theme do
    caption' "Click menu triggers, hover to switch while open:" theme
    let fileMenu : MenuBarMenu := {
      label := "File"
      items := #[
        MenuItem.action "New",
        MenuItem.action "Open",
        MenuItem.action "Save",
        MenuItem.separator,
        MenuItem.action "Exit"
      ]
    }
    let editMenu : MenuBarMenu := {
      label := "Edit"
      items := #[
        MenuItem.action "Cut",
        MenuItem.action "Copy",
        MenuItem.action "Paste",
        MenuItem.separator,
        MenuItem.submenu "Format" #[
          MenuItem.action "Bold",
          MenuItem.action "Italic",
          MenuItem.action "Underline"
        ]
      ]
    }
    let viewMenu : MenuBarMenu := {
      label := "View"
      items := #[
        MenuItem.action "Zoom In",
        MenuItem.action "Zoom Out",
        MenuItem.separator,
        MenuItem.action "Full Screen"
      ]
    }
    let helpMenu : MenuBarMenu := {
      label := "Help"
      items := #[
        MenuItem.action "Documentation",
        MenuItem.action "About"
      ]
      enabled := true
    }
    let result ← menuBar #[fileMenu, editMenu, viewMenu, helpMenu] theme
    let _ ← performEvent_ (← Event.mapM (fun path => do
      IO.println s!"MenuBar selected: menu {path.menuIndex}, path {path.itemPath.toList}"
    ) result.onSelect)
    pure ()

/-- Tooltips panel - demonstrates hover tooltips with different positions. -/
def tooltipsPanel (theme : Theme) (font : Afferent.Font) : WidgetM Unit :=
  titledPanel' "Tooltips" .outlined theme do
    caption' "Hover over buttons to see tooltips:" theme
    row' (gap := 12) (style := {}) do
      -- Tooltip above
      let (_, _) ← tooltipTop "Appears above" theme font (delay := 0.3) do
        let _ ← button "Top" theme .outline
        pure ()
      -- Tooltip below
      let (_, _) ← tooltipBottom "Appears below" theme font (delay := 0.3) do
        let _ ← button "Bottom" theme .outline
        pure ()
      -- Tooltip left
      let (_, _) ← tooltipLeft "Appears left" theme font (delay := 0.3) do
        let _ ← button "Left" theme .outline
        pure ()
      -- Tooltip right
      let (_, _) ← tooltipRight "Appears right" theme font (delay := 0.3) do
        let _ ← button "Right" theme .outline
        pure ()
      pure ()

end Demos.ReactiveShowcase
