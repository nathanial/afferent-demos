/-
  Modal Component - Self-contained FRP modal dialog with backdrop and close behavior.
-/
import Reactive
import Afferent
import Afferent.Canopy
import Demos.ReactiveShowcase.Types
import Demos.ReactiveShowcase.Component

open Reactive Reactive.Host
open Afferent CanvasM
open Afferent.Arbor
open Afferent.Canopy
open Trellis

namespace Demos.ReactiveShowcase.Components

/-- Modal component output - exposes open state and render function. -/
structure ModalComponent where
  /-- Event that fires when the modal is closed (by any method). -/
  onClose : Event Spider Unit
  /-- Whether the modal is open as a Dynamic. -/
  isOpen : Dynamic Spider Bool
  /-- Open the modal. -/
  openModal : IO Unit
  /-- Close the modal. -/
  closeModal : IO Unit
  /-- Render function that samples state and returns the modal widget (or empty if closed). -/
  render : ComponentRender

/-- Create a self-contained modal component.
    The component manages its own open/close state and close-on-backdrop/escape behavior. -/
def modal (title : String) (theme : Theme) (content : ComponentRender)
    : ReactiveM ModalComponent := do
  -- Auto-generate names via registry
  let containerName ← registerComponent "modal" (isInteractive := false)
  let backdropName ← registerComponent "modal-backdrop" (isInteractive := false)
  let closeName ← registerComponent "modal-close"

  -- Create close button hover state
  let isCloseHovered ← useHover closeName

  -- Get event streams
  let closeClicks ← useClick closeName
  let allClicks ← useAllClicks
  let keyEvents ← useKeyboard

  -- Create open trigger for external callers (openModal/closeModal)
  let (openTrigger, fireOpen) ← newTriggerEvent (t := Spider) (a := Bool)

  -- Helper: check if click is on backdrop but not modal
  let isBackdropClick (data : ClickData) : Bool :=
    hitWidget data backdropName && !hitWidget data containerName

  -- Helper: check if escape key pressed
  let isEscapePress (keyData : KeyData) : Bool :=
    keyData.event.key == .escape && keyData.event.isPress

  -- isOpen: use fixDynM because backdrop/escape close depend on current isOpen state
  let isOpen ← SpiderM.fixDynM fun isOpenBehavior => do
    -- Open/close from external trigger
    let triggerEvents ← Event.mapM (fun open_ => fun _ => open_) openTrigger

    -- Close on close button click
    let closeFromButton ← Event.mapM (fun _ => fun _ => false) closeClicks

    -- Close on backdrop click (gated by isOpen)
    let backdropClicks ← Event.filterM isBackdropClick allClicks
    let gatedBackdrop ← Event.gateM isOpenBehavior backdropClicks
    let closeFromBackdrop ← Event.mapM (fun _ => fun _ => false) gatedBackdrop

    -- Close on escape key (gated by isOpen)
    let escapeKeys ← Event.filterM isEscapePress keyEvents
    let gatedEscape ← Event.gateM isOpenBehavior escapeKeys
    let closeFromEscape ← Event.mapM (fun _ => fun _ => false) gatedEscape

    -- Merge all state transitions (leftmost wins on simultaneous)
    let allTransitions ← Event.leftmostM [closeFromButton, closeFromBackdrop, closeFromEscape, triggerEvents]

    -- Apply transitions to state
    foldDyn (fun f s => f s) false allTransitions

  -- onClose: fires when isOpen transitions from true to false
  let closeEvents ← Event.filterM (fun open_ => !open_) isOpen.updated
  let onClose ← Event.mapM (fun _ => ()) closeEvents

  -- Render function
  let render : ComponentRender := do
    let open_ ← isOpen.sample
    if open_ then
      let closeHovered ← isCloseHovered.sample
      let contentWidget ← content
      pure (modalVisual containerName backdropName closeName title true theme closeHovered {} contentWidget)
    else
      pure (spacer 0 0)

  pure {
    onClose
    isOpen
    openModal := fireOpen true
    closeModal := fireOpen false
    render
  }

end Demos.ReactiveShowcase.Components
