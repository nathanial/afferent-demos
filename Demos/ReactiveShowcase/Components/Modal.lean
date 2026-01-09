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
def modal (containerName : String) (backdropName : String) (closeName : String)
    (title : String) (theme : Theme)
    (content : ComponentRender)
    : ReactiveM ModalComponent := do
  let ctx ← liftSpider SpiderM.getTimelineCtx

  -- Create close button hover state
  let isCloseHovered ← useHover closeName

  -- Create open state
  let (isOpen, setOpen) ← liftSpider <| SpiderM.liftIO <| Dynamic.new ctx false

  -- Create onClose event
  let (onClose, fireClose) ← liftSpider <| newTriggerEvent (t := Spider) (a := Unit)

  -- Helper to close modal
  let closeModal : IO Unit := do
    let open_ ← isOpen.sample
    if open_ then
      setOpen false
      fireClose ()

  -- Wire close button click
  let closeClicks ← useClick closeName
  let _ ← liftSpider <| SpiderM.liftIO <| closeClicks.subscribe fun _ => closeModal

  -- Wire backdrop click (click outside modal)
  let allClicks ← useAllClicks
  let _ ← liftSpider <| SpiderM.liftIO <| allClicks.subscribe fun data => do
    let open_ ← isOpen.sample
    if open_ then
      let clickedBackdrop := hitWidget data backdropName
      let clickedModal := hitWidget data containerName
      if clickedBackdrop && !clickedModal then
        closeModal

  -- Wire escape key
  let keyEvents ← useKeyboard
  let _ ← liftSpider <| SpiderM.liftIO <| keyEvents.subscribe fun keyData => do
    let open_ ← isOpen.sample
    if open_ && keyData.event.key == .escape && keyData.event.isPress then
      closeModal

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
    openModal := setOpen true
    closeModal
    render
  }

end Demos.ReactiveShowcase.Components
