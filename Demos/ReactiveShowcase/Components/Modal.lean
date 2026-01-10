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

/-- Modal result - events, dynamics, and control functions. -/
structure ModalResult where
  onClose : Event Spider Unit
  isOpen : Dynamic Spider Bool
  openModal : IO Unit
  closeModal : IO Unit

/-- Create a modal component using WidgetM.
    Emits the modal widget and returns control functions. -/
def modal (title : String) (theme : Theme) (content : WidgetM Unit)
    : WidgetM ModalResult := do
  let containerName ← registerComponentW "modal" (isInteractive := false)
  let backdropName ← registerComponentW "modal-backdrop" (isInteractive := false)
  let closeName ← registerComponentW "modal-close"

  -- Pre-run content to get its renders
  let (_, contentRenders) ← runWidgetChildren content

  let isCloseHovered ← useHover closeName
  let closeClicks ← useClick closeName
  let allClicks ← useAllClicks
  let keyEvents ← useKeyboard

  let (openTrigger, fireOpen) ← newTriggerEvent (t := Spider) (a := Bool)

  let isBackdropClick (data : ClickData) : Bool :=
    hitWidget data backdropName && !hitWidget data containerName

  let isEscapePress (keyData : KeyData) : Bool :=
    keyData.event.key == .escape && keyData.event.isPress

  let isOpen ← SpiderM.fixDynM fun isOpenBehavior => do
    let triggerEvents ← Event.mapM (fun open_ => fun _ => open_) openTrigger
    let closeFromButton ← Event.mapM (fun _ => fun _ => false) closeClicks
    let backdropClicks ← Event.filterM isBackdropClick allClicks
    let gatedBackdrop ← Event.gateM isOpenBehavior backdropClicks
    let closeFromBackdrop ← Event.mapM (fun _ => fun _ => false) gatedBackdrop
    let escapeKeys ← Event.filterM isEscapePress keyEvents
    let gatedEscape ← Event.gateM isOpenBehavior escapeKeys
    let closeFromEscape ← Event.mapM (fun _ => fun _ => false) gatedEscape
    let allTransitions ← Event.leftmostM [closeFromButton, closeFromBackdrop, closeFromEscape, triggerEvents]
    foldDyn (fun f s => f s) false allTransitions

  let closeEvents ← Event.filterM (fun open_ => !open_) isOpen.updated
  let onClose ← Event.mapM (fun _ => ()) closeEvents

  emit do
    let open_ ← isOpen.sample
    if open_ then
      let closeHovered ← isCloseHovered.sample
      let contentWidgets ← contentRenders.mapM id
      let contentWidget := column (gap := 0) (style := {}) contentWidgets
      pure (modalVisual containerName backdropName closeName title true theme closeHovered {} contentWidget)
    else
      pure (spacer 0 0)

  pure {
    onClose
    isOpen
    openModal := fireOpen true
    closeModal := fireOpen false
  }

end Demos.ReactiveShowcase.Components
