/-
  Toast Component - Self-contained FRP toast notifications with auto-dismiss.
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

/-- A single toast notification with its metadata. -/
structure ToastItem where
  id : Nat
  message : String
  variant : ToastVariant
  createdAt : Float  -- Time when created (for auto-dismiss)
  duration : Float   -- How long to show (in seconds)
deriving Repr, BEq, Inhabited

/-- State for the toast manager. -/
structure ToastState where
  toasts : Array ToastItem
  nextId : Nat
  currentTime : Float
deriving Repr, BEq, Inhabited

/-- Toast manager result - provides functions to show toasts and current state. -/
structure ToastManagerResult where
  /-- Show an info toast. -/
  showInfo : String → IO Unit
  /-- Show a success toast. -/
  showSuccess : String → IO Unit
  /-- Show a warning toast. -/
  showWarning : String → IO Unit
  /-- Show an error toast. -/
  showError : String → IO Unit
  /-- Current list of active toasts. -/
  toasts : Dynamic Spider (Array ToastItem)

/-- Create a toast manager component using WidgetM.
    Manages a stack of toast notifications with auto-dismiss.
    Emits toast visuals and provides functions to show new toasts. -/
def toastManager (theme : Theme) (defaultDuration : Float := 3.0)
    : WidgetM ToastManagerResult := do
  let containerName ← registerComponentW "toast-container" (isInteractive := false)

  -- Animation frames for timing
  let animFrame ← useAnimationFrame

  -- Trigger events for showing toasts (message, variant)
  let (showTrigger, fireShow) ← newTriggerEvent (t := Spider) (a := String × ToastVariant)

  -- Combined state: toasts, next ID, and current time
  let state ← foldDyn
    (fun (event : Float ⊕ (String × ToastVariant)) (s : ToastState) =>
      match event with
      | .inl dt =>
        -- Animation frame: update time and remove expired toasts
        let newTime := s.currentTime + dt
        let activeToasts := s.toasts.filter fun t => newTime - t.createdAt < t.duration
        { s with currentTime := newTime, toasts := activeToasts }
      | .inr (msg, variant) =>
        -- New toast: add to list
        let item : ToastItem := {
          id := s.nextId
          message := msg
          variant := variant
          createdAt := s.currentTime
          duration := defaultDuration
        }
        { s with toasts := s.toasts.push item, nextId := s.nextId + 1 })
    { toasts := #[], nextId := 0, currentTime := 0.0 }
    (← Event.mergeM (← Event.mapM Sum.inl animFrame) (← Event.mapM Sum.inr showTrigger))

  let toasts ← Dynamic.mapM (·.toasts) state

  -- Helper functions to show toasts
  let showToast (msg : String) (variant : ToastVariant) : IO Unit :=
    fireShow (msg, variant)

  -- Emit toast visuals
  emit do
    let currentToasts ← toasts.sample
    if currentToasts.isEmpty then
      pure (spacer 0 0)
    else
      pure do
        let mut toastWidgets : Array Widget := #[]
        for toast in currentToasts do
          let toastName := s!"toast-{toast.id}"
          let widget ← toastVisual toastName toast.message toast.variant theme
          toastWidgets := toastWidgets.push widget
        toastContainerVisual containerName toastWidgets

  pure {
    showInfo := fun msg => showToast msg .info
    showSuccess := fun msg => showToast msg .success
    showWarning := fun msg => showToast msg .warning
    showError := fun msg => showToast msg .error
    toasts
  }

end Demos.ReactiveShowcase.Components
