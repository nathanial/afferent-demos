/-
  ProgressBar Component - Self-contained FRP progress bar with determinate and indeterminate modes.
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

/-- Float modulo for animation cycling. -/
private def floatMod (x y : Float) : Float :=
  x - y * (x / y).floor

/-- ProgressBar result - just state access since progress bars are typically display-only. -/
structure ProgressBarResult where
  value : Dynamic Spider Float

/-- Create a determinate progress bar component using WidgetM.
    Displays a static progress value. -/
def progressBar (theme : Theme) (initialValue : Float := 0.0)
    (variant : ProgressVariant := .primary)
    (label : Option String := none) (showPercentage : Bool := false)
    : WidgetM ProgressBarResult := do
  let name ← registerComponentW "progress-bar" (isInteractive := false)

  -- Create a constant dynamic
  let value ← Dynamic.pureM initialValue

  emit do
    pure (progressBarVisual name initialValue variant theme label showPercentage)

  pure { value }

/-- IndeterminateProgressBar result - includes animation control. -/
structure IndeterminateProgressBarResult where
  animationProgress : Dynamic Spider Float

/-- Create an indeterminate progress bar component using WidgetM.
    Emits an animated progress bar that cycles continuously. -/
def indeterminateProgressBar (theme : Theme)
    (variant : ProgressVariant := .primary)
    (label : Option String := none)
    : WidgetM IndeterminateProgressBarResult := do
  let name ← registerComponentW "progress-bar-indeterminate" (isInteractive := false)

  -- Subscribe to animation frames for continuous animation
  let animFrame ← useAnimationFrame

  -- Accumulate time for animation (cycle every 2 seconds)
  let cycleDuration : Float := 2.0
  let animationTime ← foldDyn (fun dt acc => floatMod (acc + dt) cycleDuration) 0.0 animFrame
  let animationProgress ← Dynamic.mapM (· / cycleDuration) animationTime

  emit do
    let progress ← animationProgress.sample
    pure (progressBarIndeterminateVisual name progress variant theme label)

  pure { animationProgress }

/-- Create a progress bar that updates based on an external event stream.
    Useful for showing download progress, file processing, etc. -/
def progressBarWithEvents (theme : Theme) (valueUpdates : Event Spider Float)
    (initialValue : Float := 0.0)
    (variant : ProgressVariant := .primary)
    (label : Option String := none) (showPercentage : Bool := true)
    : WidgetM ProgressBarResult := do
  let name ← registerComponentW "progress-bar" (isInteractive := false)

  let value ← holdDyn initialValue valueUpdates

  emit do
    let v ← value.sample
    pure (progressBarVisual name v variant theme label showPercentage)

  pure { value }

end Demos.ReactiveShowcase.Components
