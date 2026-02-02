/-
  Demo Runner - Canopy app linalg ConvexDecomposition tab content.
-/
import Reactive
import Afferent
import Afferent.Canopy
import Afferent.Canopy.Reactive
import Demos.Core.Demo
import Demos.Linalg.ConvexDecomposition
import Trellis

open Reactive Reactive.Host
open Afferent
open Afferent.Arbor
open Afferent.Canopy
open Afferent.Canopy.Reactive
open Trellis

namespace Demos

private def updateMaxTriangles (delta : Int) (state : Demos.Linalg.ConvexDecompositionState)
    : Demos.Linalg.ConvexDecompositionState :=
  let current := Int.ofNat state.config.maxTrianglesPerPart
  let raw := current + delta
  let clamped :=
    if raw < 0 then 0
    else if raw > 128 then 128
    else raw
  { state with
    config := { state.config with maxTrianglesPerPart := clamped.toNat }
  }

private def updateMaxDepth (delta : Int) (state : Demos.Linalg.ConvexDecompositionState)
    : Demos.Linalg.ConvexDecompositionState :=
  let current := Int.ofNat state.config.maxDepth
  let raw := current + delta
  let clamped :=
    if raw < 1 then 1
    else if raw > 12 then 12
    else raw
  { state with
    config := { state.config with maxDepth := clamped.toNat }
  }

private def updateMinSplit (scale : Float) (state : Demos.Linalg.ConvexDecompositionState)
    : Demos.Linalg.ConvexDecompositionState :=
  let minE := 0.02
  let maxE := 1.5
  let newVal := Linalg.Float.clamp (state.config.minSplitExtent * scale) minE maxE
  { state with
    config := { state.config with minSplitExtent := newVal }
  }

private def updateRotation (dx dy : Float) (state : Demos.Linalg.ConvexDecompositionState)
    : Demos.Linalg.ConvexDecompositionState :=
  let newYaw := state.cameraYaw + dx
  let newPitch := Linalg.Float.clamp (state.cameraPitch + dy) (-1.2) 1.2
  { state with cameraYaw := newYaw, cameraPitch := newPitch }

def convexDecompositionTabContent (env : DemoEnv) : WidgetM Unit := do
  let demoName ← registerComponentW "convex-decomposition"

  let keyEvents ← useKeyboard
  let keyUpdates ← Event.mapM (fun data =>
    fun (s : Demos.Linalg.ConvexDecompositionState) =>
      if data.event.isPress then
        match data.event.key with
        | .char 'r' => Demos.Linalg.convexDecompositionInitialState
        | .char 'h' => { s with showHulls := !s.showHulls }
        | .char 'b' => { s with showBounds := !s.showBounds }
        | .char 'm' => { s with showMesh := !s.showMesh }
        | .char 'g' => { s with showGrid := !s.showGrid }
        | .char 'x' => { s with showAxes := !s.showAxes }
        | .char 'c' => { s with meshPreset := Demos.Linalg.nextConvexMeshPreset s.meshPreset }
        | .char '[' => updateMaxTriangles (-4) s
        | .char ']' => updateMaxTriangles 4 s
        | .char ',' => updateMaxDepth (-1) s
        | .char '.' => updateMaxDepth 1 s
        | .char '-' => updateMinSplit 0.85 s
        | .char '+' => updateMinSplit 1.18 s
        | .char '=' => updateMinSplit 1.18 s
        | _ => s
      else s
    ) keyEvents

  let clickEvents ← useClickData demoName
  let clickUpdates ← Event.mapM (fun data =>
    if data.click.button != 0 then
      id
    else
      fun (s : Demos.Linalg.ConvexDecompositionState) =>
        { s with dragging := true, lastMouseX := data.click.x, lastMouseY := data.click.y }
    ) clickEvents

  let hoverEvents ← useAllHovers
  let hoverUpdates ← Event.mapM (fun data =>
    fun (state : Demos.Linalg.ConvexDecompositionState) =>
      if !state.dragging then
        state
      else
        let dx := (data.x - state.lastMouseX) * 0.005
        let dy := (data.y - state.lastMouseY) * 0.005
        let updated := updateRotation dx dy state
        { updated with lastMouseX := data.x, lastMouseY := data.y }
    ) hoverEvents

  let mouseUpEvents ← useAllMouseUp
  let mouseUpUpdates ← Event.mapM (fun data =>
    fun (s : Demos.Linalg.ConvexDecompositionState) =>
      if data.button == 0 then
        { s with dragging := false }
      else s
    ) mouseUpEvents

  let allUpdates ← Event.mergeAllListM [keyUpdates, clickUpdates, hoverUpdates, mouseUpUpdates]
  let state ← foldDyn (fun f s => f s) Demos.Linalg.convexDecompositionInitialState allUpdates

  let _ ← dynWidget state fun s => do
    let containerStyle : BoxStyle := {
      flexItem := some (FlexItem.growing 1)
      width := .percent 1.0
      height := .percent 1.0
    }
    emit (pure (namedColumn demoName 0 containerStyle #[
      Demos.Linalg.convexDecompositionWidget env s
    ]))
  pure ()

end Demos
