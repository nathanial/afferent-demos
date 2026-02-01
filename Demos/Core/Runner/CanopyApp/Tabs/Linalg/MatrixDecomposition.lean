/-
  Demo Runner - Canopy app linalg MatrixDecomposition tab content.
-/
import Reactive
import Afferent
import Afferent.Canopy
import Afferent.Canopy.Reactive
import Demos.Core.Demo
import Demos.Linalg.MatrixDecomposition
import Trellis

open Reactive Reactive.Host
open Afferent
open Afferent.Arbor
open Afferent.Canopy
open Afferent.Canopy.Reactive
open Trellis

namespace Demos
def matrixDecompositionTabContent (env : DemoEnv) : WidgetM Unit := do
  let elapsedTime ← useElapsedTime
  let stateRef ← SpiderM.liftIO (IO.mkRef Demos.Linalg.matrixDecompositionInitialState)
  let decompName ← registerComponentW "matrix-decomposition"
  let keyEvents ← useKeyboard
  let keyAction ← Event.mapM (fun data => do
    if data.event.isPress then
      match data.event.key with
      | .char '1' =>
          stateRef.modify fun s =>
            if 0 < Demos.Linalg.decompositionPresets.size then
              let (_, m) := Demos.Linalg.decompositionPresets.getD 0 ("", Linalg.Mat2.identity)
              { s with matrix := m, decomp := Demos.Linalg.decomposeMatrix2D m, presetIndex := 0 }
            else s
      | .char '2' =>
          stateRef.modify fun s =>
            if 1 < Demos.Linalg.decompositionPresets.size then
              let (_, m) := Demos.Linalg.decompositionPresets.getD 1 ("", Linalg.Mat2.identity)
              { s with matrix := m, decomp := Demos.Linalg.decomposeMatrix2D m, presetIndex := 1 }
            else s
      | .char '3' =>
          stateRef.modify fun s =>
            if 2 < Demos.Linalg.decompositionPresets.size then
              let (_, m) := Demos.Linalg.decompositionPresets.getD 2 ("", Linalg.Mat2.identity)
              { s with matrix := m, decomp := Demos.Linalg.decomposeMatrix2D m, presetIndex := 2 }
            else s
      | .char '4' =>
          stateRef.modify fun s =>
            if 3 < Demos.Linalg.decompositionPresets.size then
              let (_, m) := Demos.Linalg.decompositionPresets.getD 3 ("", Linalg.Mat2.identity)
              { s with matrix := m, decomp := Demos.Linalg.decomposeMatrix2D m, presetIndex := 3 }
            else s
      | .char '5' =>
          stateRef.modify fun s =>
            if 4 < Demos.Linalg.decompositionPresets.size then
              let (_, m) := Demos.Linalg.decompositionPresets.getD 4 ("", Linalg.Mat2.identity)
              { s with matrix := m, decomp := Demos.Linalg.decomposeMatrix2D m, presetIndex := 4 }
            else s
      | .char '6' =>
          stateRef.modify fun s =>
            if 5 < Demos.Linalg.decompositionPresets.size then
              let (_, m) := Demos.Linalg.decompositionPresets.getD 5 ("", Linalg.Mat2.identity)
              { s with matrix := m, decomp := Demos.Linalg.decomposeMatrix2D m, presetIndex := 5 }
            else s
      | .tab =>
          stateRef.modify fun s =>
            { s with currentStep := match s.currentStep with
              | .original => .afterRotation1
              | .afterRotation1 => .afterScale
              | .afterScale => .afterRotation2
              | .afterRotation2 => .original
            }
      | .char 'c' =>
          stateRef.modify fun s => { s with showComponents := !s.showComponents }
      | _ => pure ()
    ) keyEvents
  performEvent_ keyAction

  let _ ← dynWidget elapsedTime fun _ => do
    let state ← SpiderM.liftIO stateRef.get
    let containerStyle : BoxStyle := {
      flexItem := some (FlexItem.growing 1)
      width := .percent 1.0
      height := .percent 1.0
    }
    emit (pure (namedColumn decompName 0 containerStyle #[
      Demos.Linalg.matrixDecompositionWidget env state
    ]))
  pure ()

end Demos
