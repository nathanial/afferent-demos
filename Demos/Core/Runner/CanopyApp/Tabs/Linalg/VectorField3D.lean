/-
  Demo Runner - Canopy app linalg VectorField3D tab content.
-/
import Reactive
import Afferent
import Afferent.Canopy
import Afferent.Canopy.Reactive
import Demos.Core.Demo
import Demos.Linalg.VectorField3D
import Trellis

open Reactive Reactive.Host
open Afferent
open Afferent.Arbor
open Afferent.Canopy
open Afferent.Canopy.Reactive
open Trellis

namespace Demos

def vectorField3DTabContent (env : DemoEnv) : WidgetM Unit := do
  let fieldName ← registerComponentW "vector-field-3d"

  let keyEvents ← useKeyboard
  let keyUpdates ← Event.mapM (fun data =>
    fun (s : Demos.Linalg.VectorField3DState) =>
      if data.event.isPress then
        match data.event.key with
        | .char '1' => { s with fieldType := .radial }
        | .char '2' => { s with fieldType := .swirl }
        | .char '3' => { s with fieldType := .saddle }
        | .char '4' => { s with fieldType := .helix }
        | .char '=' | .char '+' =>
            { s with samplesXY := Nat.min 14 (s.samplesXY + 2) }
        | .char '-' =>
            { s with samplesXY := Nat.max 4 (s.samplesXY - 2) }
        | .char '[' =>
            { s with samplesZ := Nat.max 2 (s.samplesZ - 1) }
        | .char ']' =>
            { s with samplesZ := Nat.min 10 (s.samplesZ + 1) }
        | .char 'm' => { s with showMagnitude := !s.showMagnitude }
        | .char 'v' => { s with scaleByMagnitude := !s.scaleByMagnitude }
        | _ => s
      else s
    ) keyEvents

  let allUpdates ← Event.mergeAllListM [keyUpdates]
  let state ← foldDyn (fun f s => f s) Demos.Linalg.vectorField3DInitialState allUpdates

  let _ ← dynWidget state fun s => do
    let containerStyle : BoxStyle := {
      flexItem := some (FlexItem.growing 1)
      width := .percent 1.0
      height := .percent 1.0
    }
    emit (pure (namedColumn fieldName 0 containerStyle #[
      Demos.Linalg.vectorField3DWidget env s
    ]))
  pure ()

end Demos
