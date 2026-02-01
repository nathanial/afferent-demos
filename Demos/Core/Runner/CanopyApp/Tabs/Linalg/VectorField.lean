/-
  Demo Runner - Canopy app linalg VectorField tab content.
-/
import Reactive
import Afferent
import Afferent.Canopy
import Afferent.Canopy.Reactive
import Demos.Core.Demo
import Demos.Linalg.VectorField
import Trellis

open Reactive Reactive.Host
open Afferent
open Afferent.Arbor
open Afferent.Canopy
open Afferent.Canopy.Reactive
open Trellis

namespace Demos
def vectorFieldTabContent (env : DemoEnv) (elapsedTime : Dynamic Spider Float)
    (stateRef : IO.Ref Demos.Linalg.VectorFieldState) : WidgetM Unit := do
  let fieldName ← registerComponentW "vector-field"
  let keyEvents ← useKeyboard
  let keyAction ← Event.mapM (fun data => do
    if data.event.isPress then
      match data.event.key with
      | .char '1' =>
          stateRef.modify fun s => { s with fieldType := .radial }
      | .char '2' =>
          stateRef.modify fun s => { s with fieldType := .rotational }
      | .char '3' =>
          stateRef.modify fun s => { s with fieldType := .gradient }
      | .char '4' =>
          stateRef.modify fun s => { s with fieldType := .saddle }
      | .char '=' | .char '+' =>
          stateRef.modify fun s =>
            { s with gridResolution := Nat.min 24 (s.gridResolution + 2) }
      | .char '-' =>
          stateRef.modify fun s =>
            { s with gridResolution := Nat.max 4 (s.gridResolution - 2) }
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
    emit (pure (namedColumn fieldName 0 containerStyle #[
      Demos.Linalg.vectorFieldWidget env state
    ]))
  pure ()

end Demos
