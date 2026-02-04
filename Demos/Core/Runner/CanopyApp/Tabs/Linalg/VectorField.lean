/-
  Demo Runner - Canopy app linalg VectorField tab content.
-/
import Reactive
import Afferent
import Afferent.Canopy
import Afferent.Canopy.Reactive
import AfferentMath.Canopy.Widget.MathView
import Demos.Core.Demo
import Demos.Linalg.VectorField
import Trellis
import AfferentMath.Widget.VectorField

open Reactive Reactive.Host
open Afferent
open Afferent.Arbor
open Afferent.Canopy
open Afferent.Canopy.Reactive
open Trellis
open AfferentMath.Widget
open AfferentMath.Canopy

namespace Demos
def vectorFieldTabContent (env : DemoEnv) : WidgetM Unit := do
  let keyEvents ← useKeyboard
  let keyUpdates ← Event.mapM (fun data =>
    fun (s : Demos.Linalg.VectorFieldState) =>
      if data.event.isPress then
        match data.event.key with
        | .char '1' => { s with fieldType := .radial }
        | .char '2' => { s with fieldType := .rotational }
        | .char '3' => { s with fieldType := .gradient }
        | .char '4' => { s with fieldType := .saddle }
        | .char '=' | .char '+' =>
            { s with gridResolution := Nat.min 24 (s.gridResolution + 2) }
        | .char '-' =>
            { s with gridResolution := Nat.max 4 (s.gridResolution - 2) }
        | _ => s
      else s
    ) keyEvents

  let allUpdates ← Event.mergeAllListM [keyUpdates]
  let state ← foldDyn (fun f s => f s) Demos.Linalg.vectorFieldInitialState allUpdates

  let _ ← dynWidget state fun s => do
    let containerStyle : BoxStyle := {
      flexItem := some (FlexItem.growing 1)
      width := .percent 1.0
      height := .percent 1.0
    }
    column' (gap := 0) (style := containerStyle) do
      let viewConfig := Demos.Linalg.vectorFieldMathViewConfig env.screenScale
      let _ ← mathView2DInteractive viewConfig {} env.fontSmall (fun view => do
        Demos.Linalg.renderVectorField s view env.screenScale env.fontMedium env.fontSmall
      )
      pure ()
  pure ()

end Demos
