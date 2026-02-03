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
open Afferent.Widget
open Afferent.Canopy
open Afferent.Canopy.Reactive
open Trellis

namespace Demos

def vectorField3DTabContent (env : DemoEnv) : WidgetM Unit := do
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
    column' (gap := 0) (style := containerStyle) do
      let viewConfig := Demos.Linalg.vectorField3DMathViewConfig env.screenScale
      let _ ← mathView3DInteractive viewConfig {} env.fontSmall (fun view => do
        let sampling : VectorField.Sampling3D := {
          samplesX := s.samplesXY
          samplesY := s.samplesXY
          samplesZ := s.samplesZ
          extent := 3.0
          computeMax := true
        }
        let arrows : VectorField.ArrowStyle := {
          lineWidth := 1.2 * env.screenScale
          headLength := 6.0 * env.screenScale
          headAngle := 0.5
          scale := s.arrowScale
          scaleMode := .cell
          scaleByMagnitude := s.scaleByMagnitude
        }
        let colorScale := VectorField.ColorScale.viridis
        let coloring : VectorField.Coloring := {
          mode := if s.showMagnitude then
            .magnitude colorScale
          else
            .fixed Color.cyan
        }
        let maxMag ← VectorField.drawField3D view (Demos.Linalg.computeFieldVector3D s.fieldType)
          sampling arrows coloring
        Demos.Linalg.renderVectorField3DOverlay s view maxMag env.screenScale env.fontMedium env.fontSmall
      )
      pure ()
  pure ()

end Demos
