/-
  Demo Runner - Canopy app linalg ParticleIntegrationComparison tab content.
-/
import Reactive
import Afferent
import Afferent.Canopy
import Afferent.Canopy.Reactive
import Demos.Core.Demo
import Demos.Linalg.ParticleIntegrationComparison
import Trellis

open Reactive Reactive.Host
open Afferent
open Afferent.Arbor
open Afferent.Canopy
open Afferent.Canopy.Reactive
open Trellis

namespace Demos

def particleIntegrationComparisonTabContent (env : DemoEnv) : WidgetM Unit := do
  let animFrame ← useAnimationFrame
  let demoName ← registerComponentW "particle-integration-comparison"

  let keyEvents ← useKeyboard
  let keyUpdates ← Event.mapM (fun data =>
    fun (s : Demos.Linalg.ParticleIntegrationComparisonState) =>
      if data.event.isPress then
        match data.event.key with
        | .char 'r' => Demos.Linalg.particleIntegrationComparisonStateFor s.preset
        | .space => { s with animating := !s.animating }
        | .char '1' => Demos.Linalg.particleIntegrationComparisonStateFor .harmonic
        | .char '2' => Demos.Linalg.particleIntegrationComparisonStateFor .orbit
        | .char '3' => Demos.Linalg.particleIntegrationComparisonStateFor .projectile
        | .char '-' => { s with speed := Linalg.Float.clamp (s.speed - 0.1) 0.2 3.0 }
        | .char '+' => { s with speed := Linalg.Float.clamp (s.speed + 0.1) 0.2 3.0 }
        | .char '=' => { s with speed := Linalg.Float.clamp (s.speed + 0.1) 0.2 3.0 }
        | _ => s
      else s
    ) keyEvents

  let animUpdates ← Event.mapM (fun dt =>
    fun (s : Demos.Linalg.ParticleIntegrationComparisonState) =>
      if s.animating then
        Demos.Linalg.stepParticleIntegrationComparison s dt
      else
        { s with time := s.time + dt }
    ) animFrame

  let allUpdates ← Event.mergeAllListM [keyUpdates, animUpdates]
  let state ← foldDyn (fun f s => f s) Demos.Linalg.particleIntegrationComparisonInitialState allUpdates

  let _ ← dynWidget state fun s => do
    let containerStyle : BoxStyle := {
      flexItem := some (FlexItem.growing 1)
      width := .percent 1.0
      height := .percent 1.0
    }
    emit (pure (namedColumn demoName 0 containerStyle #[
      Demos.Linalg.particleIntegrationComparisonWidget env s
    ]))
  pure ()

end Demos
