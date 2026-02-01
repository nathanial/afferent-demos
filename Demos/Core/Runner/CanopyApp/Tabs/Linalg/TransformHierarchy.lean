/-
  Demo Runner - Canopy app linalg TransformHierarchy tab content.
-/
import Reactive
import Afferent
import Afferent.Canopy
import Afferent.Canopy.Reactive
import Demos.Core.Demo
import Demos.Linalg.TransformHierarchy
import Trellis
import Linalg.Core

open Reactive Reactive.Host
open Afferent
open Afferent.Arbor
open Afferent.Canopy
open Afferent.Canopy.Reactive
open Trellis

namespace Demos

def transformHierarchyTabContent (env : DemoEnv) : WidgetM Unit := do
  let animFrame ← useAnimationFrame
  let demoName ← registerComponentW "transform-hierarchy"

  let keyEvents ← useKeyboard
  let keyUpdates ← Event.mapM (fun data =>
    fun (s : Demos.Linalg.TransformHierarchyState) =>
      if data.event.isPress then
        match data.event.key with
        | .char 'r' => Demos.Linalg.transformHierarchyInitialState
        | .space => { s with animating := !s.animating }
        | .char 'l' => { s with showLocalGizmos := !s.showLocalGizmos }
        | .char 'w' => { s with showWorldGizmos := !s.showWorldGizmos }
        | .char '[' =>
            { s with blend := Linalg.Float.clamp (s.blend - 0.05) 0.0 1.0, animating := false }
        | .char ']' =>
            { s with blend := Linalg.Float.clamp (s.blend + 0.05) 0.0 1.0, animating := false }
        | _ => s
      else s
    ) keyEvents

  let animUpdates ← Event.mapM (fun dt =>
    fun (s : Demos.Linalg.TransformHierarchyState) =>
      if s.animating then
        let newTime := s.time + dt
        let blend := 0.5 + 0.5 * Float.sin newTime
        { s with time := newTime, blend := blend }
      else s
    ) animFrame

  let allUpdates ← Event.mergeAllListM [keyUpdates, animUpdates]
  let state ← foldDyn (fun f s => f s) Demos.Linalg.transformHierarchyInitialState allUpdates

  let _ ← dynWidget state fun s => do
    let containerStyle : BoxStyle := {
      flexItem := some (FlexItem.growing 1)
      width := .percent 1.0
      height := .percent 1.0
    }
    emit (pure (namedColumn demoName 0 containerStyle #[
      Demos.Linalg.transformHierarchyWidget env s
    ]))
  pure ()

end Demos
