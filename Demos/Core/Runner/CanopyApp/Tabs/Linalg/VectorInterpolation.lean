/-
  Demo Runner - Canopy app linalg VectorInterpolation tab content.
-/
import Reactive
import Afferent
import Afferent.Canopy
import Afferent.Canopy.Reactive
import Demos.Core.Demo
import Demos.Linalg.VectorInterpolation
import Trellis
import AfferentMath.Widget.MathView2D

open Reactive Reactive.Host
open Afferent
open Afferent.Arbor
open Afferent.Canopy
open Afferent.Canopy.Reactive
open Trellis
open AfferentMath.Widget

namespace Demos
def vectorInterpolationTabContent (env : DemoEnv) : WidgetM Unit := do
  let animFrame ← useAnimationFrame
  let interpName ← registerComponentW "vector-interpolation"

  let clickEvents ← useClickData interpName
  let clickUpdates ← Event.mapM (fun data =>
    if data.click.button != 0 then
      id
    else
      match data.nameMap.get? interpName with
      | some wid =>
          match data.layouts.get wid with
          | some layout =>
              let rect := layout.contentRect
              let localX := data.click.x - rect.x
              let localY := data.click.y - rect.y
              let config := Demos.Linalg.vectorInterpolationMathViewConfig env.screenScale
              let view := AfferentMath.Widget.MathView2D.viewForSize config rect.width rect.height
              let worldPos := AfferentMath.Widget.MathView2D.screenToWorld view (localX, localY)
              fun (state : Demos.Linalg.VectorInterpolationState) =>
                if Demos.Linalg.nearPoint worldPos state.vectorA 0.5 then
                  { state with dragging := some .vectorA }
                else if Demos.Linalg.nearPoint worldPos state.vectorB 0.5 then
                  { state with dragging := some .vectorB }
                else
                  state
          | none => id
      | none => id
    ) clickEvents

  let hoverEvents ← useAllHovers
  let hoverUpdates ← Event.mapM (fun data =>
    match data.nameMap.get? interpName with
    | some wid =>
        match data.layouts.get wid with
        | some layout =>
            let rect := layout.contentRect
            let localX := data.x - rect.x
            let localY := data.y - rect.y
            let config := Demos.Linalg.vectorInterpolationMathViewConfig env.screenScale
            let view := AfferentMath.Widget.MathView2D.viewForSize config rect.width rect.height
            let worldPos := AfferentMath.Widget.MathView2D.screenToWorld view (localX, localY)
            fun (state : Demos.Linalg.VectorInterpolationState) =>
              match state.dragging with
              | some target =>
                  match target with
                  | .vectorA => { state with vectorA := worldPos }
                  | .vectorB => { state with vectorB := worldPos }
              | none => state
        | none => id
    | none => id
    ) hoverEvents

  let mouseUpEvents ← useAllMouseUp
  let mouseUpUpdates ← Event.mapM (fun data =>
    fun (s : Demos.Linalg.VectorInterpolationState) =>
      if data.button == 0 then { s with dragging := none } else s
    ) mouseUpEvents

  let keyEvents ← useKeyboard
  let keyUpdates ← Event.mapM (fun data =>
    fun (s : Demos.Linalg.VectorInterpolationState) =>
      if data.event.key == .space && data.event.isPress then
        { s with animating := !s.animating }
      else s
    ) keyEvents

  let dtUpdates ← Event.mapM (fun dt =>
    fun (s : Demos.Linalg.VectorInterpolationState) =>
      if s.animating then
        let nextT := s.t + dt * 0.5
        let wrapped := if nextT >= 1.0 then nextT - 1.0 else nextT
        { s with t := wrapped }
      else s
    ) animFrame

  let allUpdates ← Event.mergeAllListM [clickUpdates, hoverUpdates, mouseUpUpdates, keyUpdates, dtUpdates]
  let state ← foldDyn (fun f s => f s) Demos.Linalg.vectorInterpolationInitialState allUpdates

  let _ ← dynWidget state fun s => do
    let containerStyle : BoxStyle := {
      flexItem := some (FlexItem.growing 1)
      width := .percent 1.0
      height := .percent 1.0
    }
    emit (pure (namedColumn interpName 0 containerStyle #[
      Demos.Linalg.vectorInterpolationWidget env s
    ]))
  pure ()

end Demos
