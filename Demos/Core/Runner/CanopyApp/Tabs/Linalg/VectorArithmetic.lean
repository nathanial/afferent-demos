/-
  Demo Runner - Canopy app linalg VectorArithmetic tab content.
-/
import Reactive
import Afferent
import Afferent.Canopy
import Afferent.Canopy.Reactive
import Demos.Core.Demo
import Demos.Linalg.VectorArithmetic
import Trellis

open Reactive Reactive.Host
open Afferent
open Afferent.Arbor
open Afferent.Canopy
open Afferent.Canopy.Reactive
open Trellis

namespace Demos
def vectorArithmeticTabContent (env : DemoEnv) : WidgetM Unit := do
  let arithName ← registerComponentW "vector-arithmetic"

  let clickEvents ← useClickData arithName
  let clickUpdates ← Event.mapM (fun data =>
    if data.click.button != 0 then
      id
    else
      match data.nameMap.get? arithName with
      | some wid =>
          match data.layouts.get wid with
          | some layout =>
              let rect := layout.contentRect
              let localX := data.click.x - rect.x
              let localY := data.click.y - rect.y
              let config := Demos.Linalg.vectorArithmeticMathViewConfig env.screenScale
              let view := Afferent.Widget.MathView2D.viewForSize config rect.width rect.height
              let worldPos := Afferent.Widget.MathView2D.screenToWorld view (localX, localY)
              fun (state : Demos.Linalg.VectorArithmeticState) =>
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
    match data.nameMap.get? arithName with
    | some wid =>
        match data.layouts.get wid with
        | some layout =>
            let rect := layout.contentRect
            let localX := data.x - rect.x
            let localY := data.y - rect.y
            let config := Demos.Linalg.vectorArithmeticMathViewConfig env.screenScale
            let view := Afferent.Widget.MathView2D.viewForSize config rect.width rect.height
            let worldPos := Afferent.Widget.MathView2D.screenToWorld view (localX, localY)
            fun (state : Demos.Linalg.VectorArithmeticState) =>
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
    fun (s : Demos.Linalg.VectorArithmeticState) =>
      if data.button == 0 then { s with dragging := none } else s
    ) mouseUpEvents

  let keyEvents ← useKeyboard
  let keyUpdates ← Event.mapM (fun data =>
    fun (s : Demos.Linalg.VectorArithmeticState) =>
      if data.event.isPress then
        match data.event.key with
        | .char '1' => { s with operation := .add }
        | .char '2' => { s with operation := .sub }
        | .char '3' => { s with operation := .scale }
        | .char '=' | .char '+' => { s with scaleFactor := s.scaleFactor + 0.1 }
        | .char '-' =>
            let newScale := if s.scaleFactor > 0.2 then s.scaleFactor - 0.1 else 0.1
            { s with scaleFactor := newScale }
        | _ => s
      else s
    ) keyEvents

  let allUpdates ← Event.mergeAllListM [clickUpdates, hoverUpdates, mouseUpUpdates, keyUpdates]
  let state ← foldDyn (fun f s => f s) Demos.Linalg.vectorArithmeticInitialState allUpdates

  let _ ← dynWidget state fun s => do
    let containerStyle : BoxStyle := {
      flexItem := some (FlexItem.growing 1)
      width := .percent 1.0
      height := .percent 1.0
    }
    emit (pure (namedColumn arithName 0 containerStyle #[
      Demos.Linalg.vectorArithmeticWidget env s
    ]))
  pure ()

end Demos
