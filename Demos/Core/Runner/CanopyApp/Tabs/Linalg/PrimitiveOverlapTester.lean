/-
  Demo Runner - Canopy app linalg PrimitiveOverlapTester tab content.
-/
import Reactive
import Afferent
import Afferent.Canopy
import Afferent.Canopy.Reactive
import Demos.Core.Demo
import Demos.Linalg.PrimitiveOverlapTester
import Trellis

open Reactive Reactive.Host
open Afferent
open Afferent.Arbor
open Afferent.Canopy
open Afferent.Canopy.Reactive
open Trellis

namespace Demos
def primitiveOverlapTesterTabContent (env : DemoEnv) : WidgetM Unit := do
  let overlapName ← registerComponentW "primitive-overlap-tester"

  let keyEvents ← useKeyboard
  let keyUpdates ← Event.mapM (fun data =>
    fun (s : Demos.Linalg.PrimitiveOverlapTesterState) =>
      if data.event.isPress then
        match data.event.key with
        | .char 'r' => Demos.Linalg.primitiveOverlapTesterInitialState
        | .char '1' => { s with mode := .sphereSphere }
        | .char '2' => { s with mode := .aabbAabb }
        | .char '3' => { s with mode := .sphereAabb }
        | _ => s
      else s
    ) keyEvents

  let clickEvents ← useClickData overlapName
  let clickUpdates ← Event.mapM (fun data =>
    if data.click.button != 0 then
      id
    else
      match data.nameMap.get? overlapName with
      | some wid =>
          match data.layouts.get wid with
          | some layout =>
              let rect := layout.contentRect
              let localX := data.click.x - rect.x
              let localY := data.click.y - rect.y
              let config := Demos.Linalg.primitiveOverlapMathViewConfig env.screenScale
              let view := Afferent.Widget.MathView2D.viewForSize config rect.width rect.height
              let worldPos := Afferent.Widget.MathView2D.screenToWorld view (localX, localY)
              fun (state : Demos.Linalg.PrimitiveOverlapTesterState) =>
                if Demos.Linalg.nearPoint worldPos state.centerA 0.6 then
                  { state with dragging := .shapeA }
                else if Demos.Linalg.nearPoint worldPos state.centerB 0.6 then
                  { state with dragging := .shapeB }
                else
                  state
          | none => id
      | none => id
    ) clickEvents

  let hoverEvents ← useAllHovers
  let hoverUpdates ← Event.mapM (fun data =>
    match data.nameMap.get? overlapName with
    | some wid =>
        match data.layouts.get wid with
        | some layout =>
            let rect := layout.contentRect
            let localX := data.x - rect.x
            let localY := data.y - rect.y
            let config := Demos.Linalg.primitiveOverlapMathViewConfig env.screenScale
            let view := Afferent.Widget.MathView2D.viewForSize config rect.width rect.height
            let worldPos := Afferent.Widget.MathView2D.screenToWorld view (localX, localY)
            fun (state : Demos.Linalg.PrimitiveOverlapTesterState) =>
              match state.dragging with
              | .shapeA => { state with centerA := worldPos }
              | .shapeB => { state with centerB := worldPos }
              | .none => state
        | none => id
    | none => id
    ) hoverEvents

  let mouseUpEvents ← useAllMouseUp
  let mouseUpUpdates ← Event.mapM (fun _ =>
    fun (s : Demos.Linalg.PrimitiveOverlapTesterState) => { s with dragging := .none }
    ) mouseUpEvents

  let allUpdates ← Event.mergeAllListM [keyUpdates, clickUpdates, hoverUpdates, mouseUpUpdates]
  let state ← foldDyn (fun f s => f s) Demos.Linalg.primitiveOverlapTesterInitialState allUpdates

  let _ ← dynWidget state fun s => do
    let containerStyle : BoxStyle := {
      flexItem := some (FlexItem.growing 1)
      width := .percent 1.0
      height := .percent 1.0
    }
    emit (pure (namedColumn overlapName 0 containerStyle #[
      Demos.Linalg.primitiveOverlapTesterWidget env s
    ]))
  pure ()

end Demos
