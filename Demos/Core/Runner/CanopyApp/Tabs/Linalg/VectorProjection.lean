/-
  Demo Runner - Canopy app linalg VectorProjection tab content.
-/
import Reactive
import Afferent
import Afferent.Canopy
import Afferent.Canopy.Reactive
import Demos.Core.Demo
import Demos.Linalg.VectorProjection
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
def vectorProjectionTabContent (env : DemoEnv) : WidgetM Unit := do
  let projName ← registerComponentW "vector-projection"

  let clickEvents ← useClickData projName
  let clickUpdates ← Event.mapM (fun data =>
    if data.click.button != 0 then
      id
    else
      match data.nameMap.get? projName with
      | some wid =>
          match data.layouts.get wid with
          | some layout =>
              let rect := layout.contentRect
              let localX := data.click.x - rect.x
              let localY := data.click.y - rect.y
              let config := Demos.Linalg.vectorProjectionMathViewConfig env.screenScale
              let view := AfferentMath.Widget.MathView2D.viewForSize config rect.width rect.height
              let worldPos := AfferentMath.Widget.MathView2D.screenToWorld view (localX, localY)
              fun (state : Demos.Linalg.VectorProjectionState) =>
                if Demos.Linalg.nearPoint worldPos state.vectorV 0.5 then
                  { state with dragging := some .vectorV }
                else if Demos.Linalg.nearPoint worldPos state.vectorU 0.5 then
                  { state with dragging := some .vectorU }
                else
                  state
          | none => id
      | none => id
    ) clickEvents

  let hoverEvents ← useAllHovers
  let hoverUpdates ← Event.mapM (fun data =>
    match data.nameMap.get? projName with
    | some wid =>
        match data.layouts.get wid with
        | some layout =>
            let rect := layout.contentRect
            let localX := data.x - rect.x
            let localY := data.y - rect.y
            let config := Demos.Linalg.vectorProjectionMathViewConfig env.screenScale
            let view := AfferentMath.Widget.MathView2D.viewForSize config rect.width rect.height
            let worldPos := AfferentMath.Widget.MathView2D.screenToWorld view (localX, localY)
            fun (state : Demos.Linalg.VectorProjectionState) =>
              match state.dragging with
              | some target =>
                  match target with
                  | .vectorV => { state with vectorV := worldPos }
                  | .vectorU => { state with vectorU := worldPos }
              | none => state
        | none => id
    | none => id
    ) hoverEvents

  let mouseUpEvents ← useAllMouseUp
  let mouseUpUpdates ← Event.mapM (fun data =>
    fun (s : Demos.Linalg.VectorProjectionState) =>
      if data.button == 0 then { s with dragging := none } else s
    ) mouseUpEvents

  let keyEvents ← useKeyboard
  let keyUpdates ← Event.mapM (fun data =>
    fun (s : Demos.Linalg.VectorProjectionState) =>
      if data.event.isPress then
        match data.event.key with
        | .char 'p' => { s with showMode := .projection }
        | .char 'r' => { s with showMode := .reflection }
        | .char 'b' => { s with showMode := .both }
        | _ => s
      else s
    ) keyEvents

  let allUpdates ← Event.mergeAllListM [clickUpdates, hoverUpdates, mouseUpUpdates, keyUpdates]
  let state ← foldDyn (fun f s => f s) Demos.Linalg.vectorProjectionInitialState allUpdates

  let _ ← dynWidget state fun s => do
    let containerStyle : BoxStyle := {
      flexItem := some (FlexItem.growing 1)
      width := .percent 1.0
      height := .percent 1.0
    }
    emit (pure (namedColumn projName 0 containerStyle #[
      Demos.Linalg.vectorProjectionWidget env s
    ]))
  pure ()

end Demos
