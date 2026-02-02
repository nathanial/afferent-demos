/-
  Demo Runner - Canopy app linalg BarycentricCoordinates tab content.
-/
import Reactive
import Afferent
import Afferent.Canopy
import Afferent.Canopy.Reactive
import Demos.Core.Demo
import Demos.Linalg.BarycentricCoordinates
import Trellis

open Reactive Reactive.Host
open Afferent
open Afferent.Arbor
open Afferent.Canopy
open Afferent.Canopy.Reactive
open Trellis

namespace Demos
def barycentricCoordinatesTabContent (env : DemoEnv) : WidgetM Unit := do
  let elapsedTime ← useElapsedTime
  let baryName ← registerComponentW "barycentric-coordinates"

  let keyEvents ← useKeyboard
  let keyUpdates ← Event.mapM (fun data =>
    fun (s : Demos.Linalg.BarycentricCoordinatesState) =>
      if data.event.key == .char 'r' && data.event.isPress then
        Demos.Linalg.barycentricCoordinatesInitialState
      else s
    ) keyEvents

  let clickEvents ← useClickData baryName
  let clickUpdates ← Event.mapM (fun data =>
    if data.click.button != 0 then
      id
    else
      match data.nameMap.get? baryName with
      | some wid =>
          match data.layouts.get wid with
          | some layout =>
              let rect := layout.contentRect
              let localX := data.click.x - rect.x
              let localY := data.click.y - rect.y
              let config := Demos.Linalg.barycentricMathViewConfig env.screenScale
              let view := Afferent.Widget.MathView2D.viewForSize config rect.width rect.height
              let worldPos := Afferent.Widget.MathView2D.screenToWorld view (localX, localY)
              fun (state : Demos.Linalg.BarycentricCoordinatesState) =>
                if Demos.Linalg.nearPoint worldPos state.point 0.4 then
                  { state with dragging := true }
                else
                  state
          | none => id
      | none => id
    ) clickEvents

  let hoverEvents ← useAllHovers
  let hoverUpdates ← Event.mapM (fun data =>
    match data.nameMap.get? baryName with
    | some wid =>
        match data.layouts.get wid with
        | some layout =>
            let rect := layout.contentRect
            let localX := data.x - rect.x
            let localY := data.y - rect.y
            let config := Demos.Linalg.barycentricMathViewConfig env.screenScale
            let view := Afferent.Widget.MathView2D.viewForSize config rect.width rect.height
            let worldPos := Afferent.Widget.MathView2D.screenToWorld view (localX, localY)
            fun (state : Demos.Linalg.BarycentricCoordinatesState) =>
              if state.dragging then
                { state with point := worldPos }
              else
                state
        | none => id
    | none => id
    ) hoverEvents

  let mouseUpEvents ← useAllMouseUp
  let mouseUpUpdates ← Event.mapM (fun _ =>
    fun (s : Demos.Linalg.BarycentricCoordinatesState) => { s with dragging := false }
    ) mouseUpEvents

  let allUpdates ← Event.mergeAllListM [keyUpdates, clickUpdates, hoverUpdates, mouseUpUpdates]
  let state ← foldDyn (fun f s => f s) Demos.Linalg.barycentricCoordinatesInitialState allUpdates

  let _ ← dynWidget state fun s => do
    let containerStyle : BoxStyle := {
      flexItem := some (FlexItem.growing 1)
      width := .percent 1.0
      height := .percent 1.0
    }
    emit (pure (namedColumn baryName 0 containerStyle #[
      Demos.Linalg.barycentricCoordinatesWidget env s
    ]))
  pure ()

end Demos
