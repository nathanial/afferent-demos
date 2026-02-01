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
def barycentricCoordinatesTabContent (env : DemoEnv) (elapsedTime : Dynamic Spider Float)
    (stateRef : IO.Ref Demos.Linalg.BarycentricCoordinatesState) : WidgetM Unit := do
  let baryName ← registerComponentW "barycentric-coordinates"

  let keyEvents ← useKeyboard
  let keyAction ← Event.mapM (fun data => do
    if data.event.key == .char 'r' && data.event.isPress then
      stateRef.set Demos.Linalg.barycentricCoordinatesInitialState
    ) keyEvents
  performEvent_ keyAction

  let clickEvents ← useClickData baryName
  let clickAction ← Event.mapM (fun data => do
    if data.click.button != 0 then
      pure ()
    else
      match data.nameMap.get? baryName with
      | some wid =>
          match data.layouts.get wid with
          | some layout =>
              let rect := layout.contentRect
              let localX := data.click.x - rect.x
              let localY := data.click.y - rect.y
              let origin := (rect.width / 2, rect.height / 2)
              let scale := 70.0 * env.screenScale
              let worldPos := Demos.Linalg.screenToWorld (localX, localY) origin scale
              let state ← stateRef.get
              if Demos.Linalg.nearPoint worldPos state.point 0.4 then
                stateRef.set { state with dragging := true }
              else
                pure ()
          | none => pure ()
      | none => pure ()
    ) clickEvents
  performEvent_ clickAction

  let hoverEvents ← useAllHovers
  let hoverAction ← Event.mapM (fun data => do
    let state ← stateRef.get
    if !state.dragging then
      pure ()
    else
      match data.nameMap.get? baryName with
      | some wid =>
          match data.layouts.get wid with
          | some layout =>
              let rect := layout.contentRect
              let localX := data.x - rect.x
              let localY := data.y - rect.y
              let origin := (rect.width / 2, rect.height / 2)
              let scale := 70.0 * env.screenScale
              let worldPos := Demos.Linalg.screenToWorld (localX, localY) origin scale
              stateRef.set { state with point := worldPos }
          | none => pure ()
      | none => pure ()
    ) hoverEvents
  performEvent_ hoverAction

  let mouseUpEvents ← useAllMouseUp
  let mouseUpAction ← Event.mapM (fun _ => do
    stateRef.modify fun s => { s with dragging := false }
    ) mouseUpEvents
  performEvent_ mouseUpAction

  let _ ← dynWidget elapsedTime fun _ => do
    let state ← SpiderM.liftIO stateRef.get
    let containerStyle : BoxStyle := {
      flexItem := some (FlexItem.growing 1)
      width := .percent 1.0
      height := .percent 1.0
    }
    emit (pure (namedColumn baryName 0 containerStyle #[
      Demos.Linalg.barycentricCoordinatesWidget env state
    ]))
  pure ()

end Demos
