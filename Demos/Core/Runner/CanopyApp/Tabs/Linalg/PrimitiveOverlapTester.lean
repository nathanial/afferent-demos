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
  let elapsedTime ← useElapsedTime
  let stateRef ← SpiderM.liftIO (IO.mkRef Demos.Linalg.primitiveOverlapTesterInitialState)
  let overlapName ← registerComponentW "primitive-overlap-tester"

  let keyEvents ← useKeyboard
  let keyAction ← Event.mapM (fun data => do
    if data.event.isPress then
      match data.event.key with
      | .char 'r' =>
          stateRef.set Demos.Linalg.primitiveOverlapTesterInitialState
      | .char '1' =>
          stateRef.modify fun s => { s with mode := .sphereSphere }
      | .char '2' =>
          stateRef.modify fun s => { s with mode := .aabbAabb }
      | .char '3' =>
          stateRef.modify fun s => { s with mode := .sphereAabb }
      | _ => pure ()
    ) keyEvents
  performEvent_ keyAction

  let clickEvents ← useClickData overlapName
  let clickAction ← Event.mapM (fun data => do
    if data.click.button != 0 then
      pure ()
    else
      match data.nameMap.get? overlapName with
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
              if Demos.Linalg.nearPoint worldPos state.centerA 0.6 then
                stateRef.set { state with dragging := .shapeA }
              else if Demos.Linalg.nearPoint worldPos state.centerB 0.6 then
                stateRef.set { state with dragging := .shapeB }
              else
                pure ()
          | none => pure ()
      | none => pure ()
    ) clickEvents
  performEvent_ clickAction

  let hoverEvents ← useAllHovers
  let hoverAction ← Event.mapM (fun data => do
    let state ← stateRef.get
    match state.dragging with
    | .shapeA | .shapeB =>
        match data.nameMap.get? overlapName with
        | some wid =>
            match data.layouts.get wid with
            | some layout =>
                let rect := layout.contentRect
                let localX := data.x - rect.x
                let localY := data.y - rect.y
                let origin := (rect.width / 2, rect.height / 2)
                let scale := 70.0 * env.screenScale
                let worldPos := Demos.Linalg.screenToWorld (localX, localY) origin scale
                match state.dragging with
                | .shapeA => stateRef.set { state with centerA := worldPos }
                | .shapeB => stateRef.set { state with centerB := worldPos }
                | .none => pure ()
            | none => pure ()
        | none => pure ()
    | .none => pure ()
    ) hoverEvents
  performEvent_ hoverAction

  let mouseUpEvents ← useAllMouseUp
  let mouseUpAction ← Event.mapM (fun _ => do
    stateRef.modify fun s => { s with dragging := .none }
    ) mouseUpEvents
  performEvent_ mouseUpAction

  let _ ← dynWidget elapsedTime fun _ => do
    let state ← SpiderM.liftIO stateRef.get
    let containerStyle : BoxStyle := {
      flexItem := some (FlexItem.growing 1)
      width := .percent 1.0
      height := .percent 1.0
    }
    emit (pure (namedColumn overlapName 0 containerStyle #[
      Demos.Linalg.primitiveOverlapTesterWidget env state
    ]))
  pure ()

end Demos
