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

open Reactive Reactive.Host
open Afferent
open Afferent.Arbor
open Afferent.Canopy
open Afferent.Canopy.Reactive
open Trellis

namespace Demos
def vectorProjectionTabContent (env : DemoEnv) (elapsedTime : Dynamic Spider Float)
    (stateRef : IO.Ref Demos.Linalg.VectorProjectionState) : WidgetM Unit := do
  let projName ← registerComponentW "vector-projection"

  let clickEvents ← useClickData projName
  let clickAction ← Event.mapM (fun data => do
    if data.click.button != 0 then
      pure ()
    else
      match data.nameMap.get? projName with
      | some wid =>
          match data.layouts.get wid with
          | some layout =>
              let rect := layout.contentRect
              let localX := data.click.x - rect.x
              let localY := data.click.y - rect.y
              let origin := (rect.width / 2, rect.height / 2)
              let scale := 50.0 * env.screenScale
              let worldPos := Demos.Linalg.screenToWorld (localX, localY) origin scale
              let state ← stateRef.get
              if Demos.Linalg.nearPoint worldPos state.vectorV 0.5 then
                stateRef.set { state with dragging := some .vectorV }
              else if Demos.Linalg.nearPoint worldPos state.vectorU 0.5 then
                stateRef.set { state with dragging := some .vectorU }
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
    | some target =>
        match data.nameMap.get? projName with
        | some wid =>
            match data.layouts.get wid with
            | some layout =>
                let rect := layout.contentRect
                let localX := data.x - rect.x
                let localY := data.y - rect.y
                let origin := (rect.width / 2, rect.height / 2)
                let scale := 50.0 * env.screenScale
                let worldPos := Demos.Linalg.screenToWorld (localX, localY) origin scale
                let next := match target with
                  | .vectorV => { state with vectorV := worldPos }
                  | .vectorU => { state with vectorU := worldPos }
                stateRef.set next
            | none => pure ()
        | none => pure ()
    | none => pure ()
    ) hoverEvents
  performEvent_ hoverAction

  let mouseUpEvents ← useAllMouseUp
  let mouseUpAction ← Event.mapM (fun data => do
    if data.button == 0 then
      stateRef.modify fun s => { s with dragging := none }
    ) mouseUpEvents
  performEvent_ mouseUpAction

  let keyEvents ← useKeyboard
  let keyAction ← Event.mapM (fun data => do
    if data.event.isPress then
      match data.event.key with
      | .char 'p' =>
          stateRef.modify fun s => { s with showMode := .projection }
      | .char 'r' =>
          stateRef.modify fun s => { s with showMode := .reflection }
      | .char 'b' =>
          stateRef.modify fun s => { s with showMode := .both }
      | _ => pure ()
    ) keyEvents
  performEvent_ keyAction

  let _ ← dynWidget elapsedTime fun _ => do
    let state ← SpiderM.liftIO stateRef.get
    let containerStyle : BoxStyle := {
      flexItem := some (FlexItem.growing 1)
      width := .percent 1.0
      height := .percent 1.0
    }
    emit (pure (namedColumn projName 0 containerStyle #[
      Demos.Linalg.vectorProjectionWidget env state
    ]))
  pure ()

end Demos
