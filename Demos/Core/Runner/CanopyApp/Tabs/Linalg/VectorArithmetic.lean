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
def vectorArithmeticTabContent (env : DemoEnv) (elapsedTime : Dynamic Spider Float)
    (stateRef : IO.Ref Demos.Linalg.VectorArithmeticState) : WidgetM Unit := do
  let arithName ← registerComponentW "vector-arithmetic"

  let clickEvents ← useClickData arithName
  let clickAction ← Event.mapM (fun data => do
    if data.click.button != 0 then
      pure ()
    else
      match data.nameMap.get? arithName with
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
              if Demos.Linalg.nearPoint worldPos state.vectorA 0.5 then
                stateRef.set { state with dragging := some .vectorA }
              else if Demos.Linalg.nearPoint worldPos state.vectorB 0.5 then
                stateRef.set { state with dragging := some .vectorB }
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
        match data.nameMap.get? arithName with
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
                  | .vectorA => { state with vectorA := worldPos }
                  | .vectorB => { state with vectorB := worldPos }
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
      | .char '1' =>
          stateRef.modify fun s => { s with operation := .add }
      | .char '2' =>
          stateRef.modify fun s => { s with operation := .sub }
      | .char '3' =>
          stateRef.modify fun s => { s with operation := .scale }
      | .char '=' | .char '+' =>
          stateRef.modify fun s => { s with scaleFactor := s.scaleFactor + 0.1 }
      | .char '-' =>
          stateRef.modify fun s =>
            let newScale := if s.scaleFactor > 0.2 then s.scaleFactor - 0.1 else 0.1
            { s with scaleFactor := newScale }
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
    emit (pure (namedColumn arithName 0 containerStyle #[
      Demos.Linalg.vectorArithmeticWidget env state
    ]))
  pure ()

end Demos
