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

open Reactive Reactive.Host
open Afferent
open Afferent.Arbor
open Afferent.Canopy
open Afferent.Canopy.Reactive
open Trellis

namespace Demos
def vectorInterpolationTabContent (env : DemoEnv) (elapsedTime : Dynamic Spider Float)
    (stateRef : IO.Ref Demos.Linalg.VectorInterpolationState)
    (lastTimeRef : IO.Ref Float) : WidgetM Unit := do
  let interpName ← registerComponentW "vector-interpolation"

  let clickEvents ← useClickData interpName
  let clickAction ← Event.mapM (fun data => do
    if data.click.button != 0 then
      pure ()
    else
      match data.nameMap.get? interpName with
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
        match data.nameMap.get? interpName with
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
    if data.event.key == .space && data.event.isPress then
      stateRef.modify fun s => { s with animating := !s.animating }
    ) keyEvents
  performEvent_ keyAction

  let _ ← dynWidget elapsedTime fun t => do
    let state ← SpiderM.liftIO do
      let lastT ← lastTimeRef.get
      let dt := if lastT == 0.0 then 0.0 else max 0.0 (t - lastT)
      let mut current ← stateRef.get
      if current.animating then
        let nextT := current.t + dt * 0.5
        let wrapped := if nextT >= 1.0 then nextT - 1.0 else nextT
        current := { current with t := wrapped }
        stateRef.set current
      lastTimeRef.set t
      pure current
    let containerStyle : BoxStyle := {
      flexItem := some (FlexItem.growing 1)
      width := .percent 1.0
      height := .percent 1.0
    }
    emit (pure (namedColumn interpName 0 containerStyle #[
      Demos.Linalg.vectorInterpolationWidget env state
    ]))
  pure ()

end Demos
