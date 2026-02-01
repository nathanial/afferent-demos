/-
  Demo Runner - Canopy app linalg BSplineCurveDemo tab content.
-/
import Reactive
import Afferent
import Afferent.Canopy
import Afferent.Canopy.Reactive
import Demos.Core.Demo
import Demos.Linalg.BSplineCurveDemo
import Trellis

open Reactive Reactive.Host
open Afferent
open Afferent.Arbor
open Afferent.Canopy
open Afferent.Canopy.Reactive
open Trellis

namespace Demos
def bSplineCurveDemoTabContent (env : DemoEnv) (elapsedTime : Dynamic Spider Float)
    (stateRef : IO.Ref Demos.Linalg.BSplineCurveDemoState) : WidgetM Unit := do
  let splineName ← registerComponentW "b-spline-curve-demo"

  let clickEvents ← useClickData splineName
  let clickAction ← Event.mapM (fun data => do
    if data.click.button != 0 then
      pure ()
    else
      match data.nameMap.get? splineName with
      | some wid =>
          match data.layouts.get wid with
          | some layout =>
              let rect := layout.contentRect
              let localX := data.click.x - rect.x
              let localY := data.click.y - rect.y
              let rectX := 40.0 * env.screenScale
              let rectY := rect.height - 190.0 * env.screenScale
              let rectW := rect.width - 80.0 * env.screenScale
              let rectH := 120.0 * env.screenScale
              let knotY := rectY + rectH + 12.0
              let state ← stateRef.get
              let spline : Linalg.BSpline Linalg.Vec2 := {
                controlPoints := state.controlPoints
                knots := state.knots
                degree := state.degree
              }
              let mut hitKnot : Option Nat := none
              for i in [:spline.knots.size] do
                let editable := i > spline.degree && i < spline.knots.size - spline.degree - 1
                if editable then
                  let knot := spline.knots.getD i 0.0
                  let x := rectX + knot * rectW
                  let dx := localX - x
                  let dy := localY - knotY
                  if dx * dx + dy * dy <= 70.0 then
                    hitKnot := some i
              match hitKnot with
              | some idx =>
                  let t := Linalg.Float.clamp ((localX - rectX) / rectW) 0.0 1.0
                  let prev := spline.knots.getD (idx - 1) 0.0
                  let next := spline.knots.getD (idx + 1) 1.0
                  let v := Linalg.Float.clamp t prev next
                  let knots := spline.knots.set! idx v
                  stateRef.set { state with knots := knots, dragging := .knot idx }
              | none =>
                  let origin := (rect.width / 2, rect.height / 2 - 40 * env.screenScale)
                  let scale := 60.0 * env.screenScale
                  let worldPos := Demos.Linalg.screenToWorld (localX, localY) origin scale
                  let mut hitPt : Option Nat := none
                  for i in [:state.controlPoints.size] do
                    let p := state.controlPoints.getD i Linalg.Vec2.zero
                    if Demos.Linalg.nearPoint worldPos p 0.45 then
                      hitPt := some i
                  match hitPt with
                  | some idx => stateRef.set { state with dragging := .point idx }
                  | none => pure ()
          | none => pure ()
      | none => pure ()
    ) clickEvents
  performEvent_ clickAction

  let hoverEvents ← useAllHovers
  let hoverAction ← Event.mapM (fun data => do
    let state ← stateRef.get
    match state.dragging with
    | .none => pure ()
    | .point idx =>
        match data.nameMap.get? splineName with
        | some wid =>
            match data.layouts.get wid with
            | some layout =>
                let rect := layout.contentRect
                let localX := data.x - rect.x
                let localY := data.y - rect.y
                let origin := (rect.width / 2, rect.height / 2 - 40 * env.screenScale)
                let scale := 60.0 * env.screenScale
                let worldPos := Demos.Linalg.screenToWorld (localX, localY) origin scale
                if idx < state.controlPoints.size then
                  stateRef.set { state with controlPoints := state.controlPoints.set! idx worldPos }
                else
                  pure ()
            | none => pure ()
        | none => pure ()
    | .knot idx =>
        match data.nameMap.get? splineName with
        | some wid =>
            match data.layouts.get wid with
            | some layout =>
                let rect := layout.contentRect
                let localX := data.x - rect.x
                let rectX := 40.0 * env.screenScale
                let rectW := rect.width - 80.0 * env.screenScale
                let t := Linalg.Float.clamp ((localX - rectX) / rectW) 0.0 1.0
                let prev := state.knots.getD (idx - 1) 0.0
                let next := state.knots.getD (idx + 1) 1.0
                let v := Linalg.Float.clamp t prev next
                stateRef.set { state with knots := state.knots.set! idx v }
            | none => pure ()
        | none => pure ()
    ) hoverEvents
  performEvent_ hoverAction

  let mouseUpEvents ← useAllMouseUp
  let mouseUpAction ← Event.mapM (fun _ => do
    stateRef.modify fun s => { s with dragging := .none }
    ) mouseUpEvents
  performEvent_ mouseUpAction

  let keyEvents ← useKeyboard
  let keyAction ← Event.mapM (fun data => do
    if data.event.isPress then
      match data.event.key with
      | .char 'r' =>
          stateRef.set Demos.Linalg.bSplineCurveDemoInitialState
      | .char 'u' =>
          stateRef.modify fun s =>
            { s with knots := Linalg.BSpline.uniform s.controlPoints s.degree |>.knots }
      | .char '1' =>
          stateRef.modify fun s =>
            let maxDegree := Nat.min 5 (s.controlPoints.size - 1)
            let d := Nat.min 1 maxDegree
            { s with degree := d, knots := Linalg.BSpline.uniform s.controlPoints d |>.knots }
      | .char '2' =>
          stateRef.modify fun s =>
            let maxDegree := Nat.min 5 (s.controlPoints.size - 1)
            let d := Nat.min 2 maxDegree
            { s with degree := d, knots := Linalg.BSpline.uniform s.controlPoints d |>.knots }
      | .char '3' =>
          stateRef.modify fun s =>
            let maxDegree := Nat.min 5 (s.controlPoints.size - 1)
            let d := Nat.min 3 maxDegree
            { s with degree := d, knots := Linalg.BSpline.uniform s.controlPoints d |>.knots }
      | .char '4' =>
          stateRef.modify fun s =>
            let maxDegree := Nat.min 5 (s.controlPoints.size - 1)
            let d := Nat.min 4 maxDegree
            { s with degree := d, knots := Linalg.BSpline.uniform s.controlPoints d |>.knots }
      | .char '5' =>
          stateRef.modify fun s =>
            let maxDegree := Nat.min 5 (s.controlPoints.size - 1)
            let d := Nat.min 5 maxDegree
            { s with degree := d, knots := Linalg.BSpline.uniform s.controlPoints d |>.knots }
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
    emit (pure (namedColumn splineName 0 containerStyle #[
      Demos.Linalg.bSplineCurveDemoWidget env state
    ]))
  pure ()

end Demos
