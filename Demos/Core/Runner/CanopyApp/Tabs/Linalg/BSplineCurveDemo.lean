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
import AfferentMath.Widget.MathView2D

open Reactive Reactive.Host
open Afferent
open Afferent.Arbor
open Afferent.Canopy
open Afferent.Canopy.Reactive
open Trellis
open AfferentMath.Widget

namespace Demos
def bSplineCurveDemoTabContent (env : DemoEnv) : WidgetM Unit := do
  let elapsedTime ← useElapsedTime
  let splineName ← registerComponentW "b-spline-curve-demo"

  let clickEvents ← useClickData splineName
  let clickUpdates ← Event.mapM (fun data =>
    if data.click.button != 0 then
      id
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
              fun (state : Demos.Linalg.BSplineCurveDemoState) =>
                let spline : Linalg.BSpline Linalg.Vec2 := {
                  controlPoints := state.controlPoints
                  knots := state.knots
                  degree := state.degree
                }
                let hitKnot := (Array.range spline.knots.size).findSome? fun i =>
                  let editable := i > spline.degree && i < spline.knots.size - spline.degree - 1
                  if editable then
                    let knot := spline.knots.getD i 0.0
                    let x := rectX + knot * rectW
                    let dx := localX - x
                    let dy := localY - knotY
                    if dx * dx + dy * dy <= 70.0 then some i else none
                  else none
                match hitKnot with
                | some idx =>
                    let t := Linalg.Float.clamp ((localX - rectX) / rectW) 0.0 1.0
                    let prev := spline.knots.getD (idx - 1) 0.0
                    let next := spline.knots.getD (idx + 1) 1.0
                    let v := Linalg.Float.clamp t prev next
                    let knots := spline.knots.set! idx v
                    { state with knots := knots, dragging := .knot idx }
                | none =>
                    let config := Demos.Linalg.bSplineMathViewConfig env.screenScale
                    let view := AfferentMath.Widget.MathView2D.viewForSize config rect.width rect.height
                    let worldPos := AfferentMath.Widget.MathView2D.screenToWorld view (localX, localY)
                    let hitPt := (Array.range state.controlPoints.size).findSome? fun i =>
                      let p := state.controlPoints.getD i Linalg.Vec2.zero
                      if Demos.Linalg.nearPoint worldPos p 0.45 then some i else none
                    match hitPt with
                    | some idx => { state with dragging := .point idx }
                    | none => state
          | none => id
      | none => id
    ) clickEvents

  let hoverEvents ← useAllHovers
  let hoverUpdates ← Event.mapM (fun data =>
    match data.nameMap.get? splineName with
    | some wid =>
        match data.layouts.get wid with
        | some layout =>
            let rect := layout.contentRect
            let localX := data.x - rect.x
            let localY := data.y - rect.y
            fun (state : Demos.Linalg.BSplineCurveDemoState) =>
              match state.dragging with
              | .none => state
              | .point idx =>
                  let config := Demos.Linalg.bSplineMathViewConfig env.screenScale
                  let view := AfferentMath.Widget.MathView2D.viewForSize config rect.width rect.height
                  let worldPos := AfferentMath.Widget.MathView2D.screenToWorld view (localX, localY)
                  if idx < state.controlPoints.size then
                    { state with controlPoints := state.controlPoints.set! idx worldPos }
                  else
                    state
              | .knot idx =>
                  let rectX := 40.0 * env.screenScale
                  let rectW := rect.width - 80.0 * env.screenScale
                  let t := Linalg.Float.clamp ((localX - rectX) / rectW) 0.0 1.0
                  let prev := state.knots.getD (idx - 1) 0.0
                  let next := state.knots.getD (idx + 1) 1.0
                  let v := Linalg.Float.clamp t prev next
                  { state with knots := state.knots.set! idx v }
        | none => id
    | none => id
    ) hoverEvents

  let mouseUpEvents ← useAllMouseUp
  let mouseUpUpdates ← Event.mapM (fun _ =>
    fun (s : Demos.Linalg.BSplineCurveDemoState) => { s with dragging := .none }
    ) mouseUpEvents

  let keyEvents ← useKeyboard
  let keyUpdates ← Event.mapM (fun data =>
    fun (s : Demos.Linalg.BSplineCurveDemoState) =>
      if data.event.isPress then
        match data.event.key with
        | .char 'r' => Demos.Linalg.bSplineCurveDemoInitialState
        | .char 'u' =>
            { s with knots := Linalg.BSpline.uniform s.controlPoints s.degree |>.knots }
        | .char '1' =>
            let maxDegree := Nat.min 5 (s.controlPoints.size - 1)
            let d := Nat.min 1 maxDegree
            { s with degree := d, knots := Linalg.BSpline.uniform s.controlPoints d |>.knots }
        | .char '2' =>
            let maxDegree := Nat.min 5 (s.controlPoints.size - 1)
            let d := Nat.min 2 maxDegree
            { s with degree := d, knots := Linalg.BSpline.uniform s.controlPoints d |>.knots }
        | .char '3' =>
            let maxDegree := Nat.min 5 (s.controlPoints.size - 1)
            let d := Nat.min 3 maxDegree
            { s with degree := d, knots := Linalg.BSpline.uniform s.controlPoints d |>.knots }
        | .char '4' =>
            let maxDegree := Nat.min 5 (s.controlPoints.size - 1)
            let d := Nat.min 4 maxDegree
            { s with degree := d, knots := Linalg.BSpline.uniform s.controlPoints d |>.knots }
        | .char '5' =>
            let maxDegree := Nat.min 5 (s.controlPoints.size - 1)
            let d := Nat.min 5 maxDegree
            { s with degree := d, knots := Linalg.BSpline.uniform s.controlPoints d |>.knots }
        | _ => s
      else s
    ) keyEvents

  let allUpdates ← Event.mergeAllListM [clickUpdates, hoverUpdates, mouseUpUpdates, keyUpdates]
  let state ← foldDyn (fun f s => f s) Demos.Linalg.bSplineCurveDemoInitialState allUpdates

  let _ ← dynWidget state fun s => do
    let containerStyle : BoxStyle := {
      flexItem := some (FlexItem.growing 1)
      width := .percent 1.0
      height := .percent 1.0
    }
    emit (pure (namedColumn splineName 0 containerStyle #[
      Demos.Linalg.bSplineCurveDemoWidget env s
    ]))
  pure ()

end Demos
