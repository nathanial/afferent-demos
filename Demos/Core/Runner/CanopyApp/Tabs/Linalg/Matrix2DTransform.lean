/-
  Demo Runner - Canopy app linalg Matrix2DTransform tab content.
-/
import Reactive
import Afferent
import Afferent.Canopy
import Afferent.Canopy.Reactive
import Demos.Core.Demo
import Demos.Linalg.Matrix2DTransform
import Trellis

open Reactive Reactive.Host
open Afferent
open Afferent.Arbor
open Afferent.Canopy
open Afferent.Canopy.Reactive
open Trellis

namespace Demos
def matrix2DTransformTabContent (env : DemoEnv) : WidgetM Unit := do
  let elapsedTime ← useElapsedTime
  let mat2dName ← registerComponentW "matrix-2d-transform"

  let keyEvents ← useKeyboard
  let keyUpdates ← Event.mapM (fun data =>
    fun (s : Demos.Linalg.Matrix2DTransformState) =>
      if data.event.isPress then
        match data.event.key with
        | .char '1' =>
            { s with preset := .identity, matrix := Demos.Linalg.presetToMatrix .identity }
        | .char '2' =>
            { s with preset := .rotation45, matrix := Demos.Linalg.presetToMatrix .rotation45 }
        | .char '3' =>
            { s with preset := .rotation90, matrix := Demos.Linalg.presetToMatrix .rotation90 }
        | .char '4' =>
            { s with preset := .scale2x, matrix := Demos.Linalg.presetToMatrix .scale2x }
        | .char '5' =>
            { s with preset := .scaleNonUniform, matrix := Demos.Linalg.presetToMatrix .scaleNonUniform }
        | .char '6' =>
            { s with preset := .shearX, matrix := Demos.Linalg.presetToMatrix .shearX }
        | .char '7' =>
            { s with preset := .shearY, matrix := Demos.Linalg.presetToMatrix .shearY }
        | .char '8' =>
            { s with preset := .reflectX, matrix := Demos.Linalg.presetToMatrix .reflectX }
        | .char '9' =>
            { s with preset := .reflectY, matrix := Demos.Linalg.presetToMatrix .reflectY }
        | .tab =>
            { s with editingCell := Demos.Linalg.nextMatrixCell s.editingCell }
        | .left =>
            { s with editingCell := Demos.Linalg.moveMatrixCell s.editingCell 0 (-1) }
        | .right =>
            { s with editingCell := Demos.Linalg.moveMatrixCell s.editingCell 0 1 }
        | .up =>
            { s with editingCell := Demos.Linalg.moveMatrixCell s.editingCell (-1) 0 }
        | .down =>
            { s with editingCell := Demos.Linalg.moveMatrixCell s.editingCell 1 0 }
        | .char '=' | .char '+' =>
            if s.editingCell != .none then
              let updated := Demos.Linalg.modifyMatrixCell s.matrix s.editingCell 0.1
              { s with matrix := updated, preset := .custom }
            else s
        | .char '-' =>
            if s.editingCell != .none then
              let updated := Demos.Linalg.modifyMatrixCell s.matrix s.editingCell (-0.1)
              { s with matrix := updated, preset := .custom }
            else s
        | .char 'i' =>
            let t := s.translation
            { s with translation := Linalg.Vec2.mk t.x (t.y + 0.1), preset := .custom }
        | .char 'k' =>
            let t := s.translation
            { s with translation := Linalg.Vec2.mk t.x (t.y - 0.1), preset := .custom }
        | .char 'j' =>
            let t := s.translation
            { s with translation := Linalg.Vec2.mk (t.x - 0.1) t.y, preset := .custom }
        | .char 'l' =>
            let t := s.translation
            { s with translation := Linalg.Vec2.mk (t.x + 0.1) t.y, preset := .custom }
        | .char 'g' => { s with showGrid := !s.showGrid }
        | .char 'v' => { s with showBasisVectors := !s.showBasisVectors }
        | .char 's' =>
            let newShape := match s.shape with
              | .square => .triangle
              | .triangle => .arrow
              | .arrow => .square
            { s with shape := newShape }
        | .space => { s with animating := !s.animating }
        | _ => s
      else s
    ) keyEvents

  -- Time-based animation updates (track lastTime in state)
  let timeUpdates ← Event.mapM (fun t =>
    fun (state : Demos.Linalg.Matrix2DTransformState) =>
      let dt := if state.lastTime == 0.0 then 0.0 else max 0.0 (t - state.lastTime)
      if state.animating then
        let newT := state.animT + dt * 0.5
        { state with animT := if newT >= 1.0 then 0.0 else newT, lastTime := t }
      else
        { state with lastTime := t }
    ) elapsedTime.updated

  let allUpdates ← Event.mergeAllListM [keyUpdates, timeUpdates]
  let state ← foldDyn (fun f s => f s) Demos.Linalg.matrix2DTransformInitialState allUpdates

  let _ ← dynWidget state fun s => do
    let containerStyle : BoxStyle := {
      flexItem := some (FlexItem.growing 1)
      width := .percent 1.0
      height := .percent 1.0
    }
    emit (pure (namedColumn mat2dName 0 containerStyle #[
      Demos.Linalg.matrix2DTransformWidget env s
    ]))
  pure ()

end Demos
