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
def matrix2DTransformTabContent (env : DemoEnv) (elapsedTime : Dynamic Spider Float)
    (stateRef : IO.Ref Demos.Linalg.Matrix2DTransformState)
    (lastTimeRef : IO.Ref Float) : WidgetM Unit := do
  let mat2dName ← registerComponentW "matrix-2d-transform"
  let keyEvents ← useKeyboard
  let keyAction ← Event.mapM (fun data => do
    if data.event.isPress then
      match data.event.key with
      | .char '1' =>
          stateRef.modify fun s =>
            { s with preset := .identity, matrix := Demos.Linalg.presetToMatrix .identity }
      | .char '2' =>
          stateRef.modify fun s =>
            { s with preset := .rotation45, matrix := Demos.Linalg.presetToMatrix .rotation45 }
      | .char '3' =>
          stateRef.modify fun s =>
            { s with preset := .rotation90, matrix := Demos.Linalg.presetToMatrix .rotation90 }
      | .char '4' =>
          stateRef.modify fun s =>
            { s with preset := .scale2x, matrix := Demos.Linalg.presetToMatrix .scale2x }
      | .char '5' =>
          stateRef.modify fun s =>
            { s with preset := .scaleNonUniform, matrix := Demos.Linalg.presetToMatrix .scaleNonUniform }
      | .char '6' =>
          stateRef.modify fun s =>
            { s with preset := .shearX, matrix := Demos.Linalg.presetToMatrix .shearX }
      | .char '7' =>
          stateRef.modify fun s =>
            { s with preset := .shearY, matrix := Demos.Linalg.presetToMatrix .shearY }
      | .char '8' =>
          stateRef.modify fun s =>
            { s with preset := .reflectX, matrix := Demos.Linalg.presetToMatrix .reflectX }
      | .char '9' =>
          stateRef.modify fun s =>
            { s with preset := .reflectY, matrix := Demos.Linalg.presetToMatrix .reflectY }
      | .tab =>
          stateRef.modify fun s =>
            { s with editingCell := Demos.Linalg.nextMatrixCell s.editingCell }
      | .left =>
          stateRef.modify fun s =>
            { s with editingCell := Demos.Linalg.moveMatrixCell s.editingCell 0 (-1) }
      | .right =>
          stateRef.modify fun s =>
            { s with editingCell := Demos.Linalg.moveMatrixCell s.editingCell 0 1 }
      | .up =>
          stateRef.modify fun s =>
            { s with editingCell := Demos.Linalg.moveMatrixCell s.editingCell (-1) 0 }
      | .down =>
          stateRef.modify fun s =>
            { s with editingCell := Demos.Linalg.moveMatrixCell s.editingCell 1 0 }
      | .char '=' | .char '+' =>
          stateRef.modify fun s =>
            if s.editingCell != .none then
              let updated := Demos.Linalg.modifyMatrixCell s.matrix s.editingCell 0.1
              { s with matrix := updated, preset := .custom }
            else s
      | .char '-' =>
          stateRef.modify fun s =>
            if s.editingCell != .none then
              let updated := Demos.Linalg.modifyMatrixCell s.matrix s.editingCell (-0.1)
              { s with matrix := updated, preset := .custom }
            else s
      | .char 'i' =>
          stateRef.modify fun s =>
            let t := s.translation
            { s with translation := Linalg.Vec2.mk t.x (t.y + 0.1), preset := .custom }
      | .char 'k' =>
          stateRef.modify fun s =>
            let t := s.translation
            { s with translation := Linalg.Vec2.mk t.x (t.y - 0.1), preset := .custom }
      | .char 'j' =>
          stateRef.modify fun s =>
            let t := s.translation
            { s with translation := Linalg.Vec2.mk (t.x - 0.1) t.y, preset := .custom }
      | .char 'l' =>
          stateRef.modify fun s =>
            let t := s.translation
            { s with translation := Linalg.Vec2.mk (t.x + 0.1) t.y, preset := .custom }
      | .char 'g' =>
          stateRef.modify fun s => { s with showGrid := !s.showGrid }
      | .char 'v' =>
          stateRef.modify fun s => { s with showBasisVectors := !s.showBasisVectors }
      | .char 's' =>
          stateRef.modify fun s =>
            let newShape := match s.shape with
              | .square => .triangle
              | .triangle => .arrow
              | .arrow => .square
            { s with shape := newShape }
      | .space =>
          stateRef.modify fun s => { s with animating := !s.animating }
      | _ => pure ()
    ) keyEvents
  performEvent_ keyAction

  let _ ← dynWidget elapsedTime fun t => do
    let state ← SpiderM.liftIO do
      let lastT ← lastTimeRef.get
      let dt := if lastT == 0.0 then 0.0 else max 0.0 (t - lastT)
      let mut current ← stateRef.get
      if current.animating then
        let newT := current.animT + dt * 0.5
        current := { current with animT := if newT >= 1.0 then 0.0 else newT }
        stateRef.set current
      lastTimeRef.set t
      pure current
    let containerStyle : BoxStyle := {
      flexItem := some (FlexItem.growing 1)
      width := .percent 1.0
      height := .percent 1.0
    }
    emit (pure (namedColumn mat2dName 0 containerStyle #[
      Demos.Linalg.matrix2DTransformWidget env state
    ]))
  pure ()

end Demos
