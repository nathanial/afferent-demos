/-
  Demo Runner - Canopy app linalg tab content.
-/
import Reactive
import Afferent
import Afferent.Canopy
import Afferent.Canopy.Reactive
import Demos.Core.Demo
import Demos.Linalg.VectorInterpolation
import Demos.Linalg.VectorArithmetic
import Demos.Linalg.VectorProjection
import Demos.Linalg.VectorField
import Demos.Linalg.CrossProduct3D
import Demos.Linalg.Matrix2DTransform
import Demos.Linalg.Matrix3DTransform
import Demos.Linalg.ProjectionExplorer
import Demos.Linalg.MatrixDecomposition
import Demos.Linalg.QuaternionVisualizer
import Demos.Linalg.SlerpInterpolation
import Demos.Linalg.EulerGimbalLock
import Demos.Linalg.DualQuaternionBlending
import Demos.Linalg.RayCastingPlayground
import Demos.Linalg.PrimitiveOverlapTester
import Demos.Linalg.BarycentricCoordinates
import Demos.Linalg.FrustumCullingDemo
import Demos.Linalg.BezierCurveEditor
import Demos.Linalg.CatmullRomSplineEditor
import Demos.Linalg.BSplineCurveDemo
import Demos.Linalg.ArcLengthParameterization
import Demos.Linalg.BezierPatchSurface
import Demos.Linalg.EasingFunctionGallery
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

def vectorFieldTabContent (env : DemoEnv) (elapsedTime : Dynamic Spider Float)
    (stateRef : IO.Ref Demos.Linalg.VectorFieldState) : WidgetM Unit := do
  let fieldName ← registerComponentW "vector-field"
  let keyEvents ← useKeyboard
  let keyAction ← Event.mapM (fun data => do
    if data.event.isPress then
      match data.event.key with
      | .char '1' =>
          stateRef.modify fun s => { s with fieldType := .radial }
      | .char '2' =>
          stateRef.modify fun s => { s with fieldType := .rotational }
      | .char '3' =>
          stateRef.modify fun s => { s with fieldType := .gradient }
      | .char '4' =>
          stateRef.modify fun s => { s with fieldType := .saddle }
      | .char '=' | .char '+' =>
          stateRef.modify fun s =>
            { s with gridResolution := Nat.min 24 (s.gridResolution + 2) }
      | .char '-' =>
          stateRef.modify fun s =>
            { s with gridResolution := Nat.max 4 (s.gridResolution - 2) }
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
    emit (pure (namedColumn fieldName 0 containerStyle #[
      Demos.Linalg.vectorFieldWidget env state
    ]))
  pure ()

def crossProduct3DTabContent (env : DemoEnv) (elapsedTime : Dynamic Spider Float)
    (stateRef : IO.Ref Demos.Linalg.CrossProduct3DState) : WidgetM Unit := do
  let crossName ← registerComponentW "cross-product-3d"

  let clickEvents ← useClickData crossName
  let clickAction ← Event.mapM (fun data => do
    if data.click.button != 0 then
      pure ()
    else
      stateRef.modify fun s =>
        { s with dragging := .camera, lastMouseX := data.click.x, lastMouseY := data.click.y }
    ) clickEvents
  performEvent_ clickAction

  let hoverEvents ← useAllHovers
  let hoverAction ← Event.mapM (fun data => do
    let state ← stateRef.get
    match state.dragging with
    | .camera =>
        let dx := data.x - state.lastMouseX
        let dy := data.y - state.lastMouseY
        let newYaw := state.cameraYaw + dx * 0.01
        let rawPitch := state.cameraPitch + dy * 0.01
        let newPitch := if rawPitch < -1.5 then -1.5 else if rawPitch > 1.5 then 1.5 else rawPitch
        stateRef.set { state with
          cameraYaw := newYaw
          cameraPitch := newPitch
          lastMouseX := data.x
          lastMouseY := data.y
        }
    | _ => pure ()
    ) hoverEvents
  performEvent_ hoverAction

  let mouseUpEvents ← useAllMouseUp
  let mouseUpAction ← Event.mapM (fun data => do
    if data.button == 0 then
      stateRef.modify fun s => { s with dragging := .none }
    ) mouseUpEvents
  performEvent_ mouseUpAction

  let keyEvents ← useKeyboard
  let keyAction ← Event.mapM (fun data => do
    if data.event.isPress then
      match data.event.key with
      | .char 'p' =>
          stateRef.modify fun s => { s with showParallelogram := !s.showParallelogram }
      | .char 'r' =>
          stateRef.modify fun s => { s with cameraYaw := 0.6, cameraPitch := 0.4 }
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
    emit (pure (namedColumn crossName 0 containerStyle #[
      Demos.Linalg.crossProduct3DWidget env state
    ]))
  pure ()

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

def matrix3DTransformTabContent (env : DemoEnv) (elapsedTime : Dynamic Spider Float)
    (stateRef : IO.Ref Demos.Linalg.Matrix3DTransformState) : WidgetM Unit := do
  let mat3dName ← registerComponentW "matrix-3d-transform"

  let keyEvents ← useKeyboard
  let keyAction ← Event.mapM (fun data => do
    if data.event.isPress then
      match data.event.key with
      | .char '1' =>
          stateRef.modify fun s => { s with selectedIndex := some 0 }
      | .char '2' =>
          stateRef.modify fun s => { s with selectedIndex := some 1 }
      | .char '3' =>
          stateRef.modify fun s => { s with selectedIndex := some 2 }
      | .up =>
          stateRef.modify fun s =>
            match s.selectedIndex with
            | some idx =>
                if idx > 0 then
                  let arr := s.transforms
                  let temp := arr.getD idx (.rotateX 0)
                  let arr := arr.set! idx (arr.getD (idx - 1) (.rotateX 0))
                  let arr := arr.set! (idx - 1) temp
                  { s with transforms := arr, selectedIndex := some (idx - 1) }
                else s
            | none => s
      | .down =>
          stateRef.modify fun s =>
            match s.selectedIndex with
            | some idx =>
                if idx + 1 < s.transforms.size then
                  let arr := s.transforms
                  let temp := arr.getD idx (.rotateX 0)
                  let arr := arr.set! idx (arr.getD (idx + 1) (.rotateX 0))
                  let arr := arr.set! (idx + 1) temp
                  { s with transforms := arr, selectedIndex := some (idx + 1) }
                else s
            | none => s
      | .char 'a' =>
          stateRef.modify fun s => { s with showAxes := !s.showAxes }
      | .char 'i' =>
          stateRef.modify fun s => { s with showIntermediateSteps := !s.showIntermediateSteps }
      | .char 'r' =>
          stateRef.modify fun s => { s with cameraYaw := 0.5, cameraPitch := 0.3 }
      | _ => pure ()
    ) keyEvents
  performEvent_ keyAction

  let clickEvents ← useClickData mat3dName
  let clickAction ← Event.mapM (fun data => do
    if data.click.button != 0 then
      pure ()
    else
      stateRef.modify fun s => { s with dragging := true, lastMouseX := data.click.x, lastMouseY := data.click.y }
    ) clickEvents
  performEvent_ clickAction

  let hoverEvents ← useAllHovers
  let hoverAction ← Event.mapM (fun data => do
    let state ← stateRef.get
    if state.dragging then
      let dx := data.x - state.lastMouseX
      let dy := data.y - state.lastMouseY
      let newYaw := state.cameraYaw + dx * 0.01
      let rawPitch := state.cameraPitch + dy * 0.01
      let newPitch := if rawPitch < -1.5 then -1.5 else if rawPitch > 1.5 then 1.5 else rawPitch
      stateRef.set { state with
        cameraYaw := newYaw
        cameraPitch := newPitch
        lastMouseX := data.x
        lastMouseY := data.y
      }
    else
      pure ()
    ) hoverEvents
  performEvent_ hoverAction

  let mouseUpEvents ← useAllMouseUp
  let mouseUpAction ← Event.mapM (fun data => do
    if data.button == 0 then
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
    emit (pure (namedColumn mat3dName 0 containerStyle #[
      Demos.Linalg.matrix3DTransformWidget env state
    ]))
  pure ()

def projectionExplorerTabContent (env : DemoEnv) (elapsedTime : Dynamic Spider Float)
    (stateRef : IO.Ref Demos.Linalg.ProjectionExplorerState) : WidgetM Unit := do
  let projName ← registerComponentW "projection-explorer"

  let keyEvents ← useKeyboard
  let keyAction ← Event.mapM (fun data => do
    if data.event.isPress then
      match data.event.key with
      | .tab =>
          stateRef.modify fun s =>
            { s with projType := match s.projType with
              | .perspective => .orthographic
              | .orthographic => .perspective
            }
      | .char 'f' =>
          stateRef.modify fun s =>
            let newFar := s.far + 0.5
            { s with far := if newFar > 10.0 then 10.0 else newFar }
      | .char 'n' =>
          stateRef.modify fun s =>
            let newNear := s.near + 0.1
            let maxNear := s.far - 0.5
            let clampedNear := if newNear > maxNear then maxNear else newNear
            { s with near := if clampedNear < 0.1 then 0.1 else clampedNear }
      | .char '=' | .char '+' =>
          stateRef.modify fun s =>
            match s.projType with
            | .perspective =>
                let newFov := s.fov + 0.1
                let maxFov := 2.513
                { s with fov := if newFov > maxFov then maxFov else newFov }
            | .orthographic =>
                let newSize := s.orthoSize + 0.2
                { s with orthoSize := if newSize > 5.0 then 5.0 else newSize }
      | .char '-' =>
          stateRef.modify fun s =>
            match s.projType with
            | .perspective =>
                let newFov := s.fov - 0.1
                { s with fov := if newFov < 0.3 then 0.3 else newFov }
            | .orthographic =>
                let newSize := s.orthoSize - 0.2
                { s with orthoSize := if newSize < 0.5 then 0.5 else newSize }
      | .char 'c' =>
          stateRef.modify fun s => { s with showClipSpace := !s.showClipSpace }
      | .char 'o' =>
          stateRef.modify fun s => { s with showTestObjects := !s.showTestObjects }
      | _ => pure ()
    ) keyEvents
  performEvent_ keyAction

  let clickEvents ← useClickData projName
  let clickAction ← Event.mapM (fun data => do
    if data.click.button != 0 then
      pure ()
    else
      stateRef.modify fun s => { s with dragging := true, lastMouseX := data.click.x, lastMouseY := data.click.y }
    ) clickEvents
  performEvent_ clickAction

  let hoverEvents ← useAllHovers
  let hoverAction ← Event.mapM (fun data => do
    let state ← stateRef.get
    if state.dragging then
      let dx := data.x - state.lastMouseX
      let dy := data.y - state.lastMouseY
      let newYaw := state.cameraYaw + dx * 0.01
      let rawPitch := state.cameraPitch + dy * 0.01
      let newPitch := if rawPitch < -1.5 then -1.5 else if rawPitch > 1.5 then 1.5 else rawPitch
      stateRef.set { state with
        cameraYaw := newYaw
        cameraPitch := newPitch
        lastMouseX := data.x
        lastMouseY := data.y
      }
    else
      pure ()
    ) hoverEvents
  performEvent_ hoverAction

  let mouseUpEvents ← useAllMouseUp
  let mouseUpAction ← Event.mapM (fun data => do
    if data.button == 0 then
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
    emit (pure (namedColumn projName 0 containerStyle #[
      Demos.Linalg.projectionExplorerWidget env state
    ]))
  pure ()

def matrixDecompositionTabContent (env : DemoEnv) (elapsedTime : Dynamic Spider Float)
    (stateRef : IO.Ref Demos.Linalg.MatrixDecompositionState) : WidgetM Unit := do
  let decompName ← registerComponentW "matrix-decomposition"
  let keyEvents ← useKeyboard
  let keyAction ← Event.mapM (fun data => do
    if data.event.isPress then
      match data.event.key with
      | .char '1' =>
          stateRef.modify fun s =>
            if 0 < Demos.Linalg.decompositionPresets.size then
              let (_, m) := Demos.Linalg.decompositionPresets.getD 0 ("", Linalg.Mat2.identity)
              { s with matrix := m, decomp := Demos.Linalg.decomposeMatrix2D m, presetIndex := 0 }
            else s
      | .char '2' =>
          stateRef.modify fun s =>
            if 1 < Demos.Linalg.decompositionPresets.size then
              let (_, m) := Demos.Linalg.decompositionPresets.getD 1 ("", Linalg.Mat2.identity)
              { s with matrix := m, decomp := Demos.Linalg.decomposeMatrix2D m, presetIndex := 1 }
            else s
      | .char '3' =>
          stateRef.modify fun s =>
            if 2 < Demos.Linalg.decompositionPresets.size then
              let (_, m) := Demos.Linalg.decompositionPresets.getD 2 ("", Linalg.Mat2.identity)
              { s with matrix := m, decomp := Demos.Linalg.decomposeMatrix2D m, presetIndex := 2 }
            else s
      | .char '4' =>
          stateRef.modify fun s =>
            if 3 < Demos.Linalg.decompositionPresets.size then
              let (_, m) := Demos.Linalg.decompositionPresets.getD 3 ("", Linalg.Mat2.identity)
              { s with matrix := m, decomp := Demos.Linalg.decomposeMatrix2D m, presetIndex := 3 }
            else s
      | .char '5' =>
          stateRef.modify fun s =>
            if 4 < Demos.Linalg.decompositionPresets.size then
              let (_, m) := Demos.Linalg.decompositionPresets.getD 4 ("", Linalg.Mat2.identity)
              { s with matrix := m, decomp := Demos.Linalg.decomposeMatrix2D m, presetIndex := 4 }
            else s
      | .char '6' =>
          stateRef.modify fun s =>
            if 5 < Demos.Linalg.decompositionPresets.size then
              let (_, m) := Demos.Linalg.decompositionPresets.getD 5 ("", Linalg.Mat2.identity)
              { s with matrix := m, decomp := Demos.Linalg.decomposeMatrix2D m, presetIndex := 5 }
            else s
      | .tab =>
          stateRef.modify fun s =>
            { s with currentStep := match s.currentStep with
              | .original => .afterRotation1
              | .afterRotation1 => .afterScale
              | .afterScale => .afterRotation2
              | .afterRotation2 => .original
            }
      | .char 'c' =>
          stateRef.modify fun s => { s with showComponents := !s.showComponents }
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
    emit (pure (namedColumn decompName 0 containerStyle #[
      Demos.Linalg.matrixDecompositionWidget env state
    ]))
  pure ()

def quaternionVisualizerTabContent (env : DemoEnv) (elapsedTime : Dynamic Spider Float)
    (stateRef : IO.Ref Demos.Linalg.QuaternionVisualizerState) : WidgetM Unit := do
  let quatName ← registerComponentW "quaternion-visualizer"

  let clickEvents ← useClickData quatName
  let clickAction ← Event.mapM (fun data => do
    if data.click.button != 0 && data.click.button != 1 then
      pure ()
    else
      match data.nameMap.get? quatName with
      | some wid =>
          match data.layouts.get wid with
          | some layout =>
              let rect := layout.contentRect
              let localX := data.click.x - rect.x
              let localY := data.click.y - rect.y
              let labels : Array (Demos.Linalg.QuatComponent × Nat) := #[(.x, 0), (.y, 1), (.z, 2), (.w, 3)]
              let mut hitSlider : Option Demos.Linalg.QuatComponent := none
              for (comp, idx) in labels do
                let layout := Demos.Linalg.sliderLayoutFor rect.width rect.height env.screenScale idx
                let hit := localX >= layout.x && localX <= layout.x + layout.width
                  && localY >= layout.y - 8.0 && localY <= layout.y + layout.height + 8.0
                if hit then
                  hitSlider := some comp
              let state ← stateRef.get
              match hitSlider with
              | some comp =>
                  let idx := match comp with | .x => 0 | .y => 1 | .z => 2 | .w => 3
                  let layout := Demos.Linalg.sliderLayoutFor rect.width rect.height env.screenScale idx
                  let t := (localX - layout.x) / layout.width
                  let value := Demos.Linalg.clampUnit (t * 2.0 - 1.0)
                  let q := (Demos.Linalg.setQuatComponent state.quat comp value).normalize
                  stateRef.set { state with
                    quat := q
                    dragging := .slider comp
                    lastMouseX := data.click.x
                    lastMouseY := data.click.y
                  }
              | none =>
                  if data.click.button == 1 then
                    stateRef.set { state with dragging := .camera, lastMouseX := data.click.x, lastMouseY := data.click.y }
                  else
                    stateRef.set { state with dragging := .arcball, lastMouseX := data.click.x, lastMouseY := data.click.y }
          | none => pure ()
      | none => pure ()
    ) clickEvents
  performEvent_ clickAction

  let hoverEvents ← useAllHovers
  let hoverAction ← Event.mapM (fun data => do
    let state ← stateRef.get
    match state.dragging with
    | .none => pure ()
    | .camera =>
        let dx := data.x - state.lastMouseX
        let dy := data.y - state.lastMouseY
        let yaw := state.cameraYaw + dx * 0.005
        let pitch := state.cameraPitch + dy * 0.005
        stateRef.set { state with cameraYaw := yaw, cameraPitch := pitch, lastMouseX := data.x, lastMouseY := data.y }
    | .arcball =>
        let dx := data.x - state.lastMouseX
        let dy := data.y - state.lastMouseY
        let rotY := Linalg.Quat.fromAxisAngle Linalg.Vec3.unitY (dx * 0.008)
        let rotX := Linalg.Quat.fromAxisAngle Linalg.Vec3.unitX (dy * 0.008)
        let q := Linalg.Quat.multiply rotY (Linalg.Quat.multiply rotX state.quat) |>.normalize
        stateRef.set { state with quat := q, lastMouseX := data.x, lastMouseY := data.y }
    | .slider comp =>
        match data.nameMap.get? quatName with
        | some wid =>
            match data.layouts.get wid with
            | some layout =>
                let rect := layout.contentRect
                let localX := data.x - rect.x
                let idx := match comp with | .x => 0 | .y => 1 | .z => 2 | .w => 3
                let layout := Demos.Linalg.sliderLayoutFor rect.width rect.height env.screenScale idx
                let t := (localX - layout.x) / layout.width
                let value := Demos.Linalg.clampUnit (t * 2.0 - 1.0)
                let q := (Demos.Linalg.setQuatComponent state.quat comp value).normalize
                stateRef.set { state with quat := q, lastMouseX := data.x, lastMouseY := data.y }
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
          stateRef.set Demos.Linalg.quaternionVisualizerInitialState
      | .char 'e' =>
          stateRef.modify fun s =>
            let e := s.eulerAngles
            let q := Linalg.Quat.fromEuler e.x e.y e.z
            { s with quat := q.normalize }
      | .char '1' =>
          stateRef.modify fun s => { s with selectedEuler := 0 }
      | .char '2' =>
          stateRef.modify fun s => { s with selectedEuler := 1 }
      | .char '3' =>
          stateRef.modify fun s => { s with selectedEuler := 2 }
      | .left =>
          stateRef.modify fun s =>
            let delta := -5.0 * Linalg.Float.pi / 180.0
            let e := s.eulerAngles
            let e' := match s.selectedEuler with
              | 0 => Linalg.Vec3.mk (e.x + delta) e.y e.z
              | 1 => Linalg.Vec3.mk e.x (e.y + delta) e.z
              | _ => Linalg.Vec3.mk e.x e.y (e.z + delta)
            { s with eulerAngles := e' }
      | .right =>
          stateRef.modify fun s =>
            let delta := 5.0 * Linalg.Float.pi / 180.0
            let e := s.eulerAngles
            let e' := match s.selectedEuler with
              | 0 => Linalg.Vec3.mk (e.x + delta) e.y e.z
              | 1 => Linalg.Vec3.mk e.x (e.y + delta) e.z
              | _ => Linalg.Vec3.mk e.x e.y (e.z + delta)
            { s with eulerAngles := e' }
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
    emit (pure (namedColumn quatName 0 containerStyle #[
      Demos.Linalg.quaternionVisualizerWidget env state
    ]))
  pure ()

def slerpInterpolationTabContent (env : DemoEnv) (elapsedTime : Dynamic Spider Float)
    (stateRef : IO.Ref Demos.Linalg.SlerpInterpolationState)
    (lastTimeRef : IO.Ref Float) : WidgetM Unit := do
  let slerpName ← registerComponentW "slerp-interpolation"

  let clickEvents ← useClickData slerpName
  let clickAction ← Event.mapM (fun data => do
    if data.click.button != 0 then
      pure ()
    else
      stateRef.modify fun s => { s with dragging := true, lastMouseX := data.click.x, lastMouseY := data.click.y }
    ) clickEvents
  performEvent_ clickAction

  let hoverEvents ← useAllHovers
  let hoverAction ← Event.mapM (fun data => do
    let state ← stateRef.get
    if !state.dragging then
      pure ()
    else
      let dx := data.x - state.lastMouseX
      let dy := data.y - state.lastMouseY
      let newYaw := state.cameraYaw + dx * 0.005
      let newPitch := state.cameraPitch + dy * 0.005
      stateRef.set { state with
        cameraYaw := newYaw
        cameraPitch := newPitch
        lastMouseX := data.x
        lastMouseY := data.y
      }
    ) hoverEvents
  performEvent_ hoverAction

  let mouseUpEvents ← useAllMouseUp
  let mouseUpAction ← Event.mapM (fun data => do
    if data.button == 0 then
      stateRef.modify fun s => { s with dragging := false }
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
        let newT := current.t + dt * 0.35
        current := { current with t := if newT > 1.0 then newT - 1.0 else newT }
        stateRef.set current
      lastTimeRef.set t
      pure current
    let containerStyle : BoxStyle := {
      flexItem := some (FlexItem.growing 1)
      width := .percent 1.0
      height := .percent 1.0
    }
    emit (pure (namedColumn slerpName 0 containerStyle #[
      Demos.Linalg.slerpInterpolationWidget env state
    ]))
  pure ()

def eulerGimbalLockTabContent (env : DemoEnv) (elapsedTime : Dynamic Spider Float)
    (stateRef : IO.Ref Demos.Linalg.EulerGimbalLockState) : WidgetM Unit := do
  let gimbalName ← registerComponentW "euler-gimbal-lock"

  let keyEvents ← useKeyboard
  let keyAction ← Event.mapM (fun data => do
    if data.event.isPress then
      match data.event.key with
      | .char 'r' =>
          stateRef.set Demos.Linalg.eulerGimbalLockInitialState
      | .char '1' =>
          stateRef.modify fun s => { s with selectedAxis := 0 }
      | .char '2' =>
          stateRef.modify fun s => { s with selectedAxis := 1 }
      | .char '3' =>
          stateRef.modify fun s => { s with selectedAxis := 2 }
      | .char 'o' =>
          stateRef.modify fun s =>
            let nextOrder := match s.euler.order with
              | .XYZ => .XZY
              | .XZY => .YXZ
              | .YXZ => .YZX
              | .YZX => .ZXY
              | .ZXY => .ZYX
              | .ZYX => .XYZ
            { s with euler := { s.euler with order := nextOrder } }
      | .left | .right =>
          stateRef.modify fun s =>
            let delta := if data.event.key == .left then -5.0 else 5.0
            let e := s.euler
            let radDelta := delta * Linalg.Float.pi / 180.0
            let e' := match s.selectedAxis with
              | 0 => { e with a1 := e.a1 + radDelta }
              | 1 => { e with a2 := e.a2 + radDelta }
              | _ => { e with a3 := e.a3 + radDelta }
            { s with euler := e' }
      | _ => pure ()
    ) keyEvents
  performEvent_ keyAction

  let clickEvents ← useClickData gimbalName
  let clickAction ← Event.mapM (fun data => do
    if data.click.button != 0 then
      pure ()
    else
      stateRef.modify fun s => { s with dragging := true, lastMouseX := data.click.x, lastMouseY := data.click.y }
    ) clickEvents
  performEvent_ clickAction

  let hoverEvents ← useAllHovers
  let hoverAction ← Event.mapM (fun data => do
    let state ← stateRef.get
    if !state.dragging then
      pure ()
    else
      let dx := data.x - state.lastMouseX
      let dy := data.y - state.lastMouseY
      let newYaw := state.cameraYaw + dx * 0.005
      let newPitch := state.cameraPitch + dy * 0.005
      stateRef.set { state with
        cameraYaw := newYaw
        cameraPitch := newPitch
        lastMouseX := data.x
        lastMouseY := data.y
      }
    ) hoverEvents
  performEvent_ hoverAction

  let mouseUpEvents ← useAllMouseUp
  let mouseUpAction ← Event.mapM (fun data => do
    if data.button == 0 then
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
    emit (pure (namedColumn gimbalName 0 containerStyle #[
      Demos.Linalg.eulerGimbalLockWidget env state
    ]))
  pure ()

def dualQuaternionBlendingTabContent (env : DemoEnv) (elapsedTime : Dynamic Spider Float)
    (stateRef : IO.Ref Demos.Linalg.DualQuaternionBlendingState) : WidgetM Unit := do
  let dualName ← registerComponentW "dual-quaternion-blending"

  let keyEvents ← useKeyboard
  let keyAction ← Event.mapM (fun data => do
    if data.event.isPress then
      match data.event.key with
      | .char 'r' =>
          stateRef.set Demos.Linalg.dualQuaternionBlendingInitialState
      | .char 't' =>
          stateRef.modify fun s => { s with twist := s.twist + 0.1 }
      | .char 'g' =>
          stateRef.modify fun s => { s with twist := s.twist - 0.1 }
      | .char 'b' =>
          stateRef.modify fun s => { s with bend := s.bend + 0.1 }
      | .char 'v' =>
          stateRef.modify fun s => { s with bend := s.bend - 0.1 }
      | _ => pure ()
    ) keyEvents
  performEvent_ keyAction

  let clickEvents ← useClickData dualName
  let clickAction ← Event.mapM (fun data => do
    if data.click.button != 0 then
      pure ()
    else
      stateRef.modify fun s => { s with dragging := true, lastMouseX := data.click.x, lastMouseY := data.click.y }
    ) clickEvents
  performEvent_ clickAction

  let hoverEvents ← useAllHovers
  let hoverAction ← Event.mapM (fun data => do
    let state ← stateRef.get
    if !state.dragging then
      pure ()
    else
      let dx := data.x - state.lastMouseX
      let dy := data.y - state.lastMouseY
      let newYaw := state.cameraYaw + dx * 0.005
      let newPitch := state.cameraPitch + dy * 0.005
      stateRef.set { state with
        cameraYaw := newYaw
        cameraPitch := newPitch
        lastMouseX := data.x
        lastMouseY := data.y
      }
    ) hoverEvents
  performEvent_ hoverAction

  let mouseUpEvents ← useAllMouseUp
  let mouseUpAction ← Event.mapM (fun data => do
    if data.button == 0 then
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
    emit (pure (namedColumn dualName 0 containerStyle #[
      Demos.Linalg.dualQuaternionBlendingWidget env state
    ]))
  pure ()

def rayCastingPlaygroundTabContent (env : DemoEnv) (elapsedTime : Dynamic Spider Float)
    (stateRef : IO.Ref Demos.Linalg.RayCastingPlaygroundState) : WidgetM Unit := do
  let rayName ← registerComponentW "ray-casting-playground"

  let keyEvents ← useKeyboard
  let keyAction ← Event.mapM (fun data => do
    if data.event.key == .char 'r' && data.event.isPress then
      stateRef.set Demos.Linalg.rayCastingPlaygroundInitialState
    ) keyEvents
  performEvent_ keyAction

  let clickEvents ← useClickData rayName
  let clickAction ← Event.mapM (fun data => do
    if data.click.button != 0 && data.click.button != 1 then
      pure ()
    else
      match data.nameMap.get? rayName with
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
              let origin2 := Linalg.Vec2.mk state.rayOrigin.x state.rayOrigin.z
              let target2 := Linalg.Vec2.mk state.rayTarget.x state.rayTarget.z
              if data.click.button == 1 then
                stateRef.set { state with dragging := .camera, lastMouseX := data.click.x, lastMouseY := data.click.y }
              else if Demos.Linalg.nearPoint worldPos origin2 0.5 then
                stateRef.set { state with dragging := .origin, lastMouseX := data.click.x, lastMouseY := data.click.y }
              else if Demos.Linalg.nearPoint worldPos target2 0.5 then
                stateRef.set { state with dragging := .direction, lastMouseX := data.click.x, lastMouseY := data.click.y }
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
    | .none => pure ()
    | .camera =>
        let dx := data.x - state.lastMouseX
        let dy := data.y - state.lastMouseY
        let newYaw := state.cameraYaw + dx * 0.005
        let newPitch := state.cameraPitch + dy * 0.005
        stateRef.set { state with
          cameraYaw := newYaw
          cameraPitch := newPitch
          lastMouseX := data.x
          lastMouseY := data.y
        }
    | .origin =>
        match data.nameMap.get? rayName with
        | some wid =>
            match data.layouts.get wid with
            | some layout =>
                let rect := layout.contentRect
                let localX := data.x - rect.x
                let localY := data.y - rect.y
                let origin := (rect.width / 2, rect.height / 2)
                let scale := 70.0 * env.screenScale
                let worldPos := Demos.Linalg.screenToWorld (localX, localY) origin scale
                let newOrigin := Linalg.Vec3.mk worldPos.x 0.0 worldPos.y
                stateRef.set { state with rayOrigin := newOrigin, lastMouseX := data.x, lastMouseY := data.y }
            | none => pure ()
        | none => pure ()
    | .direction =>
        match data.nameMap.get? rayName with
        | some wid =>
            match data.layouts.get wid with
            | some layout =>
                let rect := layout.contentRect
                let localX := data.x - rect.x
                let localY := data.y - rect.y
                let origin := (rect.width / 2, rect.height / 2)
                let scale := 70.0 * env.screenScale
                let worldPos := Demos.Linalg.screenToWorld (localX, localY) origin scale
                let newTarget := Linalg.Vec3.mk worldPos.x 0.0 worldPos.y
                stateRef.set { state with rayTarget := newTarget, lastMouseX := data.x, lastMouseY := data.y }
            | none => pure ()
        | none => pure ()
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
    emit (pure (namedColumn rayName 0 containerStyle #[
      Demos.Linalg.rayCastingPlaygroundWidget env state
    ]))
  pure ()

def primitiveOverlapTesterTabContent (env : DemoEnv) (elapsedTime : Dynamic Spider Float)
    (stateRef : IO.Ref Demos.Linalg.PrimitiveOverlapTesterState) : WidgetM Unit := do
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

def frustumCullingDemoTabContent (env : DemoEnv) (elapsedTime : Dynamic Spider Float)
    (stateRef : IO.Ref Demos.Linalg.FrustumCullingDemoState) : WidgetM Unit := do
  let frustumName ← registerComponentW "frustum-culling-demo"

  let keyEvents ← useKeyboard
  let keyAction ← Event.mapM (fun data => do
    if data.event.isPress then
      match data.event.key with
      | .char 'r' =>
          stateRef.set Demos.Linalg.frustumCullingDemoInitialState
      | .char 'j' =>
          stateRef.modify fun s => { s with camYaw := s.camYaw - 0.08 }
      | .char 'l' =>
          stateRef.modify fun s => { s with camYaw := s.camYaw + 0.08 }
      | .char 'i' =>
          stateRef.modify fun s =>
            let newPitch := s.camPitch + 0.08
            { s with camPitch := if newPitch > 1.2 then 1.2 else newPitch }
      | .char 'k' =>
          stateRef.modify fun s =>
            let newPitch := s.camPitch - 0.08
            { s with camPitch := if newPitch < -1.2 then -1.2 else newPitch }
      | .char '=' | .char '+' =>
          stateRef.modify fun s =>
            let newDist := s.camDist - 0.3
            { s with camDist := if newDist < 2.0 then 2.0 else newDist }
      | .char '-' =>
          stateRef.modify fun s => { s with camDist := s.camDist + 0.3 }
      | _ => pure ()
    ) keyEvents
  performEvent_ keyAction

  let clickEvents ← useClickData frustumName
  let clickAction ← Event.mapM (fun data => do
    if data.click.button != 0 then
      pure ()
    else
      stateRef.modify fun s => { s with dragging := true, lastMouseX := data.click.x, lastMouseY := data.click.y }
    ) clickEvents
  performEvent_ clickAction

  let hoverEvents ← useAllHovers
  let hoverAction ← Event.mapM (fun data => do
    let state ← stateRef.get
    if !state.dragging then
      pure ()
    else
      let dx := data.x - state.lastMouseX
      let dy := data.y - state.lastMouseY
      let newYaw := state.viewYaw + dx * 0.005
      let newPitch := state.viewPitch + dy * 0.005
      stateRef.set { state with
        viewYaw := newYaw
        viewPitch := newPitch
        lastMouseX := data.x
        lastMouseY := data.y
      }
    ) hoverEvents
  performEvent_ hoverAction

  let mouseUpEvents ← useAllMouseUp
  let mouseUpAction ← Event.mapM (fun data => do
    if data.button == 0 then
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
    emit (pure (namedColumn frustumName 0 containerStyle #[
      Demos.Linalg.frustumCullingDemoWidget env state
    ]))
  pure ()

def bezierCurveEditorTabContent (env : DemoEnv) (elapsedTime : Dynamic Spider Float)
    (stateRef : IO.Ref Demos.Linalg.BezierCurveEditorState) : WidgetM Unit := do
  let bezierName ← registerComponentW "bezier-curve-editor"

  let clickEvents ← useClickData bezierName
  let clickAction ← Event.mapM (fun data => do
    if data.click.button != 0 then
      pure ()
    else
      match data.nameMap.get? bezierName with
      | some wid =>
          match data.layouts.get wid with
          | some layout =>
              let rect := layout.contentRect
              let localX := data.click.x - rect.x
              let localY := data.click.y - rect.y
              let sliderX := rect.width - 250.0 * env.screenScale
              let sliderY := 95.0 * env.screenScale
              let sliderW := 180.0 * env.screenScale
              let sliderH := 8.0 * env.screenScale
              let hitSlider := localX >= sliderX && localX <= sliderX + sliderW
                && localY >= sliderY - 8.0 && localY <= sliderY + sliderH + 8.0
              if hitSlider then
                let t := Linalg.Float.clamp ((localX - sliderX) / sliderW) 0.0 1.0
                stateRef.modify fun s => { s with t := t, dragging := .slider }
              else
                let origin := (rect.width / 2, rect.height / 2)
                let scale := 70.0 * env.screenScale
                let worldPos := Demos.Linalg.screenToWorld (localX, localY) origin scale
                let state ← stateRef.get
                let points := match state.mode with
                  | .quadratic => state.quadPoints
                  | .cubic => state.cubicPoints
                let mut hit : Option Nat := none
                for i in [:points.size] do
                  let p := points.getD i Linalg.Vec2.zero
                  if Demos.Linalg.nearPoint worldPos p 0.45 then
                    hit := some i
                match hit with
                | some idx => stateRef.set { state with dragging := .control idx }
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
    | .slider =>
        match data.nameMap.get? bezierName with
        | some wid =>
            match data.layouts.get wid with
            | some layout =>
                let rect := layout.contentRect
                let localX := data.x - rect.x
                let sliderX := rect.width - 250.0 * env.screenScale
                let sliderW := 180.0 * env.screenScale
                let t := Linalg.Float.clamp ((localX - sliderX) / sliderW) 0.0 1.0
                stateRef.set { state with t := t }
            | none => pure ()
        | none => pure ()
    | .control idx =>
        match data.nameMap.get? bezierName with
        | some wid =>
            match data.layouts.get wid with
            | some layout =>
                let rect := layout.contentRect
                let localX := data.x - rect.x
                let localY := data.y - rect.y
                let origin := (rect.width / 2, rect.height / 2)
                let scale := 70.0 * env.screenScale
                let worldPos := Demos.Linalg.screenToWorld (localX, localY) origin scale
                let next := match state.mode with
                  | .quadratic =>
                      if idx < state.quadPoints.size then
                        { state with quadPoints := state.quadPoints.set! idx worldPos }
                      else state
                  | .cubic =>
                      if idx < state.cubicPoints.size then
                        { state with cubicPoints := state.cubicPoints.set! idx worldPos }
                      else state
                stateRef.set next
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
          stateRef.set Demos.Linalg.bezierCurveEditorInitialState
      | .char 'q' =>
          stateRef.modify fun s => { s with mode := .quadratic }
      | .char 'c' =>
          stateRef.modify fun s => { s with mode := .cubic }
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
    emit (pure (namedColumn bezierName 0 containerStyle #[
      Demos.Linalg.bezierCurveEditorWidget env state
    ]))
  pure ()

def catmullRomSplineEditorTabContent (env : DemoEnv) (elapsedTime : Dynamic Spider Float)
    (stateRef : IO.Ref Demos.Linalg.CatmullRomSplineEditorState)
    (lastTimeRef : IO.Ref Float) : WidgetM Unit := do
  let catmullName ← registerComponentW "catmull-rom-spline-editor"

  let clickEvents ← useClickData catmullName
  let clickAction ← Event.mapM (fun data => do
    if data.click.button != 0 then
      pure ()
    else
      match data.nameMap.get? catmullName with
      | some wid =>
          match data.layouts.get wid with
          | some layout =>
              let rect := layout.contentRect
              let localX := data.click.x - rect.x
              let localY := data.click.y - rect.y
              let sliderX := rect.width - 260.0 * env.screenScale
              let sliderY := 95.0 * env.screenScale
              let sliderW := 190.0 * env.screenScale
              let sliderH := 8.0 * env.screenScale
              let hitSlider := localX >= sliderX && localX <= sliderX + sliderW
                && localY >= sliderY - 8.0 && localY <= sliderY + sliderH + 8.0
              if hitSlider then
                let alpha := Linalg.Float.clamp ((localX - sliderX) / sliderW) 0.0 1.0
                stateRef.modify fun s => { s with alpha := alpha, dragging := .slider }
              else
                let origin := (rect.width / 2, rect.height / 2)
                let scale := 70.0 * env.screenScale
                let worldPos := Demos.Linalg.screenToWorld (localX, localY) origin scale
                let state ← stateRef.get
                let mut hit : Option Nat := none
                for i in [:state.points.size] do
                  let p := state.points.getD i Linalg.Vec2.zero
                  if Demos.Linalg.nearPoint worldPos p 0.45 then
                    hit := some i
                match hit with
                | some idx => stateRef.set { state with dragging := .point idx }
                | none =>
                    let newPoints := state.points.push worldPos
                    stateRef.set { state with points := newPoints, dragging := .point (newPoints.size - 1) }
          | none => pure ()
      | none => pure ()
    ) clickEvents
  performEvent_ clickAction

  let hoverEvents ← useAllHovers
  let hoverAction ← Event.mapM (fun data => do
    let state ← stateRef.get
    match state.dragging with
    | .none => pure ()
    | .slider =>
        match data.nameMap.get? catmullName with
        | some wid =>
            match data.layouts.get wid with
            | some layout =>
                let rect := layout.contentRect
                let localX := data.x - rect.x
                let sliderX := rect.width - 260.0 * env.screenScale
                let sliderW := 190.0 * env.screenScale
                let alpha := Linalg.Float.clamp ((localX - sliderX) / sliderW) 0.0 1.0
                stateRef.set { state with alpha := alpha }
            | none => pure ()
        | none => pure ()
    | .point idx =>
        match data.nameMap.get? catmullName with
        | some wid =>
            match data.layouts.get wid with
            | some layout =>
                let rect := layout.contentRect
                let localX := data.x - rect.x
                let localY := data.y - rect.y
                let origin := (rect.width / 2, rect.height / 2)
                let scale := 70.0 * env.screenScale
                let worldPos := Demos.Linalg.screenToWorld (localX, localY) origin scale
                if idx < state.points.size then
                  stateRef.set { state with points := state.points.set! idx worldPos }
                else
                  pure ()
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
          stateRef.set Demos.Linalg.catmullRomSplineEditorInitialState
      | .char 'c' =>
          stateRef.modify fun s => { s with closed := !s.closed }
      | .delete | .backspace =>
          stateRef.modify fun s =>
            if s.points.size > 0 then
              { s with points := s.points.pop }
            else s
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
        let newT := current.t + dt * 0.2
        current := { current with t := if newT > 1.0 then newT - 1.0 else newT }
        stateRef.set current
      lastTimeRef.set t
      pure current
    let containerStyle : BoxStyle := {
      flexItem := some (FlexItem.growing 1)
      width := .percent 1.0
      height := .percent 1.0
    }
    emit (pure (namedColumn catmullName 0 containerStyle #[
      Demos.Linalg.catmullRomSplineEditorWidget env state
    ]))
  pure ()

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

def arcLengthParameterizationTabContent (env : DemoEnv) (elapsedTime : Dynamic Spider Float)
    (stateRef : IO.Ref Demos.Linalg.ArcLengthParameterizationState)
    (lastTimeRef : IO.Ref Float) : WidgetM Unit := do
  let arcName ← registerComponentW "arc-length-parameterization"

  let clickEvents ← useClickData arcName
  let clickAction ← Event.mapM (fun data => do
    if data.click.button != 0 then
      pure ()
    else
      match data.nameMap.get? arcName with
      | some wid =>
          match data.layouts.get wid with
          | some layout =>
              let rect := layout.contentRect
              let localX := data.click.x - rect.x
              let localY := data.click.y - rect.y
              let sliderX := rect.width - 260.0 * env.screenScale
              let sliderY := 95.0 * env.screenScale
              let sliderW := 190.0 * env.screenScale
              let sliderH := 8.0 * env.screenScale
              let hitSlider := localX >= sliderX && localX <= sliderX + sliderW
                && localY >= sliderY - 8.0 && localY <= sliderY + sliderH + 8.0
              if hitSlider then
                let t := Linalg.Float.clamp ((localX - sliderX) / sliderW) 0.0 1.0
                let speed := 0.2 + t * 3.8
                stateRef.modify fun s => { s with speed := speed, dragging := .slider }
              else
                let origin := (rect.width / 2, rect.height / 2)
                let scale := 70.0 * env.screenScale
                let worldPos := Demos.Linalg.screenToWorld (localX, localY) origin scale
                let state ← stateRef.get
                let mut hit : Option Nat := none
                for i in [:state.controlPoints.size] do
                  let p := state.controlPoints.getD i Linalg.Vec2.zero
                  if Demos.Linalg.nearPoint worldPos p 0.45 then
                    hit := some i
                match hit with
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
    | .slider =>
        match data.nameMap.get? arcName with
        | some wid =>
            match data.layouts.get wid with
            | some layout =>
                let rect := layout.contentRect
                let localX := data.x - rect.x
                let sliderX := rect.width - 260.0 * env.screenScale
                let sliderW := 190.0 * env.screenScale
                let t := Linalg.Float.clamp ((localX - sliderX) / sliderW) 0.0 1.0
                let speed := 0.2 + t * 3.8
                stateRef.set { state with speed := speed }
            | none => pure ()
        | none => pure ()
    | .point idx =>
        match data.nameMap.get? arcName with
        | some wid =>
            match data.layouts.get wid with
            | some layout =>
                let rect := layout.contentRect
                let localX := data.x - rect.x
                let localY := data.y - rect.y
                let origin := (rect.width / 2, rect.height / 2)
                let scale := 70.0 * env.screenScale
                let worldPos := Demos.Linalg.screenToWorld (localX, localY) origin scale
                if idx < state.controlPoints.size then
                  stateRef.set { state with controlPoints := state.controlPoints.set! idx worldPos }
                else
                  pure ()
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
          stateRef.set Demos.Linalg.arcLengthParameterizationInitialState
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
        let p0 := current.controlPoints.getD 0 Linalg.Vec2.zero
        let p1 := current.controlPoints.getD 1 Linalg.Vec2.zero
        let p2 := current.controlPoints.getD 2 Linalg.Vec2.zero
        let p3 := current.controlPoints.getD 3 Linalg.Vec2.zero
        let curve := Linalg.Bezier3.mk p0 p1 p2 p3
        let evalFn := fun t => Linalg.Bezier3.evalVec2 curve t
        let table := Linalg.ArcLengthTable.build evalFn 120
        let newT := current.t + dt * 0.2
        let newS := current.s + current.speed * dt
        let wrappedS := if table.totalLength > Linalg.Float.epsilon then
          if newS > table.totalLength then newS - table.totalLength else newS
        else 0.0
        current := { current with t := (if newT > 1.0 then newT - 1.0 else newT), s := wrappedS }
        stateRef.set current
      lastTimeRef.set t
      pure current
    let containerStyle : BoxStyle := {
      flexItem := some (FlexItem.growing 1)
      width := .percent 1.0
      height := .percent 1.0
    }
    emit (pure (namedColumn arcName 0 containerStyle #[
      Demos.Linalg.arcLengthParameterizationWidget env state
    ]))
  pure ()

def bezierPatchSurfaceTabContent (env : DemoEnv) (elapsedTime : Dynamic Spider Float)
    (stateRef : IO.Ref Demos.Linalg.BezierPatchSurfaceState) : WidgetM Unit := do
  let patchName ← registerComponentW "bezier-patch-surface"

  let clickEvents ← useClickData patchName
  let clickAction ← Event.mapM (fun data => do
    match data.click.button with
    | 1 =>
        stateRef.modify fun s =>
          { s with dragging := .camera, lastMouseX := data.click.x, lastMouseY := data.click.y }
    | 0 =>
        match data.nameMap.get? patchName with
        | some wid =>
            match data.layouts.get wid with
            | some layout =>
                let rect := layout.contentRect
                let localX := data.click.x - rect.x
                let localY := data.click.y - rect.y
                let rectX := rect.width - 260.0 * env.screenScale
                let rectY := 110.0 * env.screenScale
                let rectW := 220.0 * env.screenScale
                let rectH := 220.0 * env.screenScale
                let withinMini := localX >= rectX && localX <= rectX + rectW
                  && localY >= rectY && localY <= rectY + rectH
                if withinMini then
                  let origin := (rectX + rectW / 2, rectY + rectH / 2)
                  let scale := rectW / 5.8
                  let worldPos := Demos.Linalg.screenToWorld (localX, localY) origin scale
                  let state ← stateRef.get
                  let mut hit : Option Nat := none
                  for idx in [:16] do
                    let row := idx / 4
                    let col := idx % 4
                    let p := state.patch.getPoint row col
                    let p2 := Linalg.Vec2.mk p.x p.y
                    if Demos.Linalg.nearPoint worldPos p2 0.35 then
                      hit := some idx
                  match hit with
                  | some idx => stateRef.set { state with selected := some idx, dragging := .point idx }
                  | none => pure ()
            | none => pure ()
        | none => pure ()
    | _ => pure ()
    ) clickEvents
  performEvent_ clickAction

  let hoverEvents ← useAllHovers
  let hoverAction ← Event.mapM (fun data => do
    let state ← stateRef.get
    match state.dragging with
    | .none => pure ()
    | .camera =>
        let dx := data.x - state.lastMouseX
        let dy := data.y - state.lastMouseY
        let yaw := state.cameraYaw + dx * 0.005
        let pitch := state.cameraPitch + dy * 0.005
        stateRef.set { state with cameraYaw := yaw, cameraPitch := pitch, lastMouseX := data.x, lastMouseY := data.y }
    | .point idx =>
        match data.nameMap.get? patchName with
        | some wid =>
            match data.layouts.get wid with
            | some layout =>
                let rect := layout.contentRect
                let localX := data.x - rect.x
                let localY := data.y - rect.y
                let rectX := rect.width - 260.0 * env.screenScale
                let rectY := 110.0 * env.screenScale
                let rectW := 220.0 * env.screenScale
                let rectH := 220.0 * env.screenScale
                let origin := (rectX + rectW / 2, rectY + rectH / 2)
                let scale := rectW / 5.8
                let worldPos := Demos.Linalg.screenToWorld (localX, localY) origin scale
                let row := idx / 4
                let col := idx % 4
                let p := state.patch.getPoint row col
                let patch := state.patch.setPoint row col (Linalg.Vec3.mk worldPos.x worldPos.y p.z)
                stateRef.set { state with patch := patch }
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
          stateRef.set Demos.Linalg.bezierPatchSurfaceInitialState
      | .char 'n' =>
          stateRef.modify fun s => { s with showNormals := !s.showNormals }
      | .left =>
          stateRef.modify fun s =>
            let newTess := if s.tessellation > 2 then s.tessellation - 1 else 2
            { s with tessellation := newTess }
      | .right =>
          stateRef.modify fun s =>
            let newTess := if s.tessellation < 18 then s.tessellation + 1 else 18
            { s with tessellation := newTess }
      | .up | .down =>
          stateRef.modify fun s =>
            let delta := if data.event.key == .up then 0.2 else -0.2
            match s.selected with
            | some idx =>
                let row := idx / 4
                let col := idx % 4
                let p := s.patch.getPoint row col
                let patch := s.patch.setPoint row col (Linalg.Vec3.mk p.x p.y (p.z + delta))
                { s with patch := patch }
            | none => s
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
    emit (pure (namedColumn patchName 0 containerStyle #[
      Demos.Linalg.bezierPatchSurfaceWidget env state
    ]))
  pure ()

def easingFunctionGalleryTabContent (env : DemoEnv) (elapsedTime : Dynamic Spider Float)
    (stateRef : IO.Ref Demos.Linalg.EasingFunctionGalleryState)
    (lastTimeRef : IO.Ref Float) : WidgetM Unit := do
  let easingName ← registerComponentW "easing-function-gallery"

  let keyEvents ← useKeyboard
  let keyAction ← Event.mapM (fun data => do
    if data.event.isPress then
      let count := Demos.Linalg.easingEntryCount
      if count == 0 then
        pure ()
      else
        stateRef.modify fun s =>
          let next :=
            match data.event.key with
            | .char 'r' =>
                Demos.Linalg.easingFunctionGalleryInitialState
            | .space =>
                { s with animating := !s.animating }
            | .char 'c' =>
                { s with compareMode := !s.compareMode }
            | .char 'x' =>
                { s with compare := (s.compare + 1) % count }
            | .tab =>
                if data.event.modifiers.shift then
                  { s with selected := (s.selected + count - 1) % count }
                else
                  { s with selected := (s.selected + 1) % count }
            | .left =>
                { s with selected := (s.selected + count - 1) % count }
            | .right =>
                { s with selected := (s.selected + 1) % count }
            | .up =>
                { s with speed := Linalg.Float.clamp (s.speed + 0.1) 0.1 3.0 }
            | .down =>
                { s with speed := Linalg.Float.clamp (s.speed - 0.1) 0.1 3.0 }
            | _ => s
          if next.compare == next.selected then
            { next with compare := (next.compare + 1) % count }
          else
            next
    ) keyEvents
  performEvent_ keyAction

  let _ ← dynWidget elapsedTime fun t => do
    let state ← SpiderM.liftIO do
      let lastT ← lastTimeRef.get
      let dt := if lastT == 0.0 then 0.0 else max 0.0 (t - lastT)
      let mut current ← stateRef.get
      let count := Demos.Linalg.easingEntryCount
      if current.animating && count > 0 then
        let newT := current.t + dt * current.speed
        current := { current with t := if newT > 1.0 then newT - 1.0 else newT }
      if count > 0 && current.compare == current.selected then
        current := { current with compare := (current.compare + 1) % count }
      stateRef.set current
      lastTimeRef.set t
      pure current
    let containerStyle : BoxStyle := {
      flexItem := some (FlexItem.growing 1)
      width := .percent 1.0
      height := .percent 1.0
    }
    emit (pure (namedColumn easingName 0 containerStyle #[
      Demos.Linalg.easingFunctionGalleryWidget env state
    ]))
  pure ()

end Demos
