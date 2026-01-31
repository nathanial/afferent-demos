/-
  Demo Registry - Linalg demo instances.
-/
import Demos.Core.DemoRegistry.Types

open Afferent

namespace Demos

private def arrayGet? (arr : Array α) (i : Nat) : Option α :=
  if h : i < arr.size then
    some (arr[i]'h)
  else
    none


-- ═══════════════════════════════════════════════════════════════════════════
-- Linalg Vector Demos
-- ═══════════════════════════════════════════════════════════════════════════

instance : Demo .vectorInterpolation where
  name := "VECTOR INTERPOLATION (space to toggle animation)"
  shortName := "Lerp"
  init := fun _ => pure Linalg.vectorInterpolationInitialState
  update := fun env s => do
    let mut state := s
    -- Handle keyboard: space toggles animation
    if env.keyCode == FFI.Key.space then
      env.clearKey
      state := { state with animating := !state.animating }
    -- Update animation
    if state.animating then
      let newT := (state.t + env.dt * 0.5)
      state := { state with t := if newT >= 1.0 then newT - 1.0 else newT }
    pure state
  view := fun env s => some (Linalg.vectorInterpolationWidget env s)
  handleClickWithLayouts := fun env state _contentId _hitPath click _layouts _widget => do
    if click.button != 0 then return state
    -- Calculate world position from click using content area dimensions
    let origin := (env.contentOffsetX + env.physWidthF / 2, env.contentOffsetY + env.physHeightF / 2)
    let scale := 50.0 * env.screenScale
    let worldPos := Linalg.screenToWorld (click.x, click.y) origin scale
    -- Check if near any draggable point
    if Linalg.nearPoint worldPos state.vectorA 0.5 then
      pure { state with dragging := some .vectorA }
    else if Linalg.nearPoint worldPos state.vectorB 0.5 then
      pure { state with dragging := some .vectorB }
    else
      pure state
  handleHoverWithLayouts := fun env state _contentId _hitPath mouseX mouseY _layouts _widget => do
    match state.dragging with
    | some target =>
        let origin := (env.contentOffsetX + env.physWidthF / 2, env.contentOffsetY + env.physHeightF / 2)
        let scale := 50.0 * env.screenScale
        let worldPos := Linalg.screenToWorld (mouseX, mouseY) origin scale
        match target with
        | .vectorA => pure { state with vectorA := worldPos }
        | .vectorB => pure { state with vectorB := worldPos }
    | none => pure state
  handleMouseUpWithLayouts := fun _env state _ _ _ _ _ => do
    pure { state with dragging := none }
  step := fun c _ s => pure (c, s)

instance : Demo .vectorArithmetic where
  name := "VECTOR ARITHMETIC (1/2/3 to switch ops)"
  shortName := "Arithmetic"
  init := fun _ => pure Linalg.vectorArithmeticInitialState
  update := fun env s => do
    let mut state := s
    -- Handle keyboard: 1=add, 2=sub, 3=scale
    if env.keyCode == FFI.Key.num1 then
      env.clearKey
      state := { state with operation := .add }
    else if env.keyCode == FFI.Key.num2 then
      env.clearKey
      state := { state with operation := .sub }
    else if env.keyCode == FFI.Key.num3 then
      env.clearKey
      state := { state with operation := .scale }
    -- +/- to adjust scale factor
    else if env.keyCode == 24 then  -- = (plus without shift)
      env.clearKey
      state := { state with scaleFactor := state.scaleFactor + 0.1 }
    else if env.keyCode == 27 then  -- - (minus)
      env.clearKey
      let newScale := if state.scaleFactor > 0.2 then state.scaleFactor - 0.1 else 0.1
      state := { state with scaleFactor := newScale }
    pure state
  view := fun env s => some (Linalg.vectorArithmeticWidget env s)
  handleClickWithLayouts := fun env state _contentId _hitPath click _layouts _widget => do
    if click.button != 0 then return state
    let origin := (env.contentOffsetX + env.physWidthF / 2, env.contentOffsetY + env.physHeightF / 2)
    let scale := 50.0 * env.screenScale
    let worldPos := Linalg.screenToWorld (click.x, click.y) origin scale
    if Linalg.nearPoint worldPos state.vectorA 0.5 then
      pure { state with dragging := some .vectorA }
    else if Linalg.nearPoint worldPos state.vectorB 0.5 then
      pure { state with dragging := some .vectorB }
    else
      pure state
  handleHoverWithLayouts := fun env state _contentId _hitPath mouseX mouseY _layouts _widget => do
    match state.dragging with
    | some target =>
        let origin := (env.contentOffsetX + env.physWidthF / 2, env.contentOffsetY + env.physHeightF / 2)
        let scale := 50.0 * env.screenScale
        let worldPos := Linalg.screenToWorld (mouseX, mouseY) origin scale
        match target with
        | .vectorA => pure { state with vectorA := worldPos }
        | .vectorB => pure { state with vectorB := worldPos }
    | none => pure state
  handleMouseUpWithLayouts := fun _env state _ _ _ _ _ => do
    pure { state with dragging := none }
  step := fun c _ s => pure (c, s)

instance : Demo .vectorProjection where
  name := "VECTOR PROJECTION (P/R/B to switch modes)"
  shortName := "Projection"
  init := fun _ => pure Linalg.vectorProjectionInitialState
  update := fun env s => do
    let mut state := s
    -- Handle keyboard: P=projection, R=reflection, B=both
    if env.keyCode == FFI.Key.p then
      env.clearKey
      state := { state with showMode := .projection }
    else if env.keyCode == FFI.Key.r then
      env.clearKey
      state := { state with showMode := .reflection }
    else if env.keyCode == FFI.Key.b then
      env.clearKey
      state := { state with showMode := .both }
    pure state
  view := fun env s => some (Linalg.vectorProjectionWidget env s)
  handleClickWithLayouts := fun env state _contentId _hitPath click _layouts _widget => do
    if click.button != 0 then return state
    let origin := (env.contentOffsetX + env.physWidthF / 2, env.contentOffsetY + env.physHeightF / 2)
    let scale := 50.0 * env.screenScale
    let worldPos := Linalg.screenToWorld (click.x, click.y) origin scale
    if Linalg.nearPoint worldPos state.vectorV 0.5 then
      pure { state with dragging := some .vectorV }
    else if Linalg.nearPoint worldPos state.vectorU 0.5 then
      pure { state with dragging := some .vectorU }
    else
      pure state
  handleHoverWithLayouts := fun env state _contentId _hitPath mouseX mouseY _layouts _widget => do
    match state.dragging with
    | some target =>
        let origin := (env.contentOffsetX + env.physWidthF / 2, env.contentOffsetY + env.physHeightF / 2)
        let scale := 50.0 * env.screenScale
        let worldPos := Linalg.screenToWorld (mouseX, mouseY) origin scale
        match target with
        | .vectorV => pure { state with vectorV := worldPos }
        | .vectorU => pure { state with vectorU := worldPos }
    | none => pure state
  handleMouseUpWithLayouts := fun _env state _ _ _ _ _ => do
    pure { state with dragging := none }
  step := fun c _ s => pure (c, s)

instance : Demo .vectorField where
  name := "VECTOR FIELD (1-4 to switch fields)"
  shortName := "Field"
  init := fun _ => pure Linalg.vectorFieldInitialState
  update := fun env s => do
    let mut state := s
    -- Handle keyboard: 1-4 for field types
    if env.keyCode == FFI.Key.num1 then
      env.clearKey
      state := { state with fieldType := .radial }
    else if env.keyCode == FFI.Key.num2 then
      env.clearKey
      state := { state with fieldType := .rotational }
    else if env.keyCode == FFI.Key.num3 then
      env.clearKey
      state := { state with fieldType := .gradient }
    else if env.keyCode == FFI.Key.num4 then
      env.clearKey
      state := { state with fieldType := .saddle }
    -- +/- to adjust grid resolution
    else if env.keyCode == 24 then  -- = (plus)
      env.clearKey
      state := { state with gridResolution := Nat.min 24 (state.gridResolution + 2) }
    else if env.keyCode == 27 then  -- - (minus)
      env.clearKey
      state := { state with gridResolution := Nat.max 4 (state.gridResolution - 2) }
    pure state
  view := fun env s => some (Linalg.vectorFieldWidget env s)
  step := fun c _ s => pure (c, s)

instance : Demo .crossProduct3D where
  name := "3D CROSS PRODUCT (drag to rotate)"
  shortName := "Cross 3D"
  init := fun _ => pure Linalg.crossProduct3DInitialState
  update := fun env s => do
    let mut state := s
    -- Handle keyboard: P=toggle parallelogram, R=reset camera
    if env.keyCode == FFI.Key.p then
      env.clearKey
      state := { state with showParallelogram := !state.showParallelogram }
    else if env.keyCode == FFI.Key.r then
      env.clearKey
      state := { state with cameraYaw := 0.6, cameraPitch := 0.4 }
    pure state
  view := fun env s => some (Linalg.crossProduct3DWidget env s)
  handleClickWithLayouts := fun _env state _contentId _hitPath click _layouts _widget => do
    if click.button != 0 then return state
    pure { state with dragging := .camera, lastMouseX := click.x, lastMouseY := click.y }
  handleHoverWithLayouts := fun _env state _contentId _hitPath mouseX mouseY _layouts _widget => do
    match state.dragging with
    | .camera =>
        let dx := mouseX - state.lastMouseX
        let dy := mouseY - state.lastMouseY
        let newYaw := state.cameraYaw + dx * 0.01
        let rawPitch := state.cameraPitch + dy * 0.01
        let newPitch := if rawPitch < -1.5 then -1.5 else if rawPitch > 1.5 then 1.5 else rawPitch
        pure { state with cameraYaw := newYaw, cameraPitch := newPitch, lastMouseX := mouseX, lastMouseY := mouseY }
    | _ => pure state
  handleMouseUpWithLayouts := fun _env state _ _ _ _ _ => do
    pure { state with dragging := .none }
  step := fun c _ s => pure (c, s)

-- ═══════════════════════════════════════════════════════════════════════════
-- Linalg Matrix Demos
-- ═══════════════════════════════════════════════════════════════════════════

instance : Demo .matrix2DTransform where
  name := "2D MATRIX TRANSFORM (1-9 for presets)"
  shortName := "Mat2D"
  init := fun _ => pure Linalg.matrix2DTransformInitialState
  update := fun env s => do
    let mut state := s
    -- Handle keyboard: 1-9 for presets
    if env.keyCode == FFI.Key.num1 then
      env.clearKey
      state := { state with preset := .identity, matrix := Linalg.presetToMatrix .identity }
    else if env.keyCode == FFI.Key.num2 then
      env.clearKey
      state := { state with preset := .rotation45, matrix := Linalg.presetToMatrix .rotation45 }
    else if env.keyCode == FFI.Key.num3 then
      env.clearKey
      state := { state with preset := .rotation90, matrix := Linalg.presetToMatrix .rotation90 }
    else if env.keyCode == FFI.Key.num4 then
      env.clearKey
      state := { state with preset := .scale2x, matrix := Linalg.presetToMatrix .scale2x }
    else if env.keyCode == FFI.Key.num5 then
      env.clearKey
      state := { state with preset := .scaleNonUniform, matrix := Linalg.presetToMatrix .scaleNonUniform }
    else if env.keyCode == FFI.Key.num6 then
      env.clearKey
      state := { state with preset := .shearX, matrix := Linalg.presetToMatrix .shearX }
    else if env.keyCode == FFI.Key.num7 then
      env.clearKey
      state := { state with preset := .shearY, matrix := Linalg.presetToMatrix .shearY }
    else if env.keyCode == FFI.Key.num8 then
      env.clearKey
      state := { state with preset := .reflectX, matrix := Linalg.presetToMatrix .reflectX }
    else if env.keyCode == FFI.Key.num9 then
      env.clearKey
      state := { state with preset := .reflectY, matrix := Linalg.presetToMatrix .reflectY }
    -- Tab to cycle editable matrix cell
    else if env.keyCode == FFI.Key.tab then
      env.clearKey
      state := { state with editingCell := Linalg.nextMatrixCell state.editingCell }
    -- Arrow keys to move selected cell
    else if env.keyCode == FFI.Key.left then
      env.clearKey
      state := { state with editingCell := Linalg.moveMatrixCell state.editingCell 0 (-1) }
    else if env.keyCode == FFI.Key.right then
      env.clearKey
      state := { state with editingCell := Linalg.moveMatrixCell state.editingCell 0 1 }
    else if env.keyCode == FFI.Key.up then
      env.clearKey
      state := { state with editingCell := Linalg.moveMatrixCell state.editingCell (-1) 0 }
    else if env.keyCode == FFI.Key.down then
      env.clearKey
      state := { state with editingCell := Linalg.moveMatrixCell state.editingCell 1 0 }
    -- +/- to edit selected cell
    else if env.keyCode == 24 then  -- = (plus)
      env.clearKey
      if state.editingCell != .none then
        let updated := Linalg.modifyMatrixCell state.matrix state.editingCell 0.1
        state := { state with matrix := updated, preset := .custom }
    else if env.keyCode == 27 then  -- - (minus)
      env.clearKey
      if state.editingCell != .none then
        let updated := Linalg.modifyMatrixCell state.matrix state.editingCell (-0.1)
        state := { state with matrix := updated, preset := .custom }
    -- IJKL to translate
    else if env.keyCode == FFI.Key.i then
      env.clearKey
      let t := state.translation
      state := { state with translation := Linalg.Vec2.mk t.x (t.y + 0.1), preset := .custom }
    else if env.keyCode == FFI.Key.k then
      env.clearKey
      let t := state.translation
      state := { state with translation := Linalg.Vec2.mk t.x (t.y - 0.1), preset := .custom }
    else if env.keyCode == FFI.Key.j then
      env.clearKey
      let t := state.translation
      state := { state with translation := Linalg.Vec2.mk (t.x - 0.1) t.y, preset := .custom }
    else if env.keyCode == FFI.Key.l then
      env.clearKey
      let t := state.translation
      state := { state with translation := Linalg.Vec2.mk (t.x + 0.1) t.y, preset := .custom }
    -- G to toggle grid
    else if env.keyCode == FFI.Key.g then
      env.clearKey
      state := { state with showGrid := !state.showGrid }
    -- V to toggle basis vectors
    else if env.keyCode == FFI.Key.v then
      env.clearKey
      state := { state with showBasisVectors := !state.showBasisVectors }
    -- S to cycle shapes
    else if env.keyCode == FFI.Key.s then
      env.clearKey
      let newShape := match state.shape with
        | .square => .triangle
        | .triangle => .arrow
        | .arrow => .square
      state := { state with shape := newShape }
    -- Space to toggle animation
    else if env.keyCode == FFI.Key.space then
      env.clearKey
      state := { state with animating := !state.animating }
    -- Update animation
    if state.animating then
      let newT := state.animT + env.dt * 0.5
      state := { state with animT := if newT >= 1.0 then 0.0 else newT }
    pure state
  view := fun env s => some (Linalg.matrix2DTransformWidget env s)
  step := fun c _ s => pure (c, s)

instance : Demo .matrix3DTransform where
  name := "3D TRANSFORM CHAIN (reorder transforms)"
  shortName := "Mat3D"
  init := fun _ => pure Linalg.matrix3DTransformInitialState
  update := fun env s => do
    let mut state := s
    -- 1-8 to select transform
    if env.keyCode == FFI.Key.num1 then
      env.clearKey
      state := { state with selectedIndex := some 0 }
    else if env.keyCode == FFI.Key.num2 then
      env.clearKey
      state := { state with selectedIndex := some 1 }
    else if env.keyCode == FFI.Key.num3 then
      env.clearKey
      state := { state with selectedIndex := some 2 }
    -- Up arrow to move selected transform up
    else if env.keyCode == FFI.Key.up then
      env.clearKey
      if let some idx := state.selectedIndex then
        if idx > 0 then
          let mut arr := state.transforms
          let temp := arr.getD idx (.rotateX 0)
          arr := arr.set! idx (arr.getD (idx - 1) (.rotateX 0))
          arr := arr.set! (idx - 1) temp
          state := { state with transforms := arr, selectedIndex := some (idx - 1) }
    -- Down arrow to move selected transform down
    else if env.keyCode == FFI.Key.down then
      env.clearKey
      if let some idx := state.selectedIndex then
        if idx + 1 < state.transforms.size then
          let mut arr := state.transforms
          let temp := arr.getD idx (.rotateX 0)
          arr := arr.set! idx (arr.getD (idx + 1) (.rotateX 0))
          arr := arr.set! (idx + 1) temp
          state := { state with transforms := arr, selectedIndex := some (idx + 1) }
    -- A to toggle axes
    else if env.keyCode == FFI.Key.a then
      env.clearKey
      state := { state with showAxes := !state.showAxes }
    -- I to toggle intermediate steps
    else if env.keyCode == FFI.Key.i then
      env.clearKey
      state := { state with showIntermediateSteps := !state.showIntermediateSteps }
    -- R to reset camera
    else if env.keyCode == FFI.Key.r then
      env.clearKey
      state := { state with cameraYaw := 0.5, cameraPitch := 0.3 }
    pure state
  view := fun env s => some (Linalg.matrix3DTransformWidget env s)
  handleClickWithLayouts := fun _env state _contentId _hitPath click _layouts _widget => do
    if click.button != 0 then return state
    pure { state with dragging := true, lastMouseX := click.x, lastMouseY := click.y }
  handleHoverWithLayouts := fun _env state _contentId _hitPath mouseX mouseY _layouts _widget => do
    if state.dragging then
      let dx := mouseX - state.lastMouseX
      let dy := mouseY - state.lastMouseY
      let newYaw := state.cameraYaw + dx * 0.01
      let rawPitch := state.cameraPitch + dy * 0.01
      let newPitch := if rawPitch < -1.5 then -1.5 else if rawPitch > 1.5 then 1.5 else rawPitch
      pure { state with cameraYaw := newYaw, cameraPitch := newPitch, lastMouseX := mouseX, lastMouseY := mouseY }
    else
      pure state
  handleMouseUpWithLayouts := fun _env state _ _ _ _ _ => do
    pure { state with dragging := false }
  step := fun c _ s => pure (c, s)

instance : Demo .projectionExplorer where
  name := "PROJECTION MATRIX EXPLORER"
  shortName := "Proj"
  init := fun _ => pure Linalg.projectionExplorerInitialState
  update := fun env s => do
    let mut state := s
    -- Tab to switch projection type
    if env.keyCode == FFI.Key.tab then
      env.clearKey
      state := { state with projType := match state.projType with
        | .perspective => .orthographic
        | .orthographic => .perspective
      }
    -- F to adjust far plane
    else if env.keyCode == FFI.Key.f then
      env.clearKey
      let newFar := state.far + 0.5
      state := { state with far := if newFar > 10.0 then 10.0 else newFar }
    -- N to adjust near plane
    else if env.keyCode == FFI.Key.n then
      env.clearKey
      let newNear := state.near + 0.1
      let maxNear := state.far - 0.5
      let clampedNear := if newNear > maxNear then maxNear else newNear
      state := { state with near := if clampedNear < 0.1 then 0.1 else clampedNear }
    -- + to increase FOV/size
    else if env.keyCode == 24 then  -- = (plus)
      env.clearKey
      match state.projType with
      | .perspective =>
          let newFov := state.fov + 0.1
          let maxFov := 2.513  -- ~80% of pi
          state := { state with fov := if newFov > maxFov then maxFov else newFov }
      | .orthographic =>
          let newSize := state.orthoSize + 0.2
          state := { state with orthoSize := if newSize > 5.0 then 5.0 else newSize }
    -- - to decrease FOV/size
    else if env.keyCode == 27 then  -- -
      env.clearKey
      match state.projType with
      | .perspective =>
          let newFov := state.fov - 0.1
          state := { state with fov := if newFov < 0.3 then 0.3 else newFov }
      | .orthographic =>
          let newSize := state.orthoSize - 0.2
          state := { state with orthoSize := if newSize < 0.5 then 0.5 else newSize }
    -- C to toggle clip space view
    else if env.keyCode == FFI.Key.c then
      env.clearKey
      state := { state with showClipSpace := !state.showClipSpace }
    -- O to toggle test objects
    else if env.keyCode == FFI.Key.o then
      env.clearKey
      state := { state with showTestObjects := !state.showTestObjects }
    pure state
  view := fun env s => some (Linalg.projectionExplorerWidget env s)
  handleClickWithLayouts := fun _env state _contentId _hitPath click _layouts _widget => do
    if click.button != 0 then return state
    pure { state with dragging := true, lastMouseX := click.x, lastMouseY := click.y }
  handleHoverWithLayouts := fun _env state _contentId _hitPath mouseX mouseY _layouts _widget => do
    if state.dragging then
      let dx := mouseX - state.lastMouseX
      let dy := mouseY - state.lastMouseY
      let newYaw := state.cameraYaw + dx * 0.01
      let rawPitch := state.cameraPitch + dy * 0.01
      let newPitch := if rawPitch < -1.5 then -1.5 else if rawPitch > 1.5 then 1.5 else rawPitch
      pure { state with cameraYaw := newYaw, cameraPitch := newPitch, lastMouseX := mouseX, lastMouseY := mouseY }
    else
      pure state
  handleMouseUpWithLayouts := fun _env state _ _ _ _ _ => do
    pure { state with dragging := false }
  step := fun c _ s => pure (c, s)

instance : Demo .matrixDecomposition where
  name := "MATRIX DECOMPOSITION"
  shortName := "Decomp"
  init := fun _ => pure Linalg.matrixDecompositionInitialState
  update := fun env s => do
    let mut state := s
    -- 1-6 for presets
    let presetKey := if env.keyCode == FFI.Key.num1 then some 0
                     else if env.keyCode == FFI.Key.num2 then some 1
                     else if env.keyCode == FFI.Key.num3 then some 2
                     else if env.keyCode == FFI.Key.num4 then some 3
                     else if env.keyCode == FFI.Key.num5 then some 4
                     else if env.keyCode == FFI.Key.num6 then some 5
                     else none
    if let some i := presetKey then
      if i < Linalg.decompositionPresets.size then
        env.clearKey
        let (_, m) := Linalg.decompositionPresets.getD i ("", Linalg.Mat2.identity)
        state := { state with
          matrix := m
          decomp := Linalg.decomposeMatrix2D m
          presetIndex := i
        }
    -- Tab to cycle decomposition steps
    if env.keyCode == FFI.Key.tab then
      env.clearKey
      state := { state with currentStep := match state.currentStep with
        | .original => .afterRotation1
        | .afterRotation1 => .afterScale
        | .afterScale => .afterRotation2
        | .afterRotation2 => .original
      }
    -- C to toggle components
    else if env.keyCode == FFI.Key.c then
      env.clearKey
      state := { state with showComponents := !state.showComponents }
    pure state
  view := fun env s => some (Linalg.matrixDecompositionWidget env s)
  step := fun c _ s => pure (c, s)

-- ═══════════════════════════════════════════════════════════════════════════
-- Linalg Rotation System Demos
-- ═══════════════════════════════════════════════════════════════════════════

instance : Demo .quaternionVisualizer where
  name := "QUATERNION VISUALIZER (arcball + sliders)"
  shortName := "Quat"
  init := fun _ => pure Linalg.quaternionVisualizerInitialState
  update := fun env s => do
    let mut state := s
    -- Reset
    if env.keyCode == FFI.Key.r then
      env.clearKey
      state := Linalg.quaternionVisualizerInitialState
    -- Apply Euler angles to quaternion
    else if env.keyCode == FFI.Key.e then
      env.clearKey
      let e := state.eulerAngles
      let q := Linalg.Quat.fromEuler e.x e.y e.z
      state := { state with quat := q.normalize }
    -- Select Euler component
    else if env.keyCode == FFI.Key.num1 then
      env.clearKey
      state := { state with selectedEuler := 0 }
    else if env.keyCode == FFI.Key.num2 then
      env.clearKey
      state := { state with selectedEuler := 1 }
    else if env.keyCode == FFI.Key.num3 then
      env.clearKey
      state := { state with selectedEuler := 2 }
    -- Adjust Euler angles
    else if env.keyCode == FFI.Key.left then
      env.clearKey
      let delta := -5.0 * Linalg.Float.pi / 180.0
      let e := state.eulerAngles
      let e' := match state.selectedEuler with
        | 0 => Linalg.Vec3.mk (e.x + delta) e.y e.z
        | 1 => Linalg.Vec3.mk e.x (e.y + delta) e.z
        | _ => Linalg.Vec3.mk e.x e.y (e.z + delta)
      state := { state with eulerAngles := e' }
    else if env.keyCode == FFI.Key.right then
      env.clearKey
      let delta := 5.0 * Linalg.Float.pi / 180.0
      let e := state.eulerAngles
      let e' := match state.selectedEuler with
        | 0 => Linalg.Vec3.mk (e.x + delta) e.y e.z
        | 1 => Linalg.Vec3.mk e.x (e.y + delta) e.z
        | _ => Linalg.Vec3.mk e.x e.y (e.z + delta)
      state := { state with eulerAngles := e' }
    pure state
  view := fun env s => some (Linalg.quaternionVisualizerWidget env s)
  handleClickWithLayouts := fun env state _contentId _hitPath click _layouts _widget => do
    if click.button != 0 && click.button != 1 then
      return state
    let localX := click.x - env.contentOffsetX
    let localY := click.y - env.contentOffsetY
    let labels : Array (Linalg.QuatComponent × Nat) := #[(.x, 0), (.y, 1), (.z, 2), (.w, 3)]
    let mut hitSlider : Option Linalg.QuatComponent := none
    for (comp, idx) in labels do
      let layout := Linalg.sliderLayoutFor env.physWidthF env.physHeightF env.screenScale idx
      let hit := localX >= layout.x && localX <= layout.x + layout.width
        && localY >= layout.y - 8.0 && localY <= layout.y + layout.height + 8.0
      if hit then
        hitSlider := some comp
    match hitSlider with
    | some comp =>
        let layout := Linalg.sliderLayoutFor env.physWidthF env.physHeightF env.screenScale
          (match comp with | .x => 0 | .y => 1 | .z => 2 | .w => 3)
        let t := (localX - layout.x) / layout.width
        let value := Linalg.clampUnit (t * 2.0 - 1.0)
        let q := (Linalg.setQuatComponent state.quat comp value).normalize
        pure { state with quat := q, dragging := .slider comp, lastMouseX := click.x, lastMouseY := click.y }
    | none =>
        if click.button == 1 then
          pure { state with dragging := .camera, lastMouseX := click.x, lastMouseY := click.y }
        else
          pure { state with dragging := .arcball, lastMouseX := click.x, lastMouseY := click.y }
  handleHoverWithLayouts := fun env state _contentId _hitPath mouseX mouseY _layouts _widget => do
    match state.dragging with
    | .none => pure state
    | .camera =>
        let dx := mouseX - state.lastMouseX
        let dy := mouseY - state.lastMouseY
        let yaw := state.cameraYaw + dx * 0.005
        let pitch := state.cameraPitch + dy * 0.005
        pure { state with cameraYaw := yaw, cameraPitch := pitch, lastMouseX := mouseX, lastMouseY := mouseY }
    | .arcball =>
        let dx := mouseX - state.lastMouseX
        let dy := mouseY - state.lastMouseY
        let rotY := Linalg.Quat.fromAxisAngle Linalg.Vec3.unitY (dx * 0.008)
        let rotX := Linalg.Quat.fromAxisAngle Linalg.Vec3.unitX (dy * 0.008)
        let q := Linalg.Quat.multiply rotY (Linalg.Quat.multiply rotX state.quat) |>.normalize
        pure { state with quat := q, lastMouseX := mouseX, lastMouseY := mouseY }
    | .slider comp =>
        let localX := mouseX - env.contentOffsetX
        let layout := Linalg.sliderLayoutFor env.physWidthF env.physHeightF env.screenScale
          (match comp with | .x => 0 | .y => 1 | .z => 2 | .w => 3)
        let t := (localX - layout.x) / layout.width
        let value := Linalg.clampUnit (t * 2.0 - 1.0)
        let q := (Linalg.setQuatComponent state.quat comp value).normalize
        pure { state with quat := q, lastMouseX := mouseX, lastMouseY := mouseY }
  handleMouseUpWithLayouts := fun _env state _ _ _ _ _ =>
    pure { state with dragging := .none }
  step := fun c _ s => pure (c, s)

instance : Demo .slerpInterpolation where
  name := "SLERP vs LERP (quaternion)"
  shortName := "Slerp"
  init := fun _ => pure Linalg.slerpInterpolationInitialState
  update := fun env s => do
    let mut state := s
    if env.keyCode == FFI.Key.space then
      env.clearKey
      state := { state with animating := !state.animating }
    if state.animating then
      let newT := state.t + env.dt * 0.35
      state := { state with t := if newT > 1.0 then newT - 1.0 else newT }
    pure state
  view := fun env s => some (Linalg.slerpInterpolationWidget env s)
  handleClickWithLayouts := fun _env state _ _ click _ _ => do
    if click.button != 0 then return state
    pure { state with dragging := true, lastMouseX := click.x, lastMouseY := click.y }
  handleHoverWithLayouts := fun _env state _ _ mouseX mouseY _ _ => do
    if !state.dragging then return state
    let dx := mouseX - state.lastMouseX
    let dy := mouseY - state.lastMouseY
    let newYaw := state.cameraYaw + dx * 0.005
    let newPitch := state.cameraPitch + dy * 0.005
    pure { state with cameraYaw := newYaw, cameraPitch := newPitch, lastMouseX := mouseX, lastMouseY := mouseY }
  handleMouseUpWithLayouts := fun _env state _ _ _ _ _ =>
    pure { state with dragging := false }
  step := fun c _ s => pure (c, s)

instance : Demo .eulerGimbalLock where
  name := "EULER GIMBAL LOCK"
  shortName := "Gimbal"
  init := fun _ => pure Linalg.eulerGimbalLockInitialState
  update := fun env s => do
    let mut state := s
    if env.keyCode == FFI.Key.r then
      env.clearKey
      state := Linalg.eulerGimbalLockInitialState
    else if env.keyCode == FFI.Key.num1 then
      env.clearKey
      state := { state with selectedAxis := 0 }
    else if env.keyCode == FFI.Key.num2 then
      env.clearKey
      state := { state with selectedAxis := 1 }
    else if env.keyCode == FFI.Key.num3 then
      env.clearKey
      state := { state with selectedAxis := 2 }
    else if env.keyCode == FFI.Key.o then
      env.clearKey
      let nextOrder := match state.euler.order with
        | .XYZ => .XZY
        | .XZY => .YXZ
        | .YXZ => .YZX
        | .YZX => .ZXY
        | .ZXY => .ZYX
        | .ZYX => .XYZ
      state := { state with euler := { state.euler with order := nextOrder } }
    else if env.keyCode == FFI.Key.left || env.keyCode == FFI.Key.right then
      let delta := if env.keyCode == FFI.Key.left then -5.0 else 5.0
      env.clearKey
      let e := state.euler
      let radDelta := delta * Linalg.Float.pi / 180.0
      let e' := match state.selectedAxis with
        | 0 => { e with a1 := e.a1 + radDelta }
        | 1 => { e with a2 := e.a2 + radDelta }
        | _ => { e with a3 := e.a3 + radDelta }
      state := { state with euler := e' }
    pure state
  view := fun env s => some (Linalg.eulerGimbalLockWidget env s)
  handleClickWithLayouts := fun _env state _ _ click _ _ => do
    if click.button != 0 then return state
    pure { state with dragging := true, lastMouseX := click.x, lastMouseY := click.y }
  handleHoverWithLayouts := fun _env state _ _ mouseX mouseY _ _ => do
    if !state.dragging then return state
    let dx := mouseX - state.lastMouseX
    let dy := mouseY - state.lastMouseY
    let newYaw := state.cameraYaw + dx * 0.005
    let newPitch := state.cameraPitch + dy * 0.005
    pure { state with cameraYaw := newYaw, cameraPitch := newPitch, lastMouseX := mouseX, lastMouseY := mouseY }
  handleMouseUpWithLayouts := fun _env state _ _ _ _ _ =>
    pure { state with dragging := false }
  step := fun c _ s => pure (c, s)

instance : Demo .dualQuaternionBlending where
  name := "DUAL QUATERNION BLENDING (LBS vs DLB)"
  shortName := "DualQuat"
  init := fun _ => pure Linalg.dualQuaternionBlendingInitialState
  update := fun env s => do
    let mut state := s
    if env.keyCode == FFI.Key.r then
      env.clearKey
      state := Linalg.dualQuaternionBlendingInitialState
    else if env.keyCode == FFI.Key.t then
      env.clearKey
      state := { state with twist := state.twist + 0.1 }
    else if env.keyCode == FFI.Key.g then
      env.clearKey
      state := { state with twist := state.twist - 0.1 }
    else if env.keyCode == FFI.Key.b then
      env.clearKey
      state := { state with bend := state.bend + 0.1 }
    else if env.keyCode == FFI.Key.v then
      env.clearKey
      state := { state with bend := state.bend - 0.1 }
    pure state
  view := fun env s => some (Linalg.dualQuaternionBlendingWidget env s)
  handleClickWithLayouts := fun _env state _ _ click _ _ => do
    if click.button != 0 then return state
    pure { state with dragging := true, lastMouseX := click.x, lastMouseY := click.y }
  handleHoverWithLayouts := fun _env state _ _ mouseX mouseY _ _ => do
    if !state.dragging then return state
    let dx := mouseX - state.lastMouseX
    let dy := mouseY - state.lastMouseY
    let newYaw := state.cameraYaw + dx * 0.005
    let newPitch := state.cameraPitch + dy * 0.005
    pure { state with cameraYaw := newYaw, cameraPitch := newPitch, lastMouseX := mouseX, lastMouseY := mouseY }
  handleMouseUpWithLayouts := fun _env state _ _ _ _ _ =>
    pure { state with dragging := false }
  step := fun c _ s => pure (c, s)


-- ═══════════════════════════════════════════════════════════════════════════
-- Linalg Geometry Demos
-- ═══════════════════════════════════════════════════════════════════════════

instance : Demo .rayCastingPlayground where
  name := "RAY CASTING PLAYGROUND"
  shortName := "Ray"
  init := fun _ => pure Linalg.rayCastingPlaygroundInitialState
  update := fun env s => do
    let mut state := s
    if env.keyCode == FFI.Key.r then
      env.clearKey
      state := Linalg.rayCastingPlaygroundInitialState
    pure state
  view := fun env s => some (Linalg.rayCastingPlaygroundWidget env s)
  handleClickWithLayouts := fun env state _contentId _hitPath click _layouts _widget => do
    if click.button != 0 && click.button != 1 then
      return state
    let origin := (env.contentOffsetX + env.physWidthF / 2, env.contentOffsetY + env.physHeightF / 2)
    let scale := 70.0 * env.screenScale
    let worldPos := Linalg.screenToWorld (click.x, click.y) origin scale
    let origin2 := Linalg.Vec2.mk state.rayOrigin.x state.rayOrigin.z
    let target2 := Linalg.Vec2.mk state.rayTarget.x state.rayTarget.z
    if click.button == 1 then
      pure { state with dragging := .camera, lastMouseX := click.x, lastMouseY := click.y }
    else if Linalg.nearPoint worldPos origin2 0.5 then
      pure { state with dragging := .origin, lastMouseX := click.x, lastMouseY := click.y }
    else if Linalg.nearPoint worldPos target2 0.5 then
      pure { state with dragging := .direction, lastMouseX := click.x, lastMouseY := click.y }
    else
      pure state
  handleHoverWithLayouts := fun env state _contentId _hitPath mouseX mouseY _layouts _widget => do
    match state.dragging with
    | .none => pure state
    | .camera =>
        let dx := mouseX - state.lastMouseX
        let dy := mouseY - state.lastMouseY
        let newYaw := state.cameraYaw + dx * 0.005
        let newPitch := state.cameraPitch + dy * 0.005
        pure { state with cameraYaw := newYaw, cameraPitch := newPitch, lastMouseX := mouseX, lastMouseY := mouseY }
    | .origin =>
        let origin := (env.contentOffsetX + env.physWidthF / 2, env.contentOffsetY + env.physHeightF / 2)
        let scale := 70.0 * env.screenScale
        let worldPos := Linalg.screenToWorld (mouseX, mouseY) origin scale
        let newOrigin := Linalg.Vec3.mk worldPos.x 0.0 worldPos.y
        pure { state with rayOrigin := newOrigin, lastMouseX := mouseX, lastMouseY := mouseY }
    | .direction =>
        let origin := (env.contentOffsetX + env.physWidthF / 2, env.contentOffsetY + env.physHeightF / 2)
        let scale := 70.0 * env.screenScale
        let worldPos := Linalg.screenToWorld (mouseX, mouseY) origin scale
        let newTarget := Linalg.Vec3.mk worldPos.x 0.0 worldPos.y
        pure { state with rayTarget := newTarget, lastMouseX := mouseX, lastMouseY := mouseY }
  handleMouseUpWithLayouts := fun _env state _ _ _ _ _ =>
    pure { state with dragging := .none }
  step := fun c _ s => pure (c, s)

instance : Demo .primitiveOverlapTester where
  name := "PRIMITIVE OVERLAP TESTER"
  shortName := "Overlap"
  init := fun _ => pure Linalg.primitiveOverlapTesterInitialState
  update := fun env s => do
    let mut state := s
    if env.keyCode == FFI.Key.r then
      env.clearKey
      state := Linalg.primitiveOverlapTesterInitialState
    else if env.keyCode == FFI.Key.num1 then
      env.clearKey
      state := { state with mode := .sphereSphere }
    else if env.keyCode == FFI.Key.num2 then
      env.clearKey
      state := { state with mode := .aabbAabb }
    else if env.keyCode == FFI.Key.num3 then
      env.clearKey
      state := { state with mode := .sphereAabb }
    pure state
  view := fun env s => some (Linalg.primitiveOverlapTesterWidget env s)
  handleClickWithLayouts := fun env state _contentId _hitPath click _layouts _widget => do
    if click.button != 0 then return state
    let origin := (env.contentOffsetX + env.physWidthF / 2, env.contentOffsetY + env.physHeightF / 2)
    let scale := 70.0 * env.screenScale
    let worldPos := Linalg.screenToWorld (click.x, click.y) origin scale
    if Linalg.nearPoint worldPos state.centerA 0.6 then
      pure { state with dragging := .shapeA }
    else if Linalg.nearPoint worldPos state.centerB 0.6 then
      pure { state with dragging := .shapeB }
    else
      pure state
  handleHoverWithLayouts := fun env state _contentId _hitPath mouseX mouseY _layouts _widget => do
    let origin := (env.contentOffsetX + env.physWidthF / 2, env.contentOffsetY + env.physHeightF / 2)
    let scale := 70.0 * env.screenScale
    let worldPos := Linalg.screenToWorld (mouseX, mouseY) origin scale
    match state.dragging with
    | .shapeA => pure { state with centerA := worldPos }
    | .shapeB => pure { state with centerB := worldPos }
    | .none => pure state
  handleMouseUpWithLayouts := fun _env state _ _ _ _ _ =>
    pure { state with dragging := .none }
  step := fun c _ s => pure (c, s)

instance : Demo .barycentricCoordinates where
  name := "BARYCENTRIC COORDINATES"
  shortName := "Bary"
  init := fun _ => pure Linalg.barycentricCoordinatesInitialState
  update := fun env s => do
    let mut state := s
    if env.keyCode == FFI.Key.r then
      env.clearKey
      state := Linalg.barycentricCoordinatesInitialState
    pure state
  view := fun env s => some (Linalg.barycentricCoordinatesWidget env s)
  handleClickWithLayouts := fun env state _contentId _hitPath click _layouts _widget => do
    if click.button != 0 then return state
    let origin := (env.contentOffsetX + env.physWidthF / 2, env.contentOffsetY + env.physHeightF / 2)
    let scale := 70.0 * env.screenScale
    let worldPos := Linalg.screenToWorld (click.x, click.y) origin scale
    if Linalg.nearPoint worldPos state.point 0.4 then
      pure { state with dragging := true }
    else
      pure state
  handleHoverWithLayouts := fun env state _contentId _hitPath mouseX mouseY _layouts _widget => do
    if !state.dragging then return state
    let origin := (env.contentOffsetX + env.physWidthF / 2, env.contentOffsetY + env.physHeightF / 2)
    let scale := 70.0 * env.screenScale
    let worldPos := Linalg.screenToWorld (mouseX, mouseY) origin scale
    pure { state with point := worldPos }
  handleMouseUpWithLayouts := fun _env state _ _ _ _ _ =>
    pure { state with dragging := false }
  step := fun c _ s => pure (c, s)

instance : Demo .frustumCullingDemo where
  name := "FRUSTUM CULLING DEMO"
  shortName := "Frustum"
  init := fun _ => pure Linalg.frustumCullingDemoInitialState
  update := fun env s => do
    let mut state := s
    if env.keyCode == FFI.Key.r then
      env.clearKey
      state := Linalg.frustumCullingDemoInitialState
    else if env.keyCode == FFI.Key.j then
      env.clearKey
      state := { state with camYaw := state.camYaw - 0.08 }
    else if env.keyCode == FFI.Key.l then
      env.clearKey
      state := { state with camYaw := state.camYaw + 0.08 }
    else if env.keyCode == FFI.Key.i then
      env.clearKey
      let newPitch := state.camPitch + 0.08
      state := { state with camPitch := if newPitch > 1.2 then 1.2 else newPitch }
    else if env.keyCode == FFI.Key.k then
      env.clearKey
      let newPitch := state.camPitch - 0.08
      state := { state with camPitch := if newPitch < -1.2 then -1.2 else newPitch }
    else if env.keyCode == 24 then
      env.clearKey
      let newDist := state.camDist - 0.3
      state := { state with camDist := if newDist < 2.0 then 2.0 else newDist }
    else if env.keyCode == 27 then
      env.clearKey
      state := { state with camDist := state.camDist + 0.3 }
    pure state
  view := fun env s => some (Linalg.frustumCullingDemoWidget env s)
  handleClickWithLayouts := fun _env state _ _ click _ _ => do
    if click.button != 0 then return state
    pure { state with dragging := true, lastMouseX := click.x, lastMouseY := click.y }
  handleHoverWithLayouts := fun _env state _ _ mouseX mouseY _ _ => do
    if !state.dragging then return state
    let dx := mouseX - state.lastMouseX
    let dy := mouseY - state.lastMouseY
    let newYaw := state.viewYaw + dx * 0.005
    let newPitch := state.viewPitch + dy * 0.005
    pure { state with viewYaw := newYaw, viewPitch := newPitch, lastMouseX := mouseX, lastMouseY := mouseY }
  handleMouseUpWithLayouts := fun _env state _ _ _ _ _ =>
    pure { state with dragging := false }
  step := fun c _ s => pure (c, s)

-- ═══════════════════════════════════════════════════════════════════════════
-- Linalg Curve and Spline Demos
-- ═══════════════════════════════════════════════════════════════════════════

instance : Demo .bezierCurveEditor where
  name := "BEZIER CURVE EDITOR"
  shortName := "Bezier"
  init := fun _ => pure Linalg.bezierCurveEditorInitialState
  update := fun env s => do
    let mut state := s
    if env.keyCode == FFI.Key.r then
      env.clearKey
      state := Linalg.bezierCurveEditorInitialState
    else if env.keyCode == FFI.Key.q then
      env.clearKey
      state := { state with mode := .quadratic }
    else if env.keyCode == FFI.Key.c then
      env.clearKey
      state := { state with mode := .cubic }
    pure state
  view := fun env s => some (Linalg.bezierCurveEditorWidget env s)
  handleClickWithLayouts := fun env state _contentId _hitPath click _layouts _widget => do
    if click.button != 0 then return state
    let localX := click.x - env.contentOffsetX
    let localY := click.y - env.contentOffsetY
    let sliderX := env.physWidthF - 250.0 * env.screenScale
    let sliderY := 95.0 * env.screenScale
    let sliderW := 180.0 * env.screenScale
    let sliderH := 8.0 * env.screenScale
    let hitSlider := localX >= sliderX && localX <= sliderX + sliderW
      && localY >= sliderY - 8.0 && localY <= sliderY + sliderH + 8.0
    if hitSlider then
      let t := Linalg.Float.clamp ((localX - sliderX) / sliderW) 0.0 1.0
      pure { state with t := t, dragging := .slider }
    else
      let origin := (env.physWidthF / 2, env.physHeightF / 2)
      let scale := 70.0 * env.screenScale
      let worldPos := Linalg.screenToWorld (localX, localY) origin scale
      let points := match state.mode with
        | .quadratic => state.quadPoints
        | .cubic => state.cubicPoints
      let mut hit : Option Nat := none
      for i in [:points.size] do
        let p := points.getD i Linalg.Vec2.zero
        if Linalg.nearPoint worldPos p 0.45 then
          hit := some i
      match hit with
      | some idx => pure { state with dragging := .control idx }
      | none => pure state
  handleHoverWithLayouts := fun env state _contentId _hitPath mouseX mouseY _layouts _widget => do
    match state.dragging with
    | .none => pure state
    | .slider =>
        let localX := mouseX - env.contentOffsetX
        let sliderX := env.physWidthF - 250.0 * env.screenScale
        let sliderW := 180.0 * env.screenScale
        let t := Linalg.Float.clamp ((localX - sliderX) / sliderW) 0.0 1.0
        pure { state with t := t }
    | .control idx =>
        let origin := (env.physWidthF / 2, env.physHeightF / 2)
        let scale := 70.0 * env.screenScale
        let worldPos := Linalg.screenToWorld (mouseX - env.contentOffsetX, mouseY - env.contentOffsetY) origin scale
        let state' := match state.mode with
          | .quadratic =>
              if idx < state.quadPoints.size then
                { state with quadPoints := state.quadPoints.set! idx worldPos }
              else state
          | .cubic =>
              if idx < state.cubicPoints.size then
                { state with cubicPoints := state.cubicPoints.set! idx worldPos }
              else state
        pure state'
  handleMouseUpWithLayouts := fun _env state _ _ _ _ _ =>
    pure { state with dragging := .none }
  step := fun c _ s => pure (c, s)

instance : Demo .catmullRomSplineEditor where
  name := "CATMULL-ROM SPLINE EDITOR"
  shortName := "Catmull"
  init := fun _ => pure Linalg.catmullRomSplineEditorInitialState
  update := fun env s => do
    let mut state := s
    if env.keyCode == FFI.Key.r then
      env.clearKey
      state := Linalg.catmullRomSplineEditorInitialState
    else if env.keyCode == FFI.Key.c then
      env.clearKey
      state := { state with closed := !state.closed }
    else if env.keyCode == FFI.Key.delete then
      env.clearKey
      if state.points.size > 0 then
        state := { state with points := state.points.pop }
    else if env.keyCode == FFI.Key.space then
      env.clearKey
      state := { state with animating := !state.animating }
    if state.animating then
      let newT := state.t + env.dt * 0.2
      state := { state with t := if newT > 1.0 then newT - 1.0 else newT }
    pure state
  view := fun env s => some (Linalg.catmullRomSplineEditorWidget env s)
  handleClickWithLayouts := fun env state _contentId _hitPath click _layouts _widget => do
    if click.button != 0 then return state
    let localX := click.x - env.contentOffsetX
    let localY := click.y - env.contentOffsetY
    let sliderX := env.physWidthF - 260.0 * env.screenScale
    let sliderY := 95.0 * env.screenScale
    let sliderW := 190.0 * env.screenScale
    let sliderH := 8.0 * env.screenScale
    let hitSlider := localX >= sliderX && localX <= sliderX + sliderW
      && localY >= sliderY - 8.0 && localY <= sliderY + sliderH + 8.0
    if hitSlider then
      let alpha := Linalg.Float.clamp ((localX - sliderX) / sliderW) 0.0 1.0
      pure { state with alpha := alpha, dragging := .slider }
    else
      let origin := (env.physWidthF / 2, env.physHeightF / 2)
      let scale := 70.0 * env.screenScale
      let worldPos := Linalg.screenToWorld (localX, localY) origin scale
      let mut hit : Option Nat := none
      for i in [:state.points.size] do
        let p := state.points.getD i Linalg.Vec2.zero
        if Linalg.nearPoint worldPos p 0.45 then
          hit := some i
      match hit with
      | some idx => pure { state with dragging := .point idx }
      | none =>
          let newPoints := state.points.push worldPos
          pure { state with points := newPoints, dragging := .point (newPoints.size - 1) }
  handleHoverWithLayouts := fun env state _contentId _hitPath mouseX mouseY _layouts _widget => do
    match state.dragging with
    | .none => pure state
    | .slider =>
        let localX := mouseX - env.contentOffsetX
        let sliderX := env.physWidthF - 260.0 * env.screenScale
        let sliderW := 190.0 * env.screenScale
        let alpha := Linalg.Float.clamp ((localX - sliderX) / sliderW) 0.0 1.0
        pure { state with alpha := alpha }
    | .point idx =>
        let origin := (env.physWidthF / 2, env.physHeightF / 2)
        let scale := 70.0 * env.screenScale
        let worldPos := Linalg.screenToWorld (mouseX - env.contentOffsetX, mouseY - env.contentOffsetY) origin scale
        if idx < state.points.size then
          pure { state with points := state.points.set! idx worldPos }
        else
          pure state
  handleMouseUpWithLayouts := fun _env state _ _ _ _ _ =>
    pure { state with dragging := .none }
  step := fun c _ s => pure (c, s)

instance : Demo .bSplineCurveDemo where
  name := "B-SPLINE CURVE DEMO"
  shortName := "B-Spline"
  init := fun _ => pure Linalg.bSplineCurveDemoInitialState
  update := fun env s => do
    let mut state := s
    let maxDegree := Nat.min 5 (state.controlPoints.size - 1)
    if env.keyCode == FFI.Key.r then
      env.clearKey
      state := Linalg.bSplineCurveDemoInitialState
    else if env.keyCode == FFI.Key.u then
      env.clearKey
      state := { state with knots := Linalg.BSpline.uniform state.controlPoints state.degree |>.knots }
    else if env.keyCode == FFI.Key.num1 then
      env.clearKey
      let d := Nat.min 1 maxDegree
      state := { state with degree := d, knots := Linalg.BSpline.uniform state.controlPoints d |>.knots }
    else if env.keyCode == FFI.Key.num2 then
      env.clearKey
      let d := Nat.min 2 maxDegree
      state := { state with degree := d, knots := Linalg.BSpline.uniform state.controlPoints d |>.knots }
    else if env.keyCode == FFI.Key.num3 then
      env.clearKey
      let d := Nat.min 3 maxDegree
      state := { state with degree := d, knots := Linalg.BSpline.uniform state.controlPoints d |>.knots }
    else if env.keyCode == FFI.Key.num4 then
      env.clearKey
      let d := Nat.min 4 maxDegree
      state := { state with degree := d, knots := Linalg.BSpline.uniform state.controlPoints d |>.knots }
    else if env.keyCode == FFI.Key.num5 then
      env.clearKey
      let d := Nat.min 5 maxDegree
      state := { state with degree := d, knots := Linalg.BSpline.uniform state.controlPoints d |>.knots }
    pure state
  view := fun env s => some (Linalg.bSplineCurveDemoWidget env s)
  handleClickWithLayouts := fun env state _contentId _hitPath click _layouts _widget => do
    if click.button != 0 then return state
    let localX := click.x - env.contentOffsetX
    let localY := click.y - env.contentOffsetY
    let rectX := 40.0 * env.screenScale
    let rectY := env.physHeightF - 190.0 * env.screenScale
    let rectW := env.physWidthF - 80.0 * env.screenScale
    let rectH := 120.0 * env.screenScale
    let knotY := rectY + rectH + 12.0
    let spline := { controlPoints := state.controlPoints, knots := state.knots, degree := state.degree : Linalg.BSpline Linalg.Vec2 }
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
        pure { state with knots := knots, dragging := .knot idx }
    | none =>
        let origin := (env.physWidthF / 2, env.physHeightF / 2 - 40 * env.screenScale)
        let scale := 60.0 * env.screenScale
        let worldPos := Linalg.screenToWorld (localX, localY) origin scale
        let mut hitPt : Option Nat := none
        for i in [:state.controlPoints.size] do
          let p := state.controlPoints.getD i Linalg.Vec2.zero
          if Linalg.nearPoint worldPos p 0.45 then
            hitPt := some i
        match hitPt with
        | some idx => pure { state with dragging := .point idx }
        | none => pure state
  handleHoverWithLayouts := fun env state _contentId _hitPath mouseX mouseY _layouts _widget => do
    match state.dragging with
    | .none => pure state
    | .point idx =>
        let origin := (env.physWidthF / 2, env.physHeightF / 2 - 40 * env.screenScale)
        let scale := 60.0 * env.screenScale
        let worldPos := Linalg.screenToWorld (mouseX - env.contentOffsetX, mouseY - env.contentOffsetY) origin scale
        if idx < state.controlPoints.size then
          pure { state with controlPoints := state.controlPoints.set! idx worldPos }
        else
          pure state
    | .knot idx =>
        let rectX := 40.0 * env.screenScale
        let rectW := env.physWidthF - 80.0 * env.screenScale
        let t := Linalg.Float.clamp ((mouseX - env.contentOffsetX - rectX) / rectW) 0.0 1.0
        let prev := state.knots.getD (idx - 1) 0.0
        let next := state.knots.getD (idx + 1) 1.0
        let v := Linalg.Float.clamp t prev next
        pure { state with knots := state.knots.set! idx v }
  handleMouseUpWithLayouts := fun _env state _ _ _ _ _ =>
    pure { state with dragging := .none }
  step := fun c _ s => pure (c, s)

instance : Demo .arcLengthParameterization where
  name := "ARC-LENGTH PARAMETERIZATION"
  shortName := "ArcLen"
  init := fun _ => pure Linalg.arcLengthParameterizationInitialState
  update := fun env s => do
    let mut state := s
    if env.keyCode == FFI.Key.r then
      env.clearKey
      state := Linalg.arcLengthParameterizationInitialState
    else if env.keyCode == FFI.Key.space then
      env.clearKey
      state := { state with animating := !state.animating }
    if state.animating then
      let p0 := state.controlPoints.getD 0 Linalg.Vec2.zero
      let p1 := state.controlPoints.getD 1 Linalg.Vec2.zero
      let p2 := state.controlPoints.getD 2 Linalg.Vec2.zero
      let p3 := state.controlPoints.getD 3 Linalg.Vec2.zero
      let curve := Linalg.Bezier3.mk p0 p1 p2 p3
      let evalFn := fun t => Linalg.Bezier3.evalVec2 curve t
      let table := Linalg.ArcLengthTable.build evalFn 120
      let newT := state.t + env.dt * 0.2
      let newS := state.s + state.speed * env.dt
      let wrappedS := if table.totalLength > Linalg.Float.epsilon then
        if newS > table.totalLength then newS - table.totalLength else newS
      else 0.0
      state := { state with t := (if newT > 1.0 then newT - 1.0 else newT), s := wrappedS }
    pure state
  view := fun env s => some (Linalg.arcLengthParameterizationWidget env s)
  handleClickWithLayouts := fun env state _contentId _hitPath click _layouts _widget => do
    if click.button != 0 then return state
    let localX := click.x - env.contentOffsetX
    let localY := click.y - env.contentOffsetY
    let sliderX := env.physWidthF - 260.0 * env.screenScale
    let sliderY := 95.0 * env.screenScale
    let sliderW := 190.0 * env.screenScale
    let sliderH := 8.0 * env.screenScale
    let hitSlider := localX >= sliderX && localX <= sliderX + sliderW
      && localY >= sliderY - 8.0 && localY <= sliderY + sliderH + 8.0
    if hitSlider then
      let t := Linalg.Float.clamp ((localX - sliderX) / sliderW) 0.0 1.0
      let speed := 0.2 + t * 3.8
      pure { state with speed := speed, dragging := .slider }
    else
      let origin := (env.physWidthF / 2, env.physHeightF / 2)
      let scale := 70.0 * env.screenScale
      let worldPos := Linalg.screenToWorld (localX, localY) origin scale
      let mut hit : Option Nat := none
      for i in [:state.controlPoints.size] do
        let p := state.controlPoints.getD i Linalg.Vec2.zero
        if Linalg.nearPoint worldPos p 0.45 then
          hit := some i
      match hit with
      | some idx => pure { state with dragging := .point idx }
      | none => pure state
  handleHoverWithLayouts := fun env state _contentId _hitPath mouseX mouseY _layouts _widget => do
    match state.dragging with
    | .none => pure state
    | .slider =>
        let localX := mouseX - env.contentOffsetX
        let sliderX := env.physWidthF - 260.0 * env.screenScale
        let sliderW := 190.0 * env.screenScale
        let t := Linalg.Float.clamp ((localX - sliderX) / sliderW) 0.0 1.0
        let speed := 0.2 + t * 3.8
        pure { state with speed := speed }
    | .point idx =>
        let origin := (env.physWidthF / 2, env.physHeightF / 2)
        let scale := 70.0 * env.screenScale
        let worldPos := Linalg.screenToWorld (mouseX - env.contentOffsetX, mouseY - env.contentOffsetY) origin scale
        if idx < state.controlPoints.size then
          pure { state with controlPoints := state.controlPoints.set! idx worldPos }
        else
          pure state
  handleMouseUpWithLayouts := fun _env state _ _ _ _ _ =>
    pure { state with dragging := .none }
  step := fun c _ s => pure (c, s)

instance : Demo .bezierPatchSurface where
  name := "BEZIER PATCH SURFACE"
  shortName := "Patch"
  init := fun _ => pure Linalg.bezierPatchSurfaceInitialState
  update := fun env s => do
    let mut state := s
    if env.keyCode == FFI.Key.r then
      env.clearKey
      state := Linalg.bezierPatchSurfaceInitialState
    else if env.keyCode == FFI.Key.n then
      env.clearKey
      state := { state with showNormals := !state.showNormals }
    else if env.keyCode == FFI.Key.left then
      env.clearKey
      let newTess := if state.tessellation > 2 then state.tessellation - 1 else 2
      state := { state with tessellation := newTess }
    else if env.keyCode == FFI.Key.right then
      env.clearKey
      let newTess := if state.tessellation < 18 then state.tessellation + 1 else 18
      state := { state with tessellation := newTess }
    else if env.keyCode == FFI.Key.up || env.keyCode == FFI.Key.down then
      let delta := if env.keyCode == FFI.Key.up then 0.2 else -0.2
      env.clearKey
      match state.selected with
      | some idx =>
          let row := idx / 4
          let col := idx % 4
          let p := state.patch.getPoint row col
          let patch := state.patch.setPoint row col (Linalg.Vec3.mk p.x p.y (p.z + delta))
          state := { state with patch := patch }
      | none => state := state
    pure state
  view := fun env s => some (Linalg.bezierPatchSurfaceWidget env s)
  handleClickWithLayouts := fun env state _contentId _hitPath click _layouts _widget => do
    let localX := click.x - env.contentOffsetX
    let localY := click.y - env.contentOffsetY
    let rectX := env.physWidthF - 260.0 * env.screenScale
    let rectY := 110.0 * env.screenScale
    let rectW := 220.0 * env.screenScale
    let rectH := 220.0 * env.screenScale
    let withinMini := localX >= rectX && localX <= rectX + rectW
      && localY >= rectY && localY <= rectY + rectH
    if click.button == 1 then
      pure { state with dragging := .camera, lastMouseX := click.x, lastMouseY := click.y }
    else if click.button == 0 && withinMini then
      let origin := (rectX + rectW / 2, rectY + rectH / 2)
      let scale := rectW / 5.8
      let worldPos := Linalg.screenToWorld (localX, localY) origin scale
      let mut hit : Option Nat := none
      for idx in [:16] do
        let row := idx / 4
        let col := idx % 4
        let p := state.patch.getPoint row col
        let p2 := Linalg.Vec2.mk p.x p.y
        if Linalg.nearPoint worldPos p2 0.35 then
          hit := some idx
      match hit with
      | some idx => pure { state with selected := some idx, dragging := .point idx }
      | none => pure state
    else
      pure state
  handleHoverWithLayouts := fun env state _contentId _hitPath mouseX mouseY _layouts _widget => do
    match state.dragging with
    | .none => pure state
    | .camera =>
        let dx := mouseX - state.lastMouseX
        let dy := mouseY - state.lastMouseY
        let yaw := state.cameraYaw + dx * 0.005
        let pitch := state.cameraPitch + dy * 0.005
        pure { state with cameraYaw := yaw, cameraPitch := pitch, lastMouseX := mouseX, lastMouseY := mouseY }
    | .point idx =>
        let rectX := env.physWidthF - 260.0 * env.screenScale
        let rectY := 110.0 * env.screenScale
        let rectW := 220.0 * env.screenScale
        let rectH := 220.0 * env.screenScale
        let origin := (rectX + rectW / 2, rectY + rectH / 2)
        let scale := rectW / 5.8
        let localX := mouseX - env.contentOffsetX
        let localY := mouseY - env.contentOffsetY
        let worldPos := Linalg.screenToWorld (localX, localY) origin scale
        let row := idx / 4
        let col := idx % 4
        let p := state.patch.getPoint row col
        let patch := state.patch.setPoint row col (Linalg.Vec3.mk worldPos.x worldPos.y p.z)
        pure { state with patch := patch }
  handleMouseUpWithLayouts := fun _env state _ _ _ _ _ =>
    pure { state with dragging := .none }
  step := fun c _ s => pure (c, s)

-- ═══════════════════════════════════════════════════════════════════════════
-- Linalg Easing & Interpolation Demos
-- ═══════════════════════════════════════════════════════════════════════════

instance : Demo .easingFunctionGallery where
  name := "EASING FUNCTION GALLERY"
  shortName := "Easing"
  init := fun _ => pure Linalg.easingFunctionGalleryInitialState
  update := fun env s => do
    let mut state := s
    let count := Linalg.easingEntryCount
    if count == 0 then
      return state
    if env.keyCode == FFI.Key.r then
      env.clearKey
      state := Linalg.easingFunctionGalleryInitialState
    else if env.keyCode == FFI.Key.space then
      env.clearKey
      state := { state with animating := !state.animating }
    else if env.keyCode == FFI.Key.c then
      env.clearKey
      state := { state with compareMode := !state.compareMode }
    else if env.keyCode == FFI.Key.tab then
      env.clearKey
      state := { state with selected := (state.selected + 1) % count }
    else if env.keyCode == FFI.Key.left then
      env.clearKey
      state := { state with selected := (state.selected + count - 1) % count }
    else if env.keyCode == FFI.Key.right then
      env.clearKey
      state := { state with selected := (state.selected + 1) % count }
    else if env.keyCode == FFI.Key.x then
      env.clearKey
      state := { state with compare := (state.compare + 1) % count }
    else if env.keyCode == FFI.Key.up then
      env.clearKey
      state := { state with speed := Linalg.Float.clamp (state.speed + 0.1) 0.1 3.0 }
    else if env.keyCode == FFI.Key.down then
      env.clearKey
      state := { state with speed := Linalg.Float.clamp (state.speed - 0.1) 0.1 3.0 }

    if state.compare == state.selected then
      state := { state with compare := (state.compare + 1) % count }

    if state.animating then
      let newT := state.t + env.dt * state.speed
      state := { state with t := if newT > 1.0 then newT - 1.0 else newT }
    pure state
  view := fun env s => some (Linalg.easingFunctionGalleryWidget env s)
  step := fun c _ s => pure (c, s)

instance : Demo .smoothDampFollower where
  name := "SMOOTH DAMP FOLLOWER"
  shortName := "SmoothD"
  init := fun _ => pure Linalg.smoothDampFollowerInitialState
  update := fun env s => do
    let mut state := s
    if env.keyCode == FFI.Key.r then
      env.clearKey
      state := Linalg.smoothDampFollowerInitialState
    else if env.keyCode == FFI.Key.space then
      env.clearKey
      state := { state with animating := !state.animating }
    if state.animating then
      let (_newPos, newState) := Linalg.SmoothDampState2.step
        state.dampState state.target state.smoothTime env.dt state.maxSpeed
      let speed := newState.velocity.length
      let mut history := state.history.push speed
      if history.size > 120 then
        history := history.eraseIdxIfInBounds 0
      state := { state with dampState := newState, history := history }
    pure state
  view := fun env s => some (Linalg.smoothDampFollowerWidget env s)
  handleClickWithLayouts := fun env state _contentId _hitPath click _layouts _widget => do
    if click.button != 0 then return state
    let localX := click.x - env.contentOffsetX
    let localY := click.y - env.contentOffsetY
    let layoutSmooth := Linalg.smoothDampSliderLayout env.physWidthF env.physHeightF env.screenScale 0
    let layoutMax := Linalg.smoothDampSliderLayout env.physWidthF env.physHeightF env.screenScale 1
    let hitSmooth := localX >= layoutSmooth.x && localX <= layoutSmooth.x + layoutSmooth.width
      && localY >= layoutSmooth.y - 8.0 && localY <= layoutSmooth.y + layoutSmooth.height + 8.0
    let hitMax := localX >= layoutMax.x && localX <= layoutMax.x + layoutMax.width
      && localY >= layoutMax.y - 8.0 && localY <= layoutMax.y + layoutMax.height + 8.0
    if hitSmooth then
      let t := Linalg.Float.clamp ((localX - layoutSmooth.x) / layoutSmooth.width) 0.0 1.0
      let value := Linalg.smoothDampSmoothTimeFrom t
      pure { state with smoothTime := value, dragging := .slider .smoothTime }
    else if hitMax then
      let t := Linalg.Float.clamp ((localX - layoutMax.x) / layoutMax.width) 0.0 1.0
      let value := Linalg.smoothDampMaxSpeedFrom t
      pure { state with maxSpeed := value, dragging := .slider .maxSpeed }
    else
      let origin := (env.physWidthF / 2, env.physHeightF / 2)
      let scale := 70.0 * env.screenScale
      let worldPos := Linalg.screenToWorld (localX, localY) origin scale
      if Linalg.nearPoint worldPos state.target 0.45 then
        pure { state with dragging := .target }
      else
        pure { state with target := worldPos, dragging := .target }
  handleHoverWithLayouts := fun env state _contentId _hitPath mouseX mouseY _layouts _widget => do
    match state.dragging with
    | .none => pure state
    | .target =>
        let origin := (env.physWidthF / 2, env.physHeightF / 2)
        let scale := 70.0 * env.screenScale
        let worldPos := Linalg.screenToWorld (mouseX - env.contentOffsetX, mouseY - env.contentOffsetY) origin scale
        pure { state with target := worldPos }
    | .slider which =>
        let localX := mouseX - env.contentOffsetX
        match which with
        | .smoothTime =>
            let layout := Linalg.smoothDampSliderLayout env.physWidthF env.physHeightF env.screenScale 0
            let t := Linalg.Float.clamp ((localX - layout.x) / layout.width) 0.0 1.0
            pure { state with smoothTime := Linalg.smoothDampSmoothTimeFrom t }
        | .maxSpeed =>
            let layout := Linalg.smoothDampSliderLayout env.physWidthF env.physHeightF env.screenScale 1
            let t := Linalg.Float.clamp ((localX - layout.x) / layout.width) 0.0 1.0
            pure { state with maxSpeed := Linalg.smoothDampMaxSpeedFrom t }
  handleMouseUpWithLayouts := fun _env state _ _ _ _ _ =>
    pure { state with dragging := .none }
  step := fun c _ s => pure (c, s)

instance : Demo .springAnimationPlayground where
  name := "SPRING ANIMATION PLAYGROUND"
  shortName := "Spring"
  init := fun _ => pure Linalg.springAnimationPlaygroundInitialState
  update := fun env s => do
    let mut state := s
    if env.keyCode == FFI.Key.r then
      env.clearKey
      state := Linalg.springAnimationPlaygroundInitialState
    else if env.keyCode == FFI.Key.space then
      env.clearKey
      state := { state with animating := !state.animating }
    if state.animating then
      let newTime := state.time + env.dt
      let time := if newTime > 4.0 then newTime - 4.0 else newTime
      let ω := 2.0 * Linalg.Float.pi * state.frequency
      let x := Linalg.springResponse time state.dampingRatio ω
      let v := Linalg.springVelocity time state.dampingRatio ω
      let energy := 0.5 * (x * x + (v / ω) * (v / ω))
      let mut history := state.energyHistory.push energy
      if history.size > 140 then
        history := history.eraseIdxIfInBounds 0
      state := { state with time := time, energyHistory := history }
    pure state
  view := fun env s => some (Linalg.springAnimationPlaygroundWidget env s)
  handleClickWithLayouts := fun env state _contentId _hitPath click _layouts _widget => do
    if click.button != 0 then return state
    let localX := click.x - env.contentOffsetX
    let localY := click.y - env.contentOffsetY
    let layoutDamp := Linalg.springSliderLayout env.physWidthF env.physHeightF env.screenScale 0
    let layoutFreq := Linalg.springSliderLayout env.physWidthF env.physHeightF env.screenScale 1
    let hitDamp := localX >= layoutDamp.x && localX <= layoutDamp.x + layoutDamp.width
      && localY >= layoutDamp.y - 8.0 && localY <= layoutDamp.y + layoutDamp.height + 8.0
    let hitFreq := localX >= layoutFreq.x && localX <= layoutFreq.x + layoutFreq.width
      && localY >= layoutFreq.y - 8.0 && localY <= layoutFreq.y + layoutFreq.height + 8.0
    if hitDamp then
      let t := Linalg.Float.clamp ((localX - layoutDamp.x) / layoutDamp.width) 0.0 1.0
      pure { state with dampingRatio := Linalg.springDampingFrom t, dragging := .sliderDamping }
    else if hitFreq then
      let t := Linalg.Float.clamp ((localX - layoutFreq.x) / layoutFreq.width) 0.0 1.0
      pure { state with frequency := Linalg.springFrequencyFrom t, dragging := .sliderFrequency }
    else
      pure state
  handleHoverWithLayouts := fun env state _contentId _hitPath mouseX _mouseY _layouts _widget => do
    let localX := mouseX - env.contentOffsetX
    match state.dragging with
    | .sliderDamping =>
        let layout := Linalg.springSliderLayout env.physWidthF env.physHeightF env.screenScale 0
        let t := Linalg.Float.clamp ((localX - layout.x) / layout.width) 0.0 1.0
        pure { state with dampingRatio := Linalg.springDampingFrom t }
    | .sliderFrequency =>
        let layout := Linalg.springSliderLayout env.physWidthF env.physHeightF env.screenScale 1
        let t := Linalg.Float.clamp ((localX - layout.x) / layout.width) 0.0 1.0
        pure { state with frequency := Linalg.springFrequencyFrom t }
    | .none => pure state
  handleMouseUpWithLayouts := fun _env state _ _ _ _ _ =>
    pure { state with dragging := .none }
  step := fun c _ s => pure (c, s)

-- Linalg Noise Demos

instance : Demo .noiseExplorer2D where
  name := "NOISE EXPLORER 2D"
  shortName := "Noise2D"
  init := fun _ => pure Linalg.noiseExplorer2DInitialState
  update := fun env s => do
    let mut state := s
    if env.keyCode == FFI.Key.r then
      env.clearKey
      state := Linalg.noiseExplorer2DInitialState
    pure state
  view := fun env s => some (Linalg.noiseExplorer2DWidget env s)
  handleClickWithLayouts := fun env state _contentId _hitPath click _layouts _widget => do
    if click.button != 0 then return state
    let localX := click.x - env.contentOffsetX
    let localY := click.y - env.contentOffsetY
    let drop := Linalg.noiseExplorerDropdownLayout env.physWidthF env.physHeightF env.screenScale
    let inDrop := localX >= drop.x && localX <= drop.x + drop.width
      && localY >= drop.y && localY <= drop.y + drop.height
    if inDrop then
      pure { state with dropdownOpen := !state.dropdownOpen }
    else if state.dropdownOpen then
      let mut selected : Option Linalg.NoiseType := none
      for i in [:Linalg.noiseExplorerOptions.size] do
        let optLayout := Linalg.noiseExplorerDropdownOptionLayout drop i
        if localX >= optLayout.x && localX <= optLayout.x + optLayout.width
            && localY >= optLayout.y && localY <= optLayout.y + optLayout.height then
          selected := arrayGet? Linalg.noiseExplorerOptions i
      match selected with
      | some opt => pure { state with noiseType := opt, dropdownOpen := false }
      | none => pure { state with dropdownOpen := false }
    else
      let toggle := Linalg.noiseExplorerFbmToggleLayout env.physWidthF env.physHeightF env.screenScale
      let inToggle := localX >= toggle.x && localX <= toggle.x + toggle.size
        && localY >= toggle.y && localY <= toggle.y + toggle.size
      if inToggle then
        pure { state with useFbm := !state.useFbm }
      else
        let sliders : Array Linalg.NoiseExplorerSlider := #[.scale, .offsetX, .offsetY, .octaves, .lacunarity, .persistence, .jitter]
        let mut hit : Option Linalg.NoiseExplorerSlider := none
        for i in [:sliders.size] do
          let layout := Linalg.noiseExplorerSliderLayout env.physWidthF env.physHeightF env.screenScale i
          let within := localX >= layout.x && localX <= layout.x + layout.width
            && localY >= layout.y - 10.0 && localY <= layout.y + layout.height + 10.0
          if within then
            hit := arrayGet? sliders i
        match hit with
        | some which =>
            let layout := Linalg.noiseExplorerSliderLayout env.physWidthF env.physHeightF env.screenScale
              (sliders.findIdx? (fun s => s == which) |>.getD 0)
            let t := Linalg.Float.clamp ((localX - layout.x) / layout.width) 0.0 1.0
            let newState := Linalg.noiseExplorerApplySlider state which t
            pure { newState with dragging := .slider which }
        | none => pure state
  handleHoverWithLayouts := fun env state _contentId _hitPath mouseX _mouseY _layouts _widget => do
    match state.dragging with
    | .slider which =>
        let localX := mouseX - env.contentOffsetX
        let sliders : Array Linalg.NoiseExplorerSlider := #[.scale, .offsetX, .offsetY, .octaves, .lacunarity, .persistence, .jitter]
        let idx := sliders.findIdx? (fun s => s == which) |>.getD 0
        let layout := Linalg.noiseExplorerSliderLayout env.physWidthF env.physHeightF env.screenScale idx
        let t := Linalg.Float.clamp ((localX - layout.x) / layout.width) 0.0 1.0
        pure (Linalg.noiseExplorerApplySlider state which t)
    | .none => pure state
  handleMouseUpWithLayouts := fun _env state _ _ _ _ _ =>
    pure { state with dragging := .none }
  step := fun c _ s => pure (c, s)

instance : Demo .fbmTerrainGenerator where
  name := "FBM TERRAIN GENERATOR"
  shortName := "Terrain"
  init := fun _ => pure Linalg.fbmTerrainInitialState
  update := fun env s => do
    let mut state := s
    if env.keyCode == FFI.Key.r then
      env.clearKey
      state := Linalg.fbmTerrainInitialState
    else if env.keyCode == FFI.Key.w then
      env.clearKey
      state := { state with showWireframe := !state.showWireframe }
    else if env.keyCode == FFI.Key.t then
      env.clearKey
      state := { state with showTexture := !state.showTexture }
    else if env.keyCode == FFI.Key.n then
      env.clearKey
      state := { state with showNormals := !state.showNormals }
    pure state
  view := fun env s => some (Linalg.fbmTerrainWidget env s)
  handleClickWithLayouts := fun env state _contentId _hitPath click _layouts _widget => do
    let localX := click.x - env.contentOffsetX
    let localY := click.y - env.contentOffsetY
    if click.button == 1 then
      pure { state with dragging := .camera, lastMouseX := click.x, lastMouseY := click.y }
    else if click.button != 0 then
      pure state
    else
      let toggleA := Linalg.fbmTerrainToggleLayout env.physWidthF env.physHeightF env.screenScale 0
      let toggleB := Linalg.fbmTerrainToggleLayout env.physWidthF env.physHeightF env.screenScale 1
      let toggleC := Linalg.fbmTerrainToggleLayout env.physWidthF env.physHeightF env.screenScale 2
      let hitToggle (t : Linalg.FBMTerrainToggleLayout) : Bool :=
        localX >= t.x && localX <= t.x + t.size && localY >= t.y && localY <= t.y + t.size
      if hitToggle toggleA then
        pure { state with showWireframe := !state.showWireframe }
      else if hitToggle toggleB then
        pure { state with showTexture := !state.showTexture }
      else if hitToggle toggleC then
        pure { state with showNormals := !state.showNormals }
      else
        let sliders : Array Linalg.TerrainSlider := #[.scale, .height, .octaves, .lacunarity, .persistence, .power, .terrace]
        let mut hit : Option Linalg.TerrainSlider := none
        for i in [:sliders.size] do
          let layout := Linalg.fbmTerrainSliderLayout env.physWidthF env.physHeightF env.screenScale i
          let within := localX >= layout.x && localX <= layout.x + layout.width
            && localY >= layout.y - 10.0 && localY <= layout.y + layout.height + 10.0
          if within then
            hit := arrayGet? sliders i
        match hit with
        | some which =>
            let idx := sliders.findIdx? (fun s => s == which) |>.getD 0
            let layout := Linalg.fbmTerrainSliderLayout env.physWidthF env.physHeightF env.screenScale idx
            let t := Linalg.Float.clamp ((localX - layout.x) / layout.width) 0.0 1.0
            let newState := Linalg.fbmTerrainApplySlider state which t
            pure { newState with dragging := .slider which }
        | none => pure state
  handleHoverWithLayouts := fun env state _contentId _hitPath mouseX mouseY _layouts _widget => do
    match state.dragging with
    | .camera =>
        let dx := mouseX - state.lastMouseX
        let dy := mouseY - state.lastMouseY
        let newYaw := state.cameraYaw + dx * 0.005
        let newPitch := Linalg.Float.clamp (state.cameraPitch + dy * 0.005) (-0.2) 1.4
        pure { state with cameraYaw := newYaw, cameraPitch := newPitch, lastMouseX := mouseX, lastMouseY := mouseY }
    | .slider which =>
        let localX := mouseX - env.contentOffsetX
        let sliders : Array Linalg.TerrainSlider := #[.scale, .height, .octaves, .lacunarity, .persistence, .power, .terrace]
        let idx := sliders.findIdx? (fun s => s == which) |>.getD 0
        let layout := Linalg.fbmTerrainSliderLayout env.physWidthF env.physHeightF env.screenScale idx
        let t := Linalg.Float.clamp ((localX - layout.x) / layout.width) 0.0 1.0
        pure (Linalg.fbmTerrainApplySlider state which t)
    | .none => pure state
  handleMouseUpWithLayouts := fun _env state _ _ _ _ _ =>
    pure { state with dragging := .none }
  step := fun c _ s => pure (c, s)

instance : Demo .domainWarpingDemo where
  name := "DOMAIN WARPING DEMO"
  shortName := "Warp"
  init := fun _ => pure Linalg.domainWarpingInitialState
  update := fun env s => do
    let mut state := s
    if env.keyCode == FFI.Key.r then
      env.clearKey
      state := Linalg.domainWarpingInitialState
    if state.animate then
      state := { state with time := state.time + env.dt * state.speed }
    pure state
  view := fun env s => some (Linalg.domainWarpingDemoWidget env s)
  handleClickWithLayouts := fun env state _contentId _hitPath click _layouts _widget => do
    if click.button != 0 then return state
    let localX := click.x - env.contentOffsetX
    let localY := click.y - env.contentOffsetY
    let toggleA := Linalg.domainWarpingToggleLayout env.physWidthF env.physHeightF env.screenScale 0
    let toggleB := Linalg.domainWarpingToggleLayout env.physWidthF env.physHeightF env.screenScale 1
    let toggleC := Linalg.domainWarpingToggleLayout env.physWidthF env.physHeightF env.screenScale 2
    let hitToggle (t : Linalg.DomainWarpingToggleLayout) : Bool :=
      localX >= t.x && localX <= t.x + t.size && localY >= t.y && localY <= t.y + t.size
    if hitToggle toggleA then
      pure { state with useAdvanced := !state.useAdvanced }
    else if hitToggle toggleB then
      pure { state with animate := !state.animate }
    else if hitToggle toggleC then
      pure { state with showVectors := !state.showVectors }
    else
      let sliders : Array Linalg.WarpingSlider := #[.strength1, .strength2, .scale, .speed]
      let mut hit : Option Linalg.WarpingSlider := none
      for i in [:sliders.size] do
        let layout := Linalg.domainWarpingSliderLayout env.physWidthF env.physHeightF env.screenScale i
        let within := localX >= layout.x && localX <= layout.x + layout.width
          && localY >= layout.y - 10.0 && localY <= layout.y + layout.height + 10.0
        if within then
          hit := arrayGet? sliders i
      match hit with
      | some which =>
          let idx := sliders.findIdx? (fun s => s == which) |>.getD 0
          let layout := Linalg.domainWarpingSliderLayout env.physWidthF env.physHeightF env.screenScale idx
          let t := Linalg.Float.clamp ((localX - layout.x) / layout.width) 0.0 1.0
          let newState := Linalg.domainWarpingApplySlider state which t
          pure { newState with dragging := .slider which }
      | none => pure state
  handleHoverWithLayouts := fun env state _contentId _hitPath mouseX _mouseY _layouts _widget => do
    match state.dragging with
    | .slider which =>
        let localX := mouseX - env.contentOffsetX
        let sliders : Array Linalg.WarpingSlider := #[.strength1, .strength2, .scale, .speed]
        let idx := sliders.findIdx? (fun s => s == which) |>.getD 0
        let layout := Linalg.domainWarpingSliderLayout env.physWidthF env.physHeightF env.screenScale idx
        let t := Linalg.Float.clamp ((localX - layout.x) / layout.width) 0.0 1.0
        pure (Linalg.domainWarpingApplySlider state which t)
    | .none => pure state
  handleMouseUpWithLayouts := fun _env state _ _ _ _ _ =>
    pure { state with dragging := .none }
  step := fun c _ s => pure (c, s)

instance : Demo .worleyCellularNoise where
  name := "WORLEY CELLULAR NOISE"
  shortName := "Worley"
  init := fun _ => pure Linalg.worleyCellularInitialState
  update := fun env s => do
    let mut state := s
    if env.keyCode == FFI.Key.r then
      env.clearKey
      state := Linalg.worleyCellularInitialState
    pure state
  view := fun env s => some (Linalg.worleyCellularNoiseWidget env s)
  handleClickWithLayouts := fun env state _contentId _hitPath click _layouts _widget => do
    if click.button != 0 then return state
    let localX := click.x - env.contentOffsetX
    let localY := click.y - env.contentOffsetY
    let drop := Linalg.worleyDropdownLayout env.physWidthF env.physHeightF env.screenScale
    let inDrop := localX >= drop.x && localX <= drop.x + drop.width
      && localY >= drop.y && localY <= drop.y + drop.height
    if inDrop then
      pure { state with dropdownOpen := !state.dropdownOpen }
    else if state.dropdownOpen then
      let mut selected : Option Linalg.WorleyMode := none
      for i in [:Linalg.worleyModeOptions.size] do
        let optLayout := Linalg.worleyDropdownOptionLayout drop i
        if localX >= optLayout.x && localX <= optLayout.x + optLayout.width
            && localY >= optLayout.y && localY <= optLayout.y + optLayout.height then
          selected := arrayGet? Linalg.worleyModeOptions i
      match selected with
      | some opt => pure { state with mode := opt, dropdownOpen := false }
      | none => pure { state with dropdownOpen := false }
    else
      let layout := Linalg.worleySliderLayout env.physWidthF env.physHeightF env.screenScale
      let inSlider := localX >= layout.x && localX <= layout.x + layout.width
        && localY >= layout.y - 10.0 && localY <= layout.y + layout.height + 10.0
      if inSlider then
        let t := Linalg.Float.clamp ((localX - layout.x) / layout.width) 0.0 1.0
        let newState := { state with jitter := Linalg.worleyJitterFromSlider t }
        pure { newState with dragging := .slider }
      else
        let toggleA := Linalg.worleyToggleLayout env.physWidthF env.physHeightF env.screenScale 0
        let toggleB := Linalg.worleyToggleLayout env.physWidthF env.physHeightF env.screenScale 1
        let toggleC := Linalg.worleyToggleLayout env.physWidthF env.physHeightF env.screenScale 2
        let hitToggle (t : Linalg.WorleyToggleLayout) : Bool :=
          localX >= t.x && localX <= t.x + t.size && localY >= t.y && localY <= t.y + t.size
        if hitToggle toggleA then
          pure { state with showEdges := !state.showEdges }
        else if hitToggle toggleB then
          pure { state with showPoints := !state.showPoints }
        else if hitToggle toggleC then
          pure { state with showConnections := !state.showConnections }
        else
          pure state
  handleHoverWithLayouts := fun env state _contentId _hitPath mouseX _mouseY _layouts _widget => do
    match state.dragging with
    | .slider =>
        let localX := mouseX - env.contentOffsetX
        let layout := Linalg.worleySliderLayout env.physWidthF env.physHeightF env.screenScale
        let t := Linalg.Float.clamp ((localX - layout.x) / layout.width) 0.0 1.0
        pure { state with jitter := Linalg.worleyJitterFromSlider t }
    | .none => pure state
  handleMouseUpWithLayouts := fun _env state _ _ _ _ _ =>
    pure { state with dragging := .none }
  step := fun c _ s => pure (c, s)


end Demos
