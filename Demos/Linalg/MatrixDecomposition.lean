/-
  Matrix Decomposition - Visualize 2D matrix decomposition into rotation, scale, rotation.
  Shows how any 2x2 matrix can be decomposed into simpler components.
  Demonstrates SVD-like decomposition concepts visually.
-/
import Afferent
import Afferent.Widget
import Afferent.Arbor
import Demos.Core.Demo
import Demos.Linalg.Shared
import Trellis
import Linalg.Core
import Linalg.Vec2
import Linalg.Mat2

open Afferent CanvasM Linalg

namespace Demos.Linalg

/-- Decomposition result: R2 * S * R1 where R1, R2 are rotations and S is scale -/
structure MatrixDecomp where
  rotation1 : Float  -- First rotation angle
  scaleX : Float     -- Scale along X after R1
  scaleY : Float     -- Scale along Y after R1
  rotation2 : Float  -- Second rotation angle
  deriving Inhabited

/-- Simple 2D SVD-like decomposition -/
def decomposeMatrix2D (m : Mat2) : MatrixDecomp :=
  -- Extract columns as vectors
  let c0 := m.column 0
  let c1 := m.column 1

  -- Get the lengths (singular values approximation)
  let sx := c0.length
  let sy := c1.length

  -- Get rotation from first column
  let angle1 := Float.atan2 c0.y c0.x

  -- Check for reflection by looking at determinant
  let det := m.determinant
  let effectiveScaleY := if det < 0 then -sy else sy

  -- Second rotation accounts for non-axis-aligned scaling
  let angle2 := Float.atan2 c1.y c1.x - Float.pi / 2

  {
    rotation1 := angle1
    scaleX := sx
    scaleY := effectiveScaleY
    rotation2 := 0.0  -- Simplified: we absorb into rotation1
  }

/-- Visualization step in the decomposition animation -/
inductive DecompStep where
  | original       -- Show original matrix effect
  | afterRotation1 -- After undoing first rotation
  | afterScale     -- After undoing scale
  | afterRotation2 -- After undoing second rotation (identity)
  deriving BEq, Inhabited

/-- State for the matrix decomposition demo -/
structure MatrixDecompositionState where
  matrix : Mat2 := Mat2.fromRows (Vec2.mk 1.5 0.5) (Vec2.mk (-0.3) 1.2)
  decomp : MatrixDecomp := decomposeMatrix2D (Mat2.fromRows (Vec2.mk 1.5 0.5) (Vec2.mk (-0.3) 1.2))
  currentStep : DecompStep := .original
  animating : Bool := false
  animT : Float := 0.0
  showComponents : Bool := true
  presetIndex : Nat := 0
  deriving Inhabited

def matrixDecompositionInitialState : MatrixDecompositionState := {}

/-- Available matrix presets for decomposition -/
def decompositionPresets : Array (String × Mat2) := #[
  ("Scale + Rotate", Mat2.fromRows (Vec2.mk 1.5 0.5) (Vec2.mk (-0.3) 1.2)),
  ("Pure Rotation", Mat2.rotation (Float.pi / 6)),
  ("Pure Scale", Mat2.scaling 2.0 0.5),
  ("Shear", Mat2.fromRows (Vec2.mk 1.0 0.8) (Vec2.mk 0.0 1.0)),
  ("Reflection + Scale", Mat2.fromRows (Vec2.mk (-1.5) 0.0) (Vec2.mk 0.0 1.0)),
  ("Complex", Mat2.fromRows (Vec2.mk 0.8 (-1.2)) (Vec2.mk 0.6 0.9))
]

/-- Get matrix for current animation step -/
def getStepMatrix (state : MatrixDecompositionState) : Mat2 :=
  let d := state.decomp
  match state.currentStep with
  | .original => state.matrix
  | .afterRotation1 =>
    -- Undo first rotation: apply inverse rotation to see aligned axes
    let invR1 := Mat2.rotation (-d.rotation1)
    invR1 * state.matrix
  | .afterScale =>
    -- After undoing rotation and scale: should be close to identity
    let invR1 := Mat2.rotation (-d.rotation1)
    let invS := if Float.abs d.scaleX > 0.001 && Float.abs d.scaleY > 0.001
                then Mat2.scaling (1.0 / d.scaleX) (1.0 / d.scaleY)
                else Mat2.identity
    invS * invR1 * state.matrix
  | .afterRotation2 => Mat2.identity

/-- Draw a unit circle and its transformation -/
def drawUnitCircle (matrix : Mat2) (origin : Float × Float) (scale : Float)
    (color : Color) (filled : Bool := false) : CanvasM Unit := do
  let segments := 32
  let mut points : Array (Float × Float) := #[]

  for i in [:segments] do
    let angle := (i.toFloat / segments.toFloat) * 2.0 * Float.pi
    let unitPoint := Vec2.mk (Float.cos angle) (Float.sin angle)
    let transformed := matrix.transformVec2 unitPoint
    let screenPoint := worldToScreen transformed origin scale
    points := points.push screenPoint

  if points.size < 2 then return

  let first := points.getD 0 (0.0, 0.0)
  let mut path := Afferent.Path.empty |>.moveTo (Point.mk first.1 first.2)
  for i in [1:points.size] do
    let p := points.getD i (0.0, 0.0)
    path := path.lineTo (Point.mk p.1 p.2)
  path := path.closePath

  if filled then
    setFillColor (Color.rgba color.r color.g color.b 0.2)
    fillPath path

  setStrokeColor color
  setLineWidth 2.0
  strokePath path

/-- Draw decomposition component visualization -/
def drawDecompositionComponents (state : MatrixDecompositionState) (origin : Float × Float)
    (scale : Float) (fontSmall : Font) : CanvasM Unit := do
  let d := state.decomp

  -- Draw basis vectors showing the decomposition
  let e1 := Vec2.mk 1.0 0.0
  let e2 := Vec2.mk 0.0 1.0

  -- Step 1: Original basis vectors after full transform
  let fullE1 := state.matrix.transformVec2 e1
  let fullE2 := state.matrix.transformVec2 e2
  drawVectorArrow Vec2.zero fullE1 origin scale
    { color := Color.rgba 1.0 0.3 0.3 0.8, lineWidth := 3.0 }
  drawVectorArrow Vec2.zero fullE2 origin scale
    { color := Color.rgba 0.3 1.0 0.3 0.8, lineWidth := 3.0 }

  -- Label the transformed basis vectors
  let (e1x, e1y) := worldToScreen fullE1 origin scale
  let (e2x, e2y) := worldToScreen fullE2 origin scale
  setFillColor (Color.rgba 1.0 0.3 0.3 0.9)
  fillTextXY "e1'" (e1x + 8) (e1y - 8) fontSmall
  setFillColor (Color.rgba 0.3 1.0 0.3 0.9)
  fillTextXY "e2'" (e2x + 8) (e2y - 8) fontSmall

  -- Draw original unit basis (ghosted)
  drawVectorArrow Vec2.zero e1 origin scale
    { color := Color.rgba 0.5 0.5 0.5 0.4, lineWidth := 1.5 }
  drawVectorArrow Vec2.zero e2 origin scale
    { color := Color.rgba 0.5 0.5 0.5 0.4, lineWidth := 1.5 }

/-- Render the matrix decomposition visualization -/
def renderMatrixDecomposition (state : MatrixDecompositionState)
    (w h : Float) (screenScale : Float) (fontMedium fontSmall : Font) : CanvasM Unit := do
  let origin : Float × Float := (w / 2, h / 2)
  let scale : Float := 80.0 * screenScale

  -- Draw background grid
  setStrokeColor (Color.gray 0.15)
  setLineWidth 1.0
  for i in [:13] do
    let offset := (i.toFloat - 6.0) * 0.5 * scale
    let path := Afferent.Path.empty
      |>.moveTo (Point.mk (origin.1 - 3.0 * scale) (origin.2 + offset))
      |>.lineTo (Point.mk (origin.1 + 3.0 * scale) (origin.2 + offset))
    strokePath path
    let path2 := Afferent.Path.empty
      |>.moveTo (Point.mk (origin.1 + offset) (origin.2 - 3.0 * scale))
      |>.lineTo (Point.mk (origin.1 + offset) (origin.2 + 3.0 * scale))
    strokePath path2

  -- Draw axes
  setStrokeColor (Color.gray 0.4)
  setLineWidth 1.5
  let axisPath := Afferent.Path.empty
    |>.moveTo (Point.mk (origin.1 - 3.0 * scale) origin.2)
    |>.lineTo (Point.mk (origin.1 + 3.0 * scale) origin.2)
  strokePath axisPath
  let axisPath2 := Afferent.Path.empty
    |>.moveTo (Point.mk origin.1 (origin.2 - 3.0 * scale))
    |>.lineTo (Point.mk origin.1 (origin.2 + 3.0 * scale))
  strokePath axisPath2

  -- Get the matrix for current visualization step
  let displayMatrix := getStepMatrix state

  -- Draw original unit circle (ghosted)
  drawUnitCircle Mat2.identity origin scale (Color.rgba 0.5 0.5 0.5 0.3) true

  -- Draw transformed unit circle (becomes ellipse)
  drawUnitCircle displayMatrix origin scale (Color.rgba 0.3 0.7 1.0 0.8) true

  -- Draw decomposition components
  if state.showComponents then
    drawDecompositionComponents state origin scale fontSmall

  -- Draw origin marker
  setFillColor Color.white
  fillPath (Afferent.Path.circle (Point.mk origin.1 origin.2) 4.0)

  -- Info panel (left side)
  let infoX := 20.0 * screenScale
  let infoY := h - 200.0 * screenScale

  setFillColor VecColor.label
  fillTextXY "Matrix M:" infoX infoY fontSmall

  -- Draw 2x2 matrix values
  setFillColor Color.white
  let m := state.matrix
  fillTextXY s!"[{formatFloat (m.get 0 0)}  {formatFloat (m.get 0 1)}]" infoX (infoY + 22 * screenScale) fontSmall
  fillTextXY s!"[{formatFloat (m.get 1 0)}  {formatFloat (m.get 1 1)}]" infoX (infoY + 42 * screenScale) fontSmall

  -- Decomposition info
  let d := state.decomp
  setFillColor (Color.gray 0.7)
  fillTextXY "Decomposition:" infoX (infoY + 72 * screenScale) fontSmall

  let angleDeg1 := d.rotation1 * 180 / Float.pi
  setFillColor (Color.rgba 0.8 0.8 0.3 0.9)
  fillTextXY s!"Rotation: {formatFloat angleDeg1}" infoX (infoY + 92 * screenScale) fontSmall

  setFillColor (Color.rgba 0.3 0.8 0.8 0.9)
  fillTextXY s!"Scale X: {formatFloat d.scaleX}" infoX (infoY + 112 * screenScale) fontSmall
  fillTextXY s!"Scale Y: {formatFloat d.scaleY}" infoX (infoY + 132 * screenScale) fontSmall

  -- Determinant
  let det := state.matrix.determinant
  let detColor := if det >= 0 then Color.green else Color.red
  setFillColor detColor
  fillTextXY s!"det(M) = {formatFloat det}" infoX (infoY + 160 * screenScale) fontSmall

  -- Current step indicator
  let stepName := match state.currentStep with
    | .original => "Original Transform"
    | .afterRotation1 => "After Undo Rotation"
    | .afterScale => "After Undo Scale"
    | .afterRotation2 => "Identity"
  setFillColor VecColor.label
  fillTextXY s!"View: {stepName}" infoX (infoY + 185 * screenScale) fontSmall

  -- Preset info (right side)
  let presetX := w - 200.0 * screenScale
  let presetY := 100.0 * screenScale
  setFillColor (Color.gray 0.5)
  fillTextXY "Presets (1-6):" presetX presetY fontSmall
  for i in [:decompositionPresets.size] do
    let (name, _) := decompositionPresets.getD i ("", Mat2.identity)
    let isSelected := state.presetIndex == i
    setFillColor (if isSelected then Color.yellow else Color.gray 0.6)
    fillTextXY s!"{i + 1}. {name}" presetX (presetY + (i.toFloat + 1) * 18.0 * screenScale) fontSmall

  -- Title and instructions
  setFillColor VecColor.label
  fillTextXY "MATRIX DECOMPOSITION" (20 * screenScale) (30 * screenScale) fontMedium
  setFillColor (Color.gray 0.6)
  fillTextXY "1-6: Presets | Tab: Decomposition steps | C: Toggle components" (20 * screenScale) (55 * screenScale) fontSmall

/-- Create the matrix decomposition widget -/
def matrixDecompositionWidget (env : DemoEnv) (state : MatrixDecompositionState)
    : Afferent.Arbor.WidgetBuilder := do
  Afferent.Arbor.custom (spec := {
    measure := fun _ _ => (0, 0)
    collect := fun _ => #[]
    draw := some (fun layout => do
      withContentRect layout fun w h => do
        resetTransform
        renderMatrixDecomposition state w h env.screenScale env.fontMedium env.fontSmall
    )
  }) (style := { flexItem := some (Trellis.FlexItem.growing 1) })

end Demos.Linalg
