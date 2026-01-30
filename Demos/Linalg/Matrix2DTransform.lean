/-
  Matrix 2D Transform Playground - Interactive matrix transformation visualization.
  Shows a shape before/after transformation, editable matrix cells, and presets.
  Demonstrates Mat2, Affine2D, determinant, basis vectors.
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
import Linalg.Affine2D

open Afferent CanvasM Linalg

namespace Demos.Linalg

/-- Transform presets available in the demo -/
inductive TransformPreset where
  | identity
  | rotation45
  | rotation90
  | scale2x
  | scaleNonUniform
  | shearX
  | shearY
  | reflectX
  | reflectY
  | custom
  deriving BEq, Inhabited

/-- Shape types for visualization -/
inductive ShapeType where
  | square
  | triangle
  | arrow
  deriving BEq, Inhabited

/-- Cell being edited in the matrix -/
inductive MatrixCell where
  | none
  | m00 | m01
  | m10 | m11
  deriving BEq, Inhabited

/-- State for the 2D matrix transform demo -/
structure Matrix2DTransformState where
  matrix : Mat2 := Mat2.identity
  preset : TransformPreset := .identity
  shape : ShapeType := .square
  showGrid : Bool := true
  showBasisVectors : Bool := true
  animating : Bool := false
  animT : Float := 1.0  -- 0 = identity, 1 = full transform
  editingCell : MatrixCell := .none
  deriving Inhabited

def matrix2DTransformInitialState : Matrix2DTransformState := {}

/-- Get the matrix for a preset -/
def presetToMatrix : TransformPreset -> Mat2
  | .identity => Mat2.identity
  | .rotation45 => Mat2.rotation (Float.pi / 4)
  | .rotation90 => Mat2.rotation (Float.pi / 2)
  | .scale2x => Mat2.scaling 2.0 2.0
  | .scaleNonUniform => Mat2.scaling 1.5 0.5
  | .shearX => Mat2.fromRows (Vec2.mk 1.0 0.5) (Vec2.mk 0.0 1.0)
  | .shearY => Mat2.fromRows (Vec2.mk 1.0 0.0) (Vec2.mk 0.5 1.0)
  | .reflectX => Mat2.scaling 1.0 (-1.0)
  | .reflectY => Mat2.scaling (-1.0) 1.0
  | .custom => Mat2.identity

/-- Get display name for a preset -/
def presetName : TransformPreset -> String
  | .identity => "Identity"
  | .rotation45 => "Rotate 45"
  | .rotation90 => "Rotate 90"
  | .scale2x => "Scale 2x"
  | .scaleNonUniform => "Scale 1.5x0.5"
  | .shearX => "Shear X"
  | .shearY => "Shear Y"
  | .reflectX => "Reflect X"
  | .reflectY => "Reflect Y"
  | .custom => "Custom"

/-- Lerp between identity and target matrix -/
def lerpMatrix (target : Mat2) (t : Float) : Mat2 :=
  let identity := Mat2.identity
  let result := Id.run do
    let mut arr : Array Float := #[]
    for i in [:4] do
      let a := identity.data.getD i 0.0
      let b := target.data.getD i 0.0
      arr := arr.push (Float.lerp a b t)
    return arr
  { data := result }

/-- Get the vertices of a shape (unit coordinates) -/
def getShapeVertices : ShapeType -> Array Vec2
  | .square => #[Vec2.mk (-0.5) (-0.5), Vec2.mk 0.5 (-0.5), Vec2.mk 0.5 0.5, Vec2.mk (-0.5) 0.5]
  | .triangle => #[Vec2.mk 0.0 0.6, Vec2.mk (-0.5) (-0.4), Vec2.mk 0.5 (-0.4)]
  | .arrow => #[Vec2.mk 0.0 0.6, Vec2.mk (-0.3) 0.1, Vec2.mk (-0.15) 0.1, Vec2.mk (-0.15) (-0.5),
                Vec2.mk 0.15 (-0.5), Vec2.mk 0.15 0.1, Vec2.mk 0.3 0.1]

/-- Draw a transformed grid -/
def drawTransformedGrid (matrix : Mat2) (origin : Float × Float) (scale : Float)
    (gridColor : Color) : CanvasM Unit := do
  setStrokeColor gridColor
  setLineWidth 1.0

  -- Draw horizontal grid lines
  for i in [:11] do
    let y := (i.toFloat - 5.0) * 0.5
    let start := matrix.transformVec2 (Vec2.mk (-3.0) y)
    let finish := matrix.transformVec2 (Vec2.mk 3.0 y)
    let screenStart := worldToScreen start origin scale
    let screenEnd := worldToScreen finish origin scale
    let path := Afferent.Path.empty
      |>.moveTo (Point.mk screenStart.1 screenStart.2)
      |>.lineTo (Point.mk screenEnd.1 screenEnd.2)
    strokePath path

  -- Draw vertical grid lines
  for i in [:11] do
    let x := (i.toFloat - 5.0) * 0.5
    let start := matrix.transformVec2 (Vec2.mk x (-3.0))
    let finish := matrix.transformVec2 (Vec2.mk x 3.0)
    let screenStart := worldToScreen start origin scale
    let screenEnd := worldToScreen finish origin scale
    let path := Afferent.Path.empty
      |>.moveTo (Point.mk screenStart.1 screenStart.2)
      |>.lineTo (Point.mk screenEnd.1 screenEnd.2)
    strokePath path

/-- Draw a shape with given transformation -/
def drawTransformedShape (vertices : Array Vec2) (matrix : Mat2) (origin : Float × Float)
    (scale : Float) (fillColor : Color) (strokeColor : Color) : CanvasM Unit := do
  if vertices.size < 3 then return

  -- Transform all vertices
  let transformed := vertices.map matrix.transformVec2
  let screenVerts := transformed.map (worldToScreen · origin scale)

  -- Build path
  let firstVert := screenVerts.getD 0 (0.0, 0.0)
  let mut path := Afferent.Path.empty |>.moveTo (Point.mk firstVert.1 firstVert.2)
  for i in [1:screenVerts.size] do
    let v := screenVerts.getD i (0.0, 0.0)
    path := path.lineTo (Point.mk v.1 v.2)
  path := path.closePath

  -- Fill and stroke
  setFillColor fillColor
  fillPath path
  setStrokeColor strokeColor
  setLineWidth 2.0
  strokePath path

/-- Draw basis vectors showing how unit vectors are transformed -/
def drawBasisVectors (matrix : Mat2) (origin : Float × Float) (scale : Float) : CanvasM Unit := do
  let e1 := matrix.transformVec2 (Vec2.mk 1.0 0.0)
  let e2 := matrix.transformVec2 (Vec2.mk 0.0 1.0)

  -- Draw e1 (transformed x-axis) in red
  drawVectorArrow Vec2.zero e1 origin scale
    { color := Color.rgba 1.0 0.3 0.3 0.9, lineWidth := 3.0 }

  -- Draw e2 (transformed y-axis) in green
  drawVectorArrow Vec2.zero e2 origin scale
    { color := Color.rgba 0.3 1.0 0.3 0.9, lineWidth := 3.0 }

/-- Format a matrix element for display -/
def formatMatrixElement (f : Float) : String :=
  let scaled := Float.floor (f * 100 + 0.5) / 100
  if Float.abs scaled < 0.005 then "0.00"
  else s!"{scaled}"

/-- Draw matrix display with values -/
def drawMatrixDisplay (matrix : Mat2) (x y : Float) (screenScale : Float)
    (fontSmall : Font) (editingCell : MatrixCell) : CanvasM Unit := do
  let cellW := 60.0 * screenScale
  let cellH := 30.0 * screenScale
  let padding := 4.0 * screenScale

  -- Draw bracket outline
  setStrokeColor (Color.gray 0.6)
  setLineWidth 2.0
  let bracketPath := Afferent.Path.empty
    |>.moveTo (Point.mk (x + padding) y)
    |>.lineTo (Point.mk x y)
    |>.lineTo (Point.mk x (y + cellH * 2 + padding * 2))
    |>.lineTo (Point.mk (x + padding) (y + cellH * 2 + padding * 2))
  strokePath bracketPath
  let rightX := x + cellW * 2 + padding * 2
  let bracketPath2 := Afferent.Path.empty
    |>.moveTo (Point.mk (rightX - padding) y)
    |>.lineTo (Point.mk rightX y)
    |>.lineTo (Point.mk rightX (y + cellH * 2 + padding * 2))
    |>.lineTo (Point.mk (rightX - padding) (y + cellH * 2 + padding * 2))
  strokePath bracketPath2

  -- Draw cell values
  let cells := #[(0, 0, MatrixCell.m00), (0, 1, MatrixCell.m01),
                 (1, 0, MatrixCell.m10), (1, 1, MatrixCell.m11)]
  for (row, col, cell) in cells do
    let cx := x + padding + col.toFloat * cellW
    let cy := y + padding + row.toFloat * cellH
    let value := matrix.get row col

    -- Highlight if editing
    if editingCell == cell then
      setFillColor (Color.rgba 0.3 0.5 0.8 0.3)
      fillPath (Afferent.Path.rectangleXYWH cx cy cellW cellH)

    -- Draw value
    setFillColor Color.white
    let text := formatMatrixElement value
    let (tw, _) ← fontSmall.measureText text
    fillTextXY text (cx + (cellW - tw) / 2) (cy + cellH * 0.7) fontSmall

/-- Render the 2D matrix transform visualization -/
def renderMatrix2DTransform (state : Matrix2DTransformState)
    (w h : Float) (screenScale : Float) (fontMedium fontSmall : Font) : CanvasM Unit := do
  let origin : Float × Float := (w / 2, h / 2)
  let scale : Float := 80.0 * screenScale

  -- Get effective matrix (with animation interpolation)
  let effectiveMatrix := lerpMatrix state.matrix state.animT

  -- Draw background grid (identity)
  if state.showGrid then
    setStrokeColor (Color.gray 0.15)
    setLineWidth 1.0
    for i in [:13] do
      let offset := (i.toFloat - 6.0) * 0.5 * scale
      -- Horizontal
      let path := Afferent.Path.empty
        |>.moveTo (Point.mk (origin.1 - 3.0 * scale) (origin.2 + offset))
        |>.lineTo (Point.mk (origin.1 + 3.0 * scale) (origin.2 + offset))
      strokePath path
      -- Vertical
      let path2 := Afferent.Path.empty
        |>.moveTo (Point.mk (origin.1 + offset) (origin.2 - 3.0 * scale))
        |>.lineTo (Point.mk (origin.1 + offset) (origin.2 + 3.0 * scale))
      strokePath path2

  -- Draw transformed grid
  if state.showGrid then
    drawTransformedGrid effectiveMatrix origin scale (Color.rgba 0.3 0.6 0.8 0.3)

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

  -- Draw original shape (ghosted)
  let vertices := getShapeVertices state.shape
  drawTransformedShape vertices Mat2.identity origin scale
    (Color.rgba 0.5 0.5 0.5 0.2) (Color.rgba 0.5 0.5 0.5 0.4)

  -- Draw transformed shape
  drawTransformedShape vertices effectiveMatrix origin scale
    (Color.rgba 0.2 0.6 1.0 0.4) (Color.rgba 0.4 0.8 1.0 0.9)

  -- Draw basis vectors
  if state.showBasisVectors then
    drawBasisVectors effectiveMatrix origin scale

  -- Draw origin marker
  setFillColor Color.white
  fillPath (Afferent.Path.circle (Point.mk origin.1 origin.2) 4.0)

  -- Info panel (left side)
  let infoX := 20.0 * screenScale
  let infoY := h - 180.0 * screenScale

  setFillColor VecColor.label
  fillTextXY "Matrix:" infoX infoY fontSmall
  drawMatrixDisplay state.matrix infoX (infoY + 20 * screenScale) screenScale fontSmall state.editingCell

  -- Determinant and info
  let det := state.matrix.determinant
  let detColor := if det >= 0.0 then Color.green else Color.red
  setFillColor detColor
  fillTextXY s!"det = {formatFloat det}" infoX (infoY + 100 * screenScale) fontSmall

  setFillColor (Color.gray 0.7)
  let detInfo := if Float.abs det < 0.001 then "(singular - no inverse)"
                 else if det < 0.0 then "(orientation reversed)"
                 else "(orientation preserved)"
  fillTextXY detInfo infoX (infoY + 122 * screenScale) fontSmall

  -- Preset info
  setFillColor VecColor.label
  fillTextXY s!"Preset: {presetName state.preset}" infoX (infoY + 150 * screenScale) fontSmall

  -- Title and instructions
  fillTextXY "2D MATRIX TRANSFORM" (20 * screenScale) (30 * screenScale) fontMedium
  setFillColor (Color.gray 0.6)
  fillTextXY "1-9: Presets | G: Grid | V: Vectors | S: Shape | Space: Animate" (20 * screenScale) (55 * screenScale) fontSmall

  -- Preset reference (right side)
  let refX := w - 180.0 * screenScale
  let refY := 80.0 * screenScale
  setFillColor (Color.gray 0.5)
  fillTextXY "Presets:" refX refY fontSmall
  let presets := #["1: Identity", "2: Rotate 45", "3: Rotate 90", "4: Scale 2x",
                   "5: Scale 1.5x0.5", "6: Shear X", "7: Shear Y", "8: Reflect X", "9: Reflect Y"]
  for i in [:presets.size] do
    fillTextXY (presets.getD i "") refX (refY + (i.toFloat + 1) * 18.0 * screenScale) fontSmall

/-- Create the 2D matrix transform widget -/
def matrix2DTransformWidget (env : DemoEnv) (state : Matrix2DTransformState)
    : Afferent.Arbor.WidgetBuilder := do
  Afferent.Arbor.custom (spec := {
    measure := fun _ _ => (0, 0)
    collect := fun _ => #[]
    draw := some (fun layout => do
      withContentRect layout fun w h => do
        resetTransform
        renderMatrix2DTransform state w h env.screenScale env.fontMedium env.fontSmall
    )
  }) (style := { flexItem := some (Trellis.FlexItem.growing 1) })

end Demos.Linalg
