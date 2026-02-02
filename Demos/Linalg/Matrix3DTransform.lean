/-
  Matrix 3D Transform Chain - Visualize 3D transform composition.
  Shows a stack of transforms applied to a 3D object, demonstrating non-commutativity.
  Drag to rotate view, reorder transforms to see different results.
-/
import Afferent
import Afferent.Widget
import Afferent.Arbor
import Demos.Core.Demo
import Demos.Linalg.Shared
import Trellis
import Linalg.Core
import Linalg.Vec2
import Linalg.Vec3
import Linalg.Mat4

open Afferent CanvasM Linalg
open Afferent.Widget

namespace Demos.Linalg

/-- Types of 3D transforms -/
inductive Transform3DType where
  | translateX (amount : Float)
  | translateY (amount : Float)
  | translateZ (amount : Float)
  | rotateX (angle : Float)
  | rotateY (angle : Float)
  | rotateZ (angle : Float)
  | scaleUniform (factor : Float)
  | scaleNonUniform (x y z : Float)
  deriving BEq, Inhabited

/-- Get display name for transform type -/
def Transform3DType.name : Transform3DType -> String
  | .translateX a => s!"Translate X ({formatFloat a})"
  | .translateY a => s!"Translate Y ({formatFloat a})"
  | .translateZ a => s!"Translate Z ({formatFloat a})"
  | .rotateX a => s!"Rotate X ({formatFloat (a * 180 / Float.pi)})"
  | .rotateY a => s!"Rotate Y ({formatFloat (a * 180 / Float.pi)})"
  | .rotateZ a => s!"Rotate Z ({formatFloat (a * 180 / Float.pi)})"
  | .scaleUniform f => s!"Scale ({formatFloat f})"
  | .scaleNonUniform x y z => s!"Scale ({formatFloat x}, {formatFloat y}, {formatFloat z})"

/-- Convert transform type to matrix -/
def Transform3DType.toMatrix : Transform3DType -> Mat4
  | .translateX a => Mat4.translation a 0 0
  | .translateY a => Mat4.translation 0 a 0
  | .translateZ a => Mat4.translation 0 0 a
  | .rotateX a => Mat4.rotationX a
  | .rotateY a => Mat4.rotationY a
  | .rotateZ a => Mat4.rotationZ a
  | .scaleUniform f => Mat4.scalingUniform f
  | .scaleNonUniform x y z => Mat4.scaling x y z

/-- State for the 3D transform chain demo -/
structure Matrix3DTransformState where
  transforms : Array Transform3DType := #[
    .rotateY (Float.pi / 4),
    .translateX 1.5,
    .scaleUniform 0.8
  ]
  selectedIndex : Option Nat := none
  cameraYaw : Float := 0.5
  cameraPitch : Float := 0.3
  dragging : Bool := false
  lastMouseX : Float := 0.0
  lastMouseY : Float := 0.0
  showAxes : Bool := true
  showIntermediateSteps : Bool := true
  deriving Inhabited

def matrix3DTransformInitialState : Matrix3DTransformState := {}

def matrix3DTransformMathViewConfig (state : Matrix3DTransformState) (screenScale : Float)
    : MathView3D.Config := {
  style := { flexItem := some (Trellis.FlexItem.growing 1) }
  camera := { yaw := state.cameraYaw, pitch := state.cameraPitch, distance := 8.0 }
  gridExtent := 2.5
  gridStep := 0.5
  gridMajorStep := 1.0
  showAxes := false
  showAxisLabels := false
  axisLineWidth := 2.0 * screenScale
  gridLineWidth := 1.0 * screenScale
}

/-- Compose all transforms in order -/
def composeTransforms (transforms : Array Transform3DType) : Mat4 :=
  transforms.foldl (fun acc t => t.toMatrix * acc) Mat4.identity

/-- Get a cube's vertices (centered at origin, size 1) -/
def getCubeVertices : Array Vec3 := #[
  Vec3.mk (-0.5) (-0.5) (-0.5), Vec3.mk 0.5 (-0.5) (-0.5),
  Vec3.mk 0.5 0.5 (-0.5), Vec3.mk (-0.5) 0.5 (-0.5),
  Vec3.mk (-0.5) (-0.5) 0.5, Vec3.mk 0.5 (-0.5) 0.5,
  Vec3.mk 0.5 0.5 0.5, Vec3.mk (-0.5) 0.5 0.5
]

/-- Cube edge indices (pairs of vertex indices) -/
def getCubeEdges : Array (Nat × Nat) := #[
  (0, 1), (1, 2), (2, 3), (3, 0),  -- Back face
  (4, 5), (5, 6), (6, 7), (7, 4),  -- Front face
  (0, 4), (1, 5), (2, 6), (3, 7)   -- Connecting edges
]

/-- Project 3D point to 2D with camera rotation -/
def projectPoint (view : MathView3D.View) (p : Vec3) : Option (Float × Float) :=
  MathView3D.worldToScreen view p

/-- Draw a wireframe cube -/
def drawWireframeCube (view : MathView3D.View) (matrix : Mat4)
    (color : Color) (lineWidth : Float := 2.0) : CanvasM Unit := do
  let vertices := getCubeVertices
  let edges := getCubeEdges

  -- Transform vertices
  let transformed := vertices.map (matrix.transformPoint ·)
  for (i, j) in edges do
    MathView3D.drawLine3D view (transformed.getD i Vec3.zero) (transformed.getD j Vec3.zero)
      color lineWidth

/-- Draw coordinate axes in 3D -/
def draw3DCoordinateAxes (view : MathView3D.View) (matrix : Mat4) (axisLength : Float)
    (screenScale : Float) : CanvasM Unit := do
  let origin3D := matrix.transformPoint Vec3.zero
  let xEnd := matrix.transformPoint (Vec3.mk axisLength 0 0)
  let yEnd := matrix.transformPoint (Vec3.mk 0 axisLength 0)
  let zEnd := matrix.transformPoint (Vec3.mk 0 0 axisLength)

  match projectPoint view origin3D, projectPoint view xEnd with
  | some screenO, some screenX =>
      drawArrow2D screenO screenX { color := VecColor.xAxis, lineWidth := 2.0 * screenScale }
  | _, _ => pure ()
  match projectPoint view origin3D, projectPoint view yEnd with
  | some screenO, some screenY =>
      drawArrow2D screenO screenY { color := VecColor.yAxis, lineWidth := 2.0 * screenScale }
  | _, _ => pure ()
  match projectPoint view origin3D, projectPoint view zEnd with
  | some screenO, some screenZ =>
      drawArrow2D screenO screenZ { color := VecColor.zAxis, lineWidth := 2.0 * screenScale }
  | _, _ => pure ()

/-- Render the 3D transform chain visualization -/
def renderMatrix3DTransform (state : Matrix3DTransformState)
    (view : MathView3D.View) (screenScale : Float) (fontMedium fontSmall : Font) : CanvasM Unit := do
  let w := view.width
  let h := view.height

  -- Draw world axes
  if state.showAxes then
    draw3DCoordinateAxes view Mat4.identity 3.0 screenScale

  -- Draw intermediate steps if enabled
  if state.showIntermediateSteps then
    for i in [:state.transforms.size] do
      let partialTransforms := state.transforms.toList.take (i + 1) |>.toArray
      let partialMatrix := composeTransforms partialTransforms
      let alpha := 0.2 + (i.toFloat / state.transforms.size.toFloat) * 0.3
      drawWireframeCube view partialMatrix (Color.rgba 0.5 0.5 1.0 alpha) (1.0 * screenScale)

  -- Draw original cube (ghosted)
  drawWireframeCube view Mat4.identity (Color.rgba 0.5 0.5 0.5 0.3) (1.0 * screenScale)

  -- Draw fully transformed cube
  let finalMatrix := composeTransforms state.transforms
  drawWireframeCube view finalMatrix (Color.rgba 0.3 0.8 1.0 0.9) (2.5 * screenScale)

  -- Draw local axes on transformed cube
  if state.showAxes then
    draw3DCoordinateAxes view finalMatrix 1.0 screenScale

  -- Transform list panel (left side)
  let panelX := 20.0 * screenScale
  let panelY := 100.0 * screenScale

  setFillColor VecColor.label
  fillTextXY "Transform Stack:" panelX panelY fontSmall

  for i in [:state.transforms.size] do
    let ty := panelY + (i.toFloat + 1) * 24.0 * screenScale
    let transform := state.transforms.getD i (.rotateX 0)
    let isSelected := state.selectedIndex == some i

    -- Highlight if selected
    if isSelected then
      setFillColor (Color.rgba 0.3 0.5 0.8 0.3)
      fillPath (Afferent.Path.rectangleXYWH (panelX - 5) (ty - 16) 220 22)

    -- Draw index and transform name
    let indexColor := if isSelected then Color.yellow else Color.gray 0.6
    setFillColor indexColor
    fillTextXY s!"{i + 1}." panelX ty fontSmall

    setFillColor (if isSelected then Color.white else Color.gray 0.8)
    fillTextXY (transform.name) (panelX + 25 * screenScale) ty fontSmall

  -- Instructions for reordering
  setFillColor (Color.gray 0.5)
  let instrY := panelY + (state.transforms.size.toFloat + 2) * 24.0 * screenScale
  fillTextXY "Up/Down: Move selected" panelX instrY fontSmall
  fillTextXY "1-8: Select transform" panelX (instrY + 18 * screenScale) fontSmall

  -- Show non-commutativity hint
  setFillColor (Color.gray 0.6)
  let hintY := h - 60.0 * screenScale
  fillTextXY "Transforms are applied in order (top to bottom)." panelX hintY fontSmall
  fillTextXY "Reorder to see how order affects the result!" panelX (hintY + 18 * screenScale) fontSmall

  -- Title and instructions
  setFillColor VecColor.label
  fillTextXY "3D TRANSFORM CHAIN" (20 * screenScale) (30 * screenScale) fontMedium
  setFillColor (Color.gray 0.6)
  fillTextXY "Drag: Rotate view | A: Axes | I: Intermediate | R: Reset | Arrows: Reorder" (20 * screenScale) (55 * screenScale) fontSmall

/-- Create the 3D transform chain widget -/
def matrix3DTransformWidget (env : DemoEnv) (state : Matrix3DTransformState)
    : Afferent.Arbor.WidgetBuilder := do
  let config := matrix3DTransformMathViewConfig state env.screenScale
  MathView3D.mathView3D config env.fontSmall (fun view => do
    renderMatrix3DTransform state view env.screenScale env.fontMedium env.fontSmall
  )

end Demos.Linalg
