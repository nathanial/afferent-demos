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

/-- Compose all transforms in order -/
def composeTransforms (transforms : Array Transform3DType) : Mat4 :=
  transforms.foldl (fun acc t => acc * t.toMatrix) Mat4.identity

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
def projectPoint (p : Vec3) (yaw pitch : Float) (origin : Float × Float) (scale : Float) : Float × Float :=
  -- Rotate around Y (yaw)
  let cosY := Float.cos yaw
  let sinY := Float.sin yaw
  let x1 := p.x * cosY + p.z * sinY
  let z1 := -p.x * sinY + p.z * cosY

  -- Rotate around X (pitch)
  let cosP := Float.cos pitch
  let sinP := Float.sin pitch
  let y2 := p.y * cosP - z1 * sinP

  (origin.1 + x1 * scale, origin.2 - y2 * scale)

/-- Draw a wireframe cube -/
def drawWireframeCube (matrix : Mat4) (yaw pitch : Float) (origin : Float × Float)
    (scale : Float) (color : Color) (lineWidth : Float := 2.0) : CanvasM Unit := do
  let vertices := getCubeVertices
  let edges := getCubeEdges

  -- Transform vertices
  let transformed := vertices.map (matrix.transformPoint ·)
  let projected := transformed.map (projectPoint · yaw pitch origin scale)

  -- Draw edges
  setStrokeColor color
  setLineWidth lineWidth
  for (i, j) in edges do
    let p1 := projected.getD i (0.0, 0.0)
    let p2 := projected.getD j (0.0, 0.0)
    let path := Afferent.Path.empty
      |>.moveTo (Point.mk p1.1 p1.2)
      |>.lineTo (Point.mk p2.1 p2.2)
    strokePath path

/-- Draw coordinate axes in 3D -/
def draw3DCoordinateAxes (matrix : Mat4) (yaw pitch : Float) (origin : Float × Float)
    (scale : Float) (axisLength : Float) : CanvasM Unit := do
  let origin3D := matrix.transformPoint Vec3.zero
  let xEnd := matrix.transformPoint (Vec3.mk axisLength 0 0)
  let yEnd := matrix.transformPoint (Vec3.mk 0 axisLength 0)
  let zEnd := matrix.transformPoint (Vec3.mk 0 0 axisLength)

  let screenO := projectPoint origin3D yaw pitch origin scale
  let screenX := projectPoint xEnd yaw pitch origin scale
  let screenY := projectPoint yEnd yaw pitch origin scale
  let screenZ := projectPoint zEnd yaw pitch origin scale

  -- X axis (red)
  drawArrow2D screenO screenX { color := VecColor.xAxis, lineWidth := 2.0 }
  -- Y axis (green)
  drawArrow2D screenO screenY { color := VecColor.yAxis, lineWidth := 2.0 }
  -- Z axis (blue)
  drawArrow2D screenO screenZ { color := VecColor.zAxis, lineWidth := 2.0 }

/-- Render the 3D transform chain visualization -/
def renderMatrix3DTransform (state : Matrix3DTransformState)
    (w h : Float) (screenScale : Float) (fontMedium fontSmall : Font) : CanvasM Unit := do
  let origin : Float × Float := (w / 2, h / 2)
  let scale : Float := 60.0 * screenScale

  -- Draw ground grid in XZ plane
  setStrokeColor (Color.gray 0.15)
  setLineWidth 1.0
  for i in [:11] do
    let offset := (i.toFloat - 5.0) * 0.5
    -- Lines along X
    let p1 := projectPoint (Vec3.mk (-2.5) 0 offset) state.cameraYaw state.cameraPitch origin scale
    let p2 := projectPoint (Vec3.mk 2.5 0 offset) state.cameraYaw state.cameraPitch origin scale
    let path := Afferent.Path.empty
      |>.moveTo (Point.mk p1.1 p1.2)
      |>.lineTo (Point.mk p2.1 p2.2)
    strokePath path
    -- Lines along Z
    let p3 := projectPoint (Vec3.mk offset 0 (-2.5)) state.cameraYaw state.cameraPitch origin scale
    let p4 := projectPoint (Vec3.mk offset 0 2.5) state.cameraYaw state.cameraPitch origin scale
    let path2 := Afferent.Path.empty
      |>.moveTo (Point.mk p3.1 p3.2)
      |>.lineTo (Point.mk p4.1 p4.2)
    strokePath path2

  -- Draw world axes
  if state.showAxes then
    draw3DCoordinateAxes Mat4.identity state.cameraYaw state.cameraPitch origin scale 3.0

  -- Draw intermediate steps if enabled
  if state.showIntermediateSteps then
    for i in [:state.transforms.size] do
      let partialTransforms := state.transforms.toList.take (i + 1) |>.toArray
      let partialMatrix := composeTransforms partialTransforms
      let alpha := 0.2 + (i.toFloat / state.transforms.size.toFloat) * 0.3
      drawWireframeCube partialMatrix state.cameraYaw state.cameraPitch origin scale
        (Color.rgba 0.5 0.5 1.0 alpha) 1.0

  -- Draw original cube (ghosted)
  drawWireframeCube Mat4.identity state.cameraYaw state.cameraPitch origin scale
    (Color.rgba 0.5 0.5 0.5 0.3) 1.0

  -- Draw fully transformed cube
  let finalMatrix := composeTransforms state.transforms
  drawWireframeCube finalMatrix state.cameraYaw state.cameraPitch origin scale
    (Color.rgba 0.3 0.8 1.0 0.9) 2.5

  -- Draw local axes on transformed cube
  if state.showAxes then
    draw3DCoordinateAxes finalMatrix state.cameraYaw state.cameraPitch origin scale 1.0

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
  Afferent.Arbor.custom (spec := {
    measure := fun _ _ => (0, 0)
    collect := fun _ => #[]
    draw := some (fun layout => do
      withContentRect layout fun w h => do
        resetTransform
        renderMatrix3DTransform state w h env.screenScale env.fontMedium env.fontSmall
    )
  }) (style := { flexItem := some (Trellis.FlexItem.growing 1) })

end Demos.Linalg
