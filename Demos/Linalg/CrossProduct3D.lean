/-
  Cross Product 3D Demo - Shows 3D cross product with rotatable view.
  Drag to rotate camera, shows perpendicularity of cross product.
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

open Afferent CanvasM Linalg

namespace Demos.Linalg

/-- Drag mode for 3D demo -/
inductive CrossDragMode where
  | none
  | camera
  | vectorA
  | vectorB
  deriving BEq, Inhabited

/-- State for cross product 3D demo -/
structure CrossProduct3DState where
  vectorA : Vec3 := Vec3.mk 2.0 0.5 0.0
  vectorB : Vec3 := Vec3.mk 0.5 2.0 1.0
  cameraYaw : Float := 0.6
  cameraPitch : Float := 0.4
  dragging : CrossDragMode := .none
  lastMouseX : Float := 0.0
  lastMouseY : Float := 0.0
  showParallelogram : Bool := true
  deriving Inhabited

def crossProduct3DInitialState : CrossProduct3DState := {}

/-- Project a 3D point to 2D screen using orthographic projection with camera rotation -/
def project3Dto2D (p : Vec3) (yaw pitch : Float) (origin : Float × Float) (scale : Float) : Float × Float :=
  -- Rotate around Y axis (yaw)
  let cosY := Float.cos yaw
  let sinY := Float.sin yaw
  let x1 := p.x * cosY + p.z * sinY
  let z1 := -p.x * sinY + p.z * cosY

  -- Rotate around X axis (pitch)
  let cosP := Float.cos pitch
  let sinP := Float.sin pitch
  let y2 := p.y * cosP - z1 * sinP
  let _ := p.y * sinP + z1 * cosP  -- z2 (for depth, not used in ortho)

  -- Orthographic projection
  (origin.1 + x1 * scale, origin.2 - y2 * scale)

/-- Draw a 3D arrow (vector from origin to tip) -/
def draw3DArrow (vec : Vec3) (yaw pitch : Float) (origin : Float × Float)
    (scale : Float) (config : ArrowConfig := {}) : CanvasM Unit := do
  let start := project3Dto2D Vec3.zero yaw pitch origin scale
  let finish := project3Dto2D vec yaw pitch origin scale
  drawArrow2D start finish config

/-- Draw XYZ coordinate axes -/
def draw3DAxes (yaw pitch : Float) (origin : Float × Float) (scale : Float)
    (axisLength : Float) (fontSmall : Font) : CanvasM Unit := do
  -- X axis (red)
  let xEnd := Vec3.mk axisLength 0.0 0.0
  draw3DArrow xEnd yaw pitch origin scale { color := VecColor.xAxis, lineWidth := 2.0 }
  let (xx, xy) := project3Dto2D xEnd yaw pitch origin scale
  setFillColor VecColor.xAxis
  fillTextXY "X" (xx + 8) (xy - 8) fontSmall

  -- Y axis (green)
  let yEnd := Vec3.mk 0.0 axisLength 0.0
  draw3DArrow yEnd yaw pitch origin scale { color := VecColor.yAxis, lineWidth := 2.0 }
  let (yx, yy) := project3Dto2D yEnd yaw pitch origin scale
  setFillColor VecColor.yAxis
  fillTextXY "Y" (yx + 8) (yy - 8) fontSmall

  -- Z axis (blue)
  let zEnd := Vec3.mk 0.0 0.0 axisLength
  draw3DArrow zEnd yaw pitch origin scale { color := VecColor.zAxis, lineWidth := 2.0 }
  let (zx, zy) := project3Dto2D zEnd yaw pitch origin scale
  setFillColor VecColor.zAxis
  fillTextXY "Z" (zx + 8) (zy - 8) fontSmall

/-- Draw the parallelogram formed by two vectors (for visualizing cross product magnitude) -/
def drawParallelogram3D (a b : Vec3) (yaw pitch : Float) (origin : Float × Float)
    (scale : Float) (color : Color) : CanvasM Unit := do
  let o := project3Dto2D Vec3.zero yaw pitch origin scale
  let pa := project3Dto2D a yaw pitch origin scale
  let pb := project3Dto2D b yaw pitch origin scale
  let pab := project3Dto2D (a + b) yaw pitch origin scale

  setFillColor color
  let path := Afferent.Path.empty
    |>.moveTo (Point.mk o.1 o.2)
    |>.lineTo (Point.mk pa.1 pa.2)
    |>.lineTo (Point.mk pab.1 pab.2)
    |>.lineTo (Point.mk pb.1 pb.2)
    |>.closePath
  fillPath path

  -- Outline
  setStrokeColor (Color.rgba color.r color.g color.b 0.6)
  setLineWidth 1.0
  strokePath path

/-- Render the cross product 3D visualization -/
def renderCrossProduct3D (state : CrossProduct3DState)
    (w h : Float) (screenScale : Float) (fontMedium fontSmall : Font) : CanvasM Unit := do
  let origin : Float × Float := (w / 2, h / 2)
  let scale : Float := 60.0 * screenScale

  -- Background grid (simple)
  setStrokeColor (Color.gray 0.15)
  setLineWidth 1.0
  let gridSize := 8
  let gridScale := scale * 0.5
  for i in [:gridSize * 2 + 1] do
    let offset := (i.toFloat - gridSize.toFloat) * gridScale
    -- Horizontal lines (in XZ plane projected)
    let p1 := project3Dto2D (Vec3.mk (-gridSize.toFloat) 0.0 (offset / gridScale)) state.cameraYaw state.cameraPitch origin gridScale
    let p2 := project3Dto2D (Vec3.mk gridSize.toFloat 0.0 (offset / gridScale)) state.cameraYaw state.cameraPitch origin gridScale
    let path := Afferent.Path.empty
      |>.moveTo (Point.mk p1.1 p1.2)
      |>.lineTo (Point.mk p2.1 p2.2)
    strokePath path
    -- Vertical lines (in XZ plane)
    let p3 := project3Dto2D (Vec3.mk (offset / gridScale) 0.0 (-gridSize.toFloat)) state.cameraYaw state.cameraPitch origin gridScale
    let p4 := project3Dto2D (Vec3.mk (offset / gridScale) 0.0 gridSize.toFloat) state.cameraYaw state.cameraPitch origin gridScale
    let path2 := Afferent.Path.empty
      |>.moveTo (Point.mk p3.1 p3.2)
      |>.lineTo (Point.mk p4.1 p4.2)
    strokePath path2

  -- Draw coordinate axes
  draw3DAxes state.cameraYaw state.cameraPitch origin scale 3.5 fontSmall

  -- Calculate cross product
  let cross := Vec3.cross state.vectorA state.vectorB
  let crossMag := cross.length

  -- Draw parallelogram if enabled
  if state.showParallelogram then
    drawParallelogram3D state.vectorA state.vectorB state.cameraYaw state.cameraPitch
      origin scale (Color.rgba 0.8 0.8 0.0 0.2)

  -- Draw vector A (cyan)
  draw3DArrow state.vectorA state.cameraYaw state.cameraPitch origin scale
    { color := VecColor.vectorA, lineWidth := 3.0 }
  let screenA := project3Dto2D state.vectorA state.cameraYaw state.cameraPitch origin scale
  setFillColor VecColor.vectorA
  fillTextXY "A" (screenA.1 + 10) (screenA.2 - 10) fontSmall

  -- Draw vector B (magenta)
  draw3DArrow state.vectorB state.cameraYaw state.cameraPitch origin scale
    { color := VecColor.vectorB, lineWidth := 3.0 }
  let screenB := project3Dto2D state.vectorB state.cameraYaw state.cameraPitch origin scale
  setFillColor VecColor.vectorB
  fillTextXY "B" (screenB.1 + 10) (screenB.2 - 10) fontSmall

  -- Draw cross product vector (yellow)
  if crossMag > 0.01 then
    draw3DArrow cross state.cameraYaw state.cameraPitch origin scale
      { color := VecColor.reflection, lineWidth := 3.5 }
    let screenC := project3Dto2D cross state.cameraYaw state.cameraPitch origin scale
    setFillColor VecColor.reflection
    fillTextXY "A×B" (screenC.1 + 10) (screenC.2 - 10) fontSmall

  -- Draw origin marker
  let (ox, oy) := project3Dto2D Vec3.zero state.cameraYaw state.cameraPitch origin scale
  setFillColor Color.white
  fillPath (Afferent.Path.circle (Point.mk ox oy) 4.0)

  -- Info panel
  let infoY := h - 160 * screenScale
  setFillColor VecColor.label
  fillTextXY s!"A = {formatVec3 state.vectorA}" (20 * screenScale) infoY fontSmall
  fillTextXY s!"B = {formatVec3 state.vectorB}" (20 * screenScale) (infoY + 22 * screenScale) fontSmall
  fillTextXY s!"A × B = {formatVec3 cross}" (20 * screenScale) (infoY + 44 * screenScale) fontSmall
  fillTextXY s!"|A × B| = {formatFloat crossMag}  (parallelogram area)" (20 * screenScale) (infoY + 66 * screenScale) fontSmall

  -- Verify perpendicularity
  let dotAC := Vec3.dot state.vectorA cross
  let dotBC := Vec3.dot state.vectorB cross
  setFillColor (Color.gray 0.7)
  fillTextXY s!"A · (A×B) = {formatFloat dotAC}  (should be 0)" (20 * screenScale) (infoY + 88 * screenScale) fontSmall
  fillTextXY s!"B · (A×B) = {formatFloat dotBC}  (should be 0)" (20 * screenScale) (infoY + 110 * screenScale) fontSmall

  -- Camera info
  fillTextXY s!"Camera: yaw={formatFloat state.cameraYaw}, pitch={formatFloat state.cameraPitch}" (20 * screenScale) (infoY + 132 * screenScale) fontSmall

  -- Title and instructions
  setFillColor VecColor.label
  fillTextXY "3D CROSS PRODUCT" (20 * screenScale) (30 * screenScale) fontMedium
  setFillColor (Color.gray 0.6)
  fillTextXY "Drag: rotate view | P: toggle parallelogram | R: reset camera" (20 * screenScale) (55 * screenScale) fontSmall

/-- Create the cross product 3D widget -/
def crossProduct3DWidget (env : DemoEnv) (state : CrossProduct3DState)
    : Afferent.Arbor.WidgetBuilder := do
  Afferent.Arbor.custom (spec := {
    measure := fun _ _ => (0, 0)
    collect := fun _ => #[]
    draw := some (fun layout => do
      withContentRect layout fun w h => do
        resetTransform
        renderCrossProduct3D state w h env.screenScale env.fontMedium env.fontSmall
    )
  }) (style := { flexItem := some (Trellis.FlexItem.growing 1) })

end Demos.Linalg
