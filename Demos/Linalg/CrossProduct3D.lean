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
import AfferentMath.Widget.MathView3D

open Afferent CanvasM Linalg
open Afferent.Widget
open AfferentMath.Widget

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

def crossProduct3DMathViewConfig (state : CrossProduct3DState) (screenScale : Float)
    : MathView3D.Config := {
  style := { flexItem := some (Trellis.FlexItem.growing 1) }
  camera := { yaw := state.cameraYaw, pitch := state.cameraPitch, distance := 10.0 }
  gridExtent := 8.0
  gridStep := 1.0
  gridMajorStep := 2.0
  axisLength := 3.5
  showAxes := false
  showAxisLabels := false
  gridLineWidth := 1.0 * screenScale
  axisLineWidth := 2.0 * screenScale
}

/-- Draw a 3D arrow (vector from origin to tip) -/
def draw3DArrow (view : MathView3D.View) (vec : Vec3)
    (config : ArrowConfig := {}) : CanvasM Unit := do
  match MathView3D.worldToScreen view Vec3.zero, MathView3D.worldToScreen view vec with
  | some start, some finish => drawArrow2D start finish config
  | _, _ => pure ()

/-- Draw XYZ coordinate axes -/
def draw3DAxes (view : MathView3D.View) (axisLength : Float) (fontSmall : Font) : CanvasM Unit := do
  -- X axis (red)
  let xEnd := Vec3.mk axisLength 0.0 0.0
  draw3DArrow view xEnd { color := VecColor.xAxis, lineWidth := 2.0 }
  match MathView3D.worldToScreen view xEnd with
  | some (xx, xy) =>
      setFillColor VecColor.xAxis
      fillTextXY "X" (xx + 8) (xy - 8) fontSmall
  | none => pure ()

  -- Y axis (green)
  let yEnd := Vec3.mk 0.0 axisLength 0.0
  draw3DArrow view yEnd { color := VecColor.yAxis, lineWidth := 2.0 }
  match MathView3D.worldToScreen view yEnd with
  | some (yx, yy) =>
      setFillColor VecColor.yAxis
      fillTextXY "Y" (yx + 8) (yy - 8) fontSmall
  | none => pure ()

  -- Z axis (blue)
  let zEnd := Vec3.mk 0.0 0.0 axisLength
  draw3DArrow view zEnd { color := VecColor.zAxis, lineWidth := 2.0 }
  match MathView3D.worldToScreen view zEnd with
  | some (zx, zy) =>
      setFillColor VecColor.zAxis
      fillTextXY "Z" (zx + 8) (zy - 8) fontSmall
  | none => pure ()

/-- Draw the parallelogram formed by two vectors (for visualizing cross product magnitude) -/
def drawParallelogram3D (view : MathView3D.View) (a b : Vec3) (color : Color) : CanvasM Unit := do
  match MathView3D.worldToScreen view Vec3.zero,
        MathView3D.worldToScreen view a,
        MathView3D.worldToScreen view b,
        MathView3D.worldToScreen view (a + b) with
  | some o, some pa, some pb, some pab =>
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
  | _, _, _, _ => pure ()

/-- Render the cross product 3D visualization -/
def renderCrossProduct3D (state : CrossProduct3DState)
    (view : MathView3D.View) (screenScale : Float) (fontMedium fontSmall : Font) : CanvasM Unit := do
  let h := view.height

  -- Draw coordinate axes
  draw3DAxes view 3.5 fontSmall

  -- Calculate cross product
  let cross := Vec3.cross state.vectorA state.vectorB
  let crossMag := cross.length

  -- Draw parallelogram if enabled
  if state.showParallelogram then
    drawParallelogram3D view state.vectorA state.vectorB (Color.rgba 0.8 0.8 0.0 0.2)

  -- Draw vector A (cyan)
  draw3DArrow view state.vectorA
    { color := VecColor.vectorA, lineWidth := 3.0 }
  match MathView3D.worldToScreen view state.vectorA with
  | some screenA =>
      setFillColor VecColor.vectorA
      fillTextXY "A" (screenA.1 + 10) (screenA.2 - 10) fontSmall
  | none => pure ()

  -- Draw vector B (magenta)
  draw3DArrow view state.vectorB
    { color := VecColor.vectorB, lineWidth := 3.0 }
  match MathView3D.worldToScreen view state.vectorB with
  | some screenB =>
      setFillColor VecColor.vectorB
      fillTextXY "B" (screenB.1 + 10) (screenB.2 - 10) fontSmall
  | none => pure ()

  -- Draw cross product vector (yellow)
  if crossMag > 0.01 then
    draw3DArrow view cross
      { color := VecColor.reflection, lineWidth := 3.5 }
    match MathView3D.worldToScreen view cross with
    | some screenC =>
        setFillColor VecColor.reflection
        fillTextXY "A×B" (screenC.1 + 10) (screenC.2 - 10) fontSmall
    | none => pure ()

  -- Draw origin marker
  match MathView3D.worldToScreen view Vec3.zero with
  | some (ox, oy) =>
      setFillColor Color.white
      fillPath (Afferent.Path.circle (Point.mk ox oy) (4.0 * screenScale))
  | none => pure ()

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
  let config := crossProduct3DMathViewConfig state env.screenScale
  MathView3D.mathView3D config env.fontSmall (fun view => do
    renderCrossProduct3D state view env.screenScale env.fontMedium env.fontSmall
  )

end Demos.Linalg
