/-
  Quaternion Visualizer - 3D object rotated by quaternion.
  Shows axis-angle representation, arcball rotation, and component sliders.
-/
import Afferent
import Afferent.Widget
import Afferent.Arbor
import Demos.Core.Demo
import Demos.Linalg.Shared
import Demos.Linalg.RotationShared
import Trellis
import Linalg.Core
import Linalg.Vec2
import Linalg.Vec3
import Linalg.Mat4
import Linalg.Quat

open Afferent CanvasM Linalg
open Afferent.Widget

namespace Demos.Linalg

/-- Quaternion component selector. -/
inductive QuatComponent where
  | x | y | z | w
  deriving BEq, Inhabited

/-- Drag mode for quaternion visualizer. -/
inductive QuatDragMode where
  | none
  | arcball
  | camera
  | slider (component : QuatComponent)
  deriving BEq, Inhabited

/-- State for quaternion visualization. -/
structure QuaternionVisualizerState where
  quat : Quat := Quat.identity
  cameraYaw : Float := 0.5
  cameraPitch : Float := 0.3
  dragging : QuatDragMode := .none
  lastMouseX : Float := 0.0
  lastMouseY : Float := 0.0
  eulerAngles : Vec3 := Vec3.zero
  selectedEuler : Nat := 0
  deriving Inhabited

def quaternionVisualizerInitialState : QuaternionVisualizerState := {}

def quaternionVisualizerMathViewConfig (state : QuaternionVisualizerState) (screenScale : Float)
    : MathView3D.Config := {
  style := { flexItem := some (Trellis.FlexItem.growing 1) }
  camera := { yaw := state.cameraYaw, pitch := state.cameraPitch, distance := 8.0 }
  gridExtent := 2.5
  gridStep := 0.5
  gridMajorStep := 1.0
  axisLength := 3.0
  showAxes := false
  showAxisLabels := false
  gridLineWidth := 1.0 * screenScale
  axisLineWidth := 2.0 * screenScale
}

/-- Get quaternion component value. -/
def getQuatComponent (q : Quat) : QuatComponent → Float
  | .x => q.x
  | .y => q.y
  | .z => q.z
  | .w => q.w

/-- Set quaternion component value. -/
def setQuatComponent (q : Quat) (comp : QuatComponent) (value : Float) : Quat :=
  match comp with
  | .x => { q with x := value }
  | .y => { q with y := value }
  | .z => { q with z := value }
  | .w => { q with w := value }

/-- Slider layout parameters. -/
structure SliderLayout where
  x : Float
  y : Float
  width : Float
  height : Float

/-- Compute slider geometry for a given index. -/
def sliderLayoutFor (w h screenScale : Float) (idx : Nat) : SliderLayout :=
  let startX := w - 250.0 * screenScale
  let startY := 120.0 * screenScale
  let width := 180.0 * screenScale
  let height := 8.0 * screenScale
  let spacing := 32.0 * screenScale
  { x := startX, y := startY + idx.toFloat * spacing, width := width, height := height }

/-- Clamp float to [-1, 1]. -/
def clampUnit (v : Float) : Float :=
  if v < -1.0 then -1.0 else if v > 1.0 then 1.0 else v

/-- Render a labeled slider. -/
def renderSlider (label : String) (value : Float) (layout : SliderLayout) (fontSmall : Font)
    (active : Bool := false) : CanvasM Unit := do
  let t := (value + 1.0) * 0.5
  let knobX := layout.x + t * layout.width
  let knobY := layout.y + layout.height / 2.0
  let knobRadius := layout.height * 0.75
  let trackHeight := layout.height * 0.5

  let trackColor := if active then Color.gray 0.7 else Color.gray 0.5
  setFillColor trackColor
  fillPath (Afferent.Path.rectangleXYWH layout.x (layout.y + layout.height / 2.0 - trackHeight / 2.0)
    layout.width trackHeight)

  setFillColor (if active then Color.yellow else Color.gray 0.8)
  fillPath (Afferent.Path.circle (Point.mk knobX knobY) knobRadius)

  setFillColor (Color.gray 0.8)
  fillTextXY label (layout.x - 26.0) (layout.y + 6.0) fontSmall

/-- Render the quaternion visualization. -/
def renderQuaternionVisualizer (state : QuaternionVisualizerState)
    (view : MathView3D.View) (screenScale : Float) (fontMedium fontSmall : Font) : CanvasM Unit := do
  let w := view.width
  let h := view.height

  rotDraw3DAxes view 3.0 fontSmall

  -- Draw cube rotated by quaternion
  let rotMat := state.quat.toMat4
  rotDrawWireframeCube rotMat view (Color.rgba 0.3 0.8 1.0 0.9) (2.5 * screenScale)

  -- Draw axis-angle representation
  let (axis, angle) := state.quat.toAxisAngle
  let axisLine := axis.scale 2.0
  rotDraw3DArrow view axisLine { color := Color.yellow, lineWidth := 2.5 * screenScale }

  -- Draw rotated forward vector to demonstrate rotateVec3
  let forward := state.quat.rotateVec3 Vec3.unitZ
  rotDraw3DArrow view (forward.scale 2.0)
    { color := VecColor.interpolated, lineWidth := 2.5 * screenScale }

  -- Info panel
  let infoY := h - 160 * screenScale
  let angleDeg := angle * 180.0 / Float.pi
  setFillColor VecColor.label
  fillTextXY s!"q = ({formatFloat state.quat.x}, {formatFloat state.quat.y}, {formatFloat state.quat.z}, {formatFloat state.quat.w})"
    (20 * screenScale) infoY fontSmall
  fillTextXY s!"axis = {formatVec3 axis}"
    (20 * screenScale) (infoY + 22 * screenScale) fontSmall
  fillTextXY s!"angle = {formatFloat angleDeg} deg"
    (20 * screenScale) (infoY + 44 * screenScale) fontSmall
  fillTextXY s!"euler edit (deg): {formatFloat (state.eulerAngles.x * 180.0 / Float.pi)}, "
    (20 * screenScale) (infoY + 66 * screenScale) fontSmall
  fillTextXY s!"  {formatFloat (state.eulerAngles.y * 180.0 / Float.pi)}, {formatFloat (state.eulerAngles.z * 180.0 / Float.pi)}"
    (20 * screenScale) (infoY + 86 * screenScale) fontSmall

  -- Title and instructions
  fillTextXY "QUATERNION VISUALIZER" (20 * screenScale) (30 * screenScale) fontMedium
  setFillColor (Color.gray 0.6)
  fillTextXY "Drag: arcball | Right-drag: camera | E: apply Euler | 1/2/3 select Euler axis | ←/→ adjust"
    (20 * screenScale) (55 * screenScale) fontSmall

  -- Sliders panel
  let labels : Array (QuatComponent × String) := #[
    (.x, "x"), (.y, "y"), (.z, "z"), (.w, "w")
  ]
  for i in [:labels.size] do
    let (comp, label) := labels.getD i (.x, "x")
    let layout := sliderLayoutFor w h screenScale i
    let value := getQuatComponent state.quat comp
    let active := match state.dragging with
      | .slider c => c == comp
      | _ => false
    renderSlider label value layout fontSmall active

/-- Create the quaternion visualizer widget. -/
def quaternionVisualizerWidget (env : DemoEnv) (state : QuaternionVisualizerState)
    : Afferent.Arbor.WidgetBuilder := do
  let config := quaternionVisualizerMathViewConfig state env.screenScale
  MathView3D.mathView3D config env.fontSmall (fun view => do
    renderQuaternionVisualizer state view env.screenScale env.fontMedium env.fontSmall
  )

end Demos.Linalg
