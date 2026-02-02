/-
  Inertia Tensor Visualizer - show inertia ellipsoid for basic shapes.
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
import Linalg.Mat3
import Linalg.Physics

open Afferent CanvasM Linalg
open Afferent.Widget

namespace Demos.Linalg

/-- Shape options for inertia tensor visualization. -/
inductive TensorShape where
  | sphere
  | box
  | cylinder
  deriving BEq, Inhabited

/-- Slider targets. -/
inductive TensorSlider where
  | sizeA
  | sizeB
  | sizeC
  | mass
  | offsetX
  | offsetY
  deriving BEq, Inhabited

/-- State for inertia tensor visualizer. -/
structure InertiaTensorVisualizerState where
  shape : TensorShape := .sphere
  mass : Float := 1.5
  sizeA : Float := 1.0
  sizeB : Float := 1.2
  sizeC : Float := 0.8
  offsetX : Float := 0.0
  offsetY : Float := 0.0
  dropdownOpen : Bool := false
  dragging : Option TensorSlider := none
  deriving Inhabited

/-- Initial state. -/
def inertiaTensorVisualizerInitialState : InertiaTensorVisualizerState := {}

def inertiaTensorMathViewConfig (screenScale : Float) : MathView2D.Config := {
  style := { flexItem := some (Trellis.FlexItem.growing 1) }
  scale := 70.0 * screenScale
  minorStep := 1.0
  majorStep := 2.0
  gridMinorColor := Color.gray 0.2
  gridMajorColor := Color.gray 0.4
  axisColor := Color.gray 0.6
  labelColor := VecColor.label
  labelPrecision := 0
}

private def shapeName : TensorShape → String
  | .sphere => "Sphere"
  | .box => "Box"
  | .cylinder => "Cylinder"

private structure SliderLayout where
  x : Float
  y : Float
  width : Float
  height : Float

private def sliderLayout (w _h screenScale : Float) (idx : Nat) : SliderLayout :=
  let width := 220 * screenScale
  let height := 16 * screenScale
  let x := w - width - 30 * screenScale
  let y := 110 * screenScale + idx.toFloat * 34 * screenScale
  { x := x, y := y, width := width, height := height }

private def drawSlider (layout : SliderLayout) (value minV maxV : Float) (label : String)
    (font : Font) : CanvasM Unit := do
  setStrokeColor (Color.gray 0.5)
  setLineWidth 1.5
  let path := Afferent.Path.empty
    |>.moveTo (Point.mk layout.x (layout.y + layout.height / 2))
    |>.lineTo (Point.mk (layout.x + layout.width) (layout.y + layout.height / 2))
  strokePath path

  let t := Float.clamp ((value - minV) / (maxV - minV)) 0.0 1.0
  let knobX := layout.x + t * layout.width
  setFillColor (Color.gray 0.9)
  fillPath (Afferent.Path.circle (Point.mk knobX (layout.y + layout.height / 2)) 6.0)

  setFillColor (Color.gray 0.7)
  fillTextXY s!"{label}: {formatFloat value}" layout.x (layout.y - 8.0) font

private structure DropdownLayout where
  x : Float
  y : Float
  width : Float
  height : Float

private def dropdownLayout (w _h screenScale : Float) : DropdownLayout :=
  let width := 160 * screenScale
  let height := 28 * screenScale
  let x := w - width - 30 * screenScale
  let y := 30 * screenScale
  { x := x, y := y, width := width, height := height }

private def dropdownOptionLayout (drop : DropdownLayout) (idx : Nat) : DropdownLayout :=
  { x := drop.x, y := drop.y + drop.height + idx.toFloat * drop.height,
    width := drop.width, height := drop.height }

private def drawDropdown (drop : DropdownLayout) (label : String) (font : Font) : CanvasM Unit := do
  setFillColor (Color.gray 0.15)
  fillPath (Afferent.Path.rectangleXYWH drop.x drop.y drop.width drop.height)
  setStrokeColor (Color.gray 0.5)
  setLineWidth 1.0
  strokePath (Afferent.Path.rectangleXYWH drop.x drop.y drop.width drop.height)
  setFillColor (Color.gray 0.8)
  fillTextXY label (drop.x + 8) (drop.y + drop.height - 8) font

private def inertiaFor (shape : TensorShape) (mass : Float) (a b c : Float) : Mat3 :=
  match shape with
  | .sphere => InertiaTensor.solidSphere mass a
  | .box => InertiaTensor.solidBox mass (Vec3.mk a b c)
  | .cylinder => InertiaTensor.solidCylinder mass a b

/-- Render inertia tensor visualizer. -/
def renderInertiaTensorVisualizer (state : InertiaTensorVisualizerState)
    (view : MathView2D.View) (screenScale : Float) (fontMedium fontSmall : Font) : CanvasM Unit := do
  let w := view.width
  let h := view.height
  let origin : Float × Float := (view.origin.x, view.origin.y)
  let scale := view.scale

  let baseTensor := inertiaFor state.shape state.mass state.sizeA state.sizeB state.sizeC
  let offset := Vec3.mk state.offsetX state.offsetY 0.0
  let tensor := InertiaTensor.parallelAxis baseTensor state.mass offset

  let ix := tensor.get 0 0
  let iy := tensor.get 1 1
  let iz := tensor.get 2 2

  let rx := Float.sqrt (Float.max ix 0.0) * 0.35
  let ry := Float.sqrt (Float.max iy 0.0) * 0.35
  let rz := Float.sqrt (Float.max iz 0.0) * 0.25

  let (cx, cy) := worldToScreen Vec2.zero origin scale
  setStrokeColor (Color.rgba 0.3 0.9 0.7 0.9)
  setLineWidth 2.2
  strokeEllipse (Point.mk cx cy) (rx * scale) (ry * scale)
  setStrokeColor (Color.rgba 0.9 0.6 0.3 0.8)
  setLineWidth 1.6
  strokePath (Afferent.Path.circle (Point.mk cx cy) (rz * scale))

  let drop := dropdownLayout w h screenScale
  drawDropdown drop s!"{shapeName state.shape}" fontSmall
  if state.dropdownOpen then
    let options : Array TensorShape := #[.sphere, .box, .cylinder]
    for i in [:options.size] do
      let opt := options[i]!
      let optLayout := dropdownOptionLayout drop i
      let label := shapeName opt
      setFillColor (if opt == state.shape then Color.rgba 0.2 0.6 0.9 0.3 else Color.gray 0.12)
      fillPath (Afferent.Path.rectangleXYWH optLayout.x optLayout.y optLayout.width optLayout.height)
      setStrokeColor (Color.gray 0.5)
      setLineWidth 1.0
      strokePath (Afferent.Path.rectangleXYWH optLayout.x optLayout.y optLayout.width optLayout.height)
      setFillColor (Color.gray 0.8)
      fillTextXY label (optLayout.x + 8) (optLayout.y + optLayout.height - 8) fontSmall

  let sliders : Array (TensorSlider × String × Float × Float × Float) := #[
    (.sizeA, "Size A", state.sizeA, 0.3, 2.5),
    (.sizeB, "Size B", state.sizeB, 0.3, 2.5),
    (.sizeC, "Size C", state.sizeC, 0.3, 2.5),
    (.mass, "Mass", state.mass, 0.5, 5.0),
    (.offsetX, "Offset X", state.offsetX, -1.5, 1.5),
    (.offsetY, "Offset Y", state.offsetY, -1.5, 1.5)
  ]

  for i in [:sliders.size] do
    let (_, label, value, minV, maxV) := sliders[i]!
    let layout := sliderLayout w h screenScale i
    drawSlider layout value minV maxV label fontSmall

  let infoY := h - 140 * screenScale
  setFillColor VecColor.label
  fillTextXY s!"Ixx: {formatFloat ix}  Iyy: {formatFloat iy}  Izz: {formatFloat iz}" (20 * screenScale) infoY fontSmall
  fillTextXY s!"offset: ({formatFloat state.offsetX}, {formatFloat state.offsetY})" (20 * screenScale)
    (infoY + 20 * screenScale) fontSmall

  fillTextXY "INERTIA TENSOR VISUALIZER" (20 * screenScale) (30 * screenScale) fontMedium
  setFillColor (Color.gray 0.6)
  fillTextXY "Click dropdown to change shape. Drag sliders to adjust." (20 * screenScale) (55 * screenScale) fontSmall

/-- Create inertia tensor visualizer widget. -/
def inertiaTensorVisualizerWidget (env : DemoEnv) (state : InertiaTensorVisualizerState)
    : Afferent.Arbor.WidgetBuilder := do
  let config := inertiaTensorMathViewConfig env.screenScale
  MathView2D.mathView2D config env.fontSmall (fun view => do
    renderInertiaTensorVisualizer state view env.screenScale env.fontMedium env.fontSmall
  )

end Demos.Linalg
