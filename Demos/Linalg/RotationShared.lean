/-
  Shared helpers for rotation system demos (3D projection, simple shapes).
-/
import Afferent
import Afferent.Widget
import Afferent.Arbor
import Demos.Core.Demo
import Demos.Linalg.Shared
import Linalg.Vec3
import Linalg.Mat4

open Afferent CanvasM Linalg

namespace Demos.Linalg

/-- Project a 3D point to 2D screen using orthographic projection with camera rotation. -/
def rotProject3Dto2D (p : Vec3) (yaw pitch : Float) (origin : Float × Float) (scale : Float) : Float × Float :=
  -- Rotate around Y axis (yaw)
  let cosY := Float.cos yaw
  let sinY := Float.sin yaw
  let x1 := p.x * cosY + p.z * sinY
  let z1 := -p.x * sinY + p.z * cosY

  -- Rotate around X axis (pitch)
  let cosP := Float.cos pitch
  let sinP := Float.sin pitch
  let y2 := p.y * cosP - z1 * sinP

  (origin.1 + x1 * scale, origin.2 - y2 * scale)

/-- Draw a 3D arrow (vector from origin to tip). -/
def rotDraw3DArrow (vec : Vec3) (yaw pitch : Float) (origin : Float × Float)
    (scale : Float) (config : ArrowConfig := {}) : CanvasM Unit := do
  let start := rotProject3Dto2D Vec3.zero yaw pitch origin scale
  let finish := rotProject3Dto2D vec yaw pitch origin scale
  drawArrow2D start finish config

/-- Draw XYZ coordinate axes with labels. -/
def rotDraw3DAxes (yaw pitch : Float) (origin : Float × Float) (scale : Float)
    (axisLength : Float) (fontSmall : Font) : CanvasM Unit := do
  let xEnd := Vec3.mk axisLength 0.0 0.0
  rotDraw3DArrow xEnd yaw pitch origin scale { color := VecColor.xAxis, lineWidth := 2.0 }
  let (xx, xy) := rotProject3Dto2D xEnd yaw pitch origin scale
  setFillColor VecColor.xAxis
  fillTextXY "X" (xx + 8) (xy - 8) fontSmall

  let yEnd := Vec3.mk 0.0 axisLength 0.0
  rotDraw3DArrow yEnd yaw pitch origin scale { color := VecColor.yAxis, lineWidth := 2.0 }
  let (yx, yy) := rotProject3Dto2D yEnd yaw pitch origin scale
  setFillColor VecColor.yAxis
  fillTextXY "Y" (yx + 8) (yy - 8) fontSmall

  let zEnd := Vec3.mk 0.0 0.0 axisLength
  rotDraw3DArrow zEnd yaw pitch origin scale { color := VecColor.zAxis, lineWidth := 2.0 }
  let (zx, zy) := rotProject3Dto2D zEnd yaw pitch origin scale
  setFillColor VecColor.zAxis
  fillTextXY "Z" (zx + 8) (zy - 8) fontSmall

/-- Cube vertices (centered at origin, size 1). -/
def cubeVertices : Array Vec3 := #[
  Vec3.mk (-0.5) (-0.5) (-0.5), Vec3.mk 0.5 (-0.5) (-0.5),
  Vec3.mk 0.5 0.5 (-0.5), Vec3.mk (-0.5) 0.5 (-0.5),
  Vec3.mk (-0.5) (-0.5) 0.5, Vec3.mk 0.5 (-0.5) 0.5,
  Vec3.mk 0.5 0.5 0.5, Vec3.mk (-0.5) 0.5 0.5
]

/-- Cube edge indices (pairs of vertex indices). -/
def cubeEdges : Array (Nat × Nat) := #[
  (0, 1), (1, 2), (2, 3), (3, 0),
  (4, 5), (5, 6), (6, 7), (7, 4),
  (0, 4), (1, 5), (2, 6), (3, 7)
]

/-- Draw a wireframe cube with a given transform. -/
def rotDrawWireframeCube (matrix : Mat4) (yaw pitch : Float) (origin : Float × Float)
    (scale : Float) (color : Color) (lineWidth : Float := 2.0) : CanvasM Unit := do
  let transformed := cubeVertices.map (matrix.transformPoint ·)
  let projected := transformed.map (rotProject3Dto2D · yaw pitch origin scale)

  setStrokeColor color
  setLineWidth lineWidth
  for (i, j) in cubeEdges do
    let p1 := projected.getD i (0.0, 0.0)
    let p2 := projected.getD j (0.0, 0.0)
    let path := Afferent.Path.empty
      |>.moveTo (Point.mk p1.1 p1.2)
      |>.lineTo (Point.mk p2.1 p2.2)
    strokePath path

/-- Build an orthonormal basis for a given axis. -/
def basisFromAxis (axis : Vec3) : Vec3 × Vec3 :=
  let n := axis.normalize
  let helper := if Float.abs n.x < 0.8 then Vec3.unitX else Vec3.unitY
  let u := n.cross helper |>.normalize
  let v := n.cross u |>.normalize
  (u, v)

/-- Draw a circle in 3D around a given axis. -/
def rotDrawCircle3D (center axis : Vec3) (radius : Float) (yaw pitch : Float)
    (origin : Float × Float) (scale : Float) (segments : Nat := 48)
    (color : Color := Color.gray 0.6) (lineWidth : Float := 1.5) : CanvasM Unit := do
  if segments < 3 then return
  let (u, v) := basisFromAxis axis
  let mut points : Array (Float × Float) := #[]
  for i in [:segments + 1] do
    let t := (i.toFloat / segments.toFloat) * (2.0 * Float.pi)
    let pt := center + u.scale (Float.cos t * radius) + v.scale (Float.sin t * radius)
    points := points.push (rotProject3Dto2D pt yaw pitch origin scale)
  if points.size < 2 then return
  setStrokeColor color
  setLineWidth lineWidth
  let mut path := Afferent.Path.empty
  let p0 := points.getD 0 (0.0, 0.0)
  path := path.moveTo (Point.mk p0.1 p0.2)
  for i in [:points.size] do
    let p := points.getD i (0.0, 0.0)
    path := path.lineTo (Point.mk p.1 p.2)
  strokePath path

/-- Draw three great-circle rings for a unit sphere. -/
def rotDrawSphereRings (yaw pitch : Float) (origin : Float × Float) (scale : Float)
    (radius : Float := 1.0) : CanvasM Unit := do
  rotDrawCircle3D Vec3.zero Vec3.unitX radius yaw pitch origin scale 64 (Color.gray 0.25) 1.0
  rotDrawCircle3D Vec3.zero Vec3.unitY radius yaw pitch origin scale 64 (Color.gray 0.25) 1.0
  rotDrawCircle3D Vec3.zero Vec3.unitZ radius yaw pitch origin scale 64 (Color.gray 0.25) 1.0

end Demos.Linalg
