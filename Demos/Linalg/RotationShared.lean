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
import AfferentMath.Widget.MathView3D

open Afferent CanvasM Linalg
open Afferent.Widget
open AfferentMath.Widget

namespace Demos.Linalg

/-- Project a 3D point to 2D screen using MathView3D. -/
def rotProject3Dto2D (view : MathView3D.View) (p : Vec3) : Option (Float × Float) :=
  MathView3D.worldToScreen view p

/-- Draw a 3D arrow (vector from origin to tip). -/
def rotDraw3DArrow (view : MathView3D.View) (vec : Vec3) (config : ArrowConfig := {}) : CanvasM Unit := do
  match rotProject3Dto2D view Vec3.zero, rotProject3Dto2D view vec with
  | some start, some finish => drawArrow2D start finish config
  | _, _ => pure ()

/-- Draw XYZ coordinate axes with labels. -/
def rotDraw3DAxes (view : MathView3D.View) (axisLength : Float) (fontSmall : Font) : CanvasM Unit := do
  let xEnd := Vec3.mk axisLength 0.0 0.0
  rotDraw3DArrow view xEnd { color := VecColor.xAxis, lineWidth := 2.0 }
  match rotProject3Dto2D view xEnd with
  | some (xx, xy) =>
      setFillColor VecColor.xAxis
      fillTextXY "X" (xx + 8) (xy - 8) fontSmall
  | none => pure ()

  let yEnd := Vec3.mk 0.0 axisLength 0.0
  rotDraw3DArrow view yEnd { color := VecColor.yAxis, lineWidth := 2.0 }
  match rotProject3Dto2D view yEnd with
  | some (yx, yy) =>
      setFillColor VecColor.yAxis
      fillTextXY "Y" (yx + 8) (yy - 8) fontSmall
  | none => pure ()

  let zEnd := Vec3.mk 0.0 0.0 axisLength
  rotDraw3DArrow view zEnd { color := VecColor.zAxis, lineWidth := 2.0 }
  match rotProject3Dto2D view zEnd with
  | some (zx, zy) =>
      setFillColor VecColor.zAxis
      fillTextXY "Z" (zx + 8) (zy - 8) fontSmall
  | none => pure ()

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
def rotDrawWireframeCube (matrix : Mat4) (view : MathView3D.View)
    (color : Color) (lineWidth : Float := 2.0) : CanvasM Unit := do
  let transformed := cubeVertices.map (matrix.transformPoint ·)
  for (i, j) in cubeEdges do
    MathView3D.drawLine3D view (transformed.getD i Vec3.zero) (transformed.getD j Vec3.zero)
      color lineWidth

/-- Build an orthonormal basis for a given axis. -/
def basisFromAxis (axis : Vec3) : Vec3 × Vec3 :=
  let n := axis.normalize
  let helper := if Float.abs n.x < 0.8 then Vec3.unitX else Vec3.unitY
  let u := n.cross helper |>.normalize
  let v := n.cross u |>.normalize
  (u, v)

/-- Draw a circle in 3D around a given axis. -/
def rotDrawCircle3D (center axis : Vec3) (radius : Float) (view : MathView3D.View)
    (segments : Nat := 48) (color : Color := Color.gray 0.6) (lineWidth : Float := 1.5)
    : CanvasM Unit := do
  if segments < 3 then return
  let (u, v) := basisFromAxis axis
  let mut points : Array Vec3 := #[]
  for i in [:segments + 1] do
    let t := (i.toFloat / segments.toFloat) * (2.0 * Float.pi)
    let pt := center + u.scale (Float.cos t * radius) + v.scale (Float.sin t * radius)
    points := points.push pt
  MathView3D.drawPolyline3D view points color lineWidth

/-- Draw three great-circle rings for a unit sphere. -/
def rotDrawSphereRings (view : MathView3D.View) (radius : Float := 1.0) : CanvasM Unit := do
  rotDrawCircle3D Vec3.zero Vec3.unitX radius view 64 (Color.gray 0.25) 1.0
  rotDrawCircle3D Vec3.zero Vec3.unitY radius view 64 (Color.gray 0.25) 1.0
  rotDrawCircle3D Vec3.zero Vec3.unitZ radius view 64 (Color.gray 0.25) 1.0

end Demos.Linalg
