/-
  Ray Casting Playground - ray intersections with sphere, AABB, plane, triangle.
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
import Linalg.Geometry.Ray
import Linalg.Geometry.Sphere
import Linalg.Geometry.AABB
import Linalg.Geometry.Plane
import Linalg.Geometry.Triangle
import Linalg.Geometry.Intersection
import AfferentMath.Widget.MathView3D

open Afferent CanvasM Linalg
open Afferent.Widget
open AfferentMath.Widget

namespace Demos.Linalg

/-- Drag target for ray casting demo. -/
inductive RayDragTarget where
  | none
  | origin
  | direction
  | camera
  deriving BEq, Inhabited

/-- State for ray casting playground. -/
structure RayCastingPlaygroundState where
  rayOrigin : Vec3 := Vec3.mk (-2.0) 0.0 (-1.2)
  rayTarget : Vec3 := Vec3.mk 1.8 0.0 1.2
  cameraYaw : Float := 0.6
  cameraPitch : Float := 0.35
  dragging : RayDragTarget := .none
  lastMouseX : Float := 0.0
  lastMouseY : Float := 0.0
  deriving Inhabited

def rayCastingPlaygroundInitialState : RayCastingPlaygroundState := {}

def rayCastingPlaygroundMathViewConfig (state : RayCastingPlaygroundState) (screenScale : Float)
    : MathView3D.Config := {
  style := { flexItem := some (Trellis.FlexItem.growing 1) }
  camera := { yaw := state.cameraYaw, pitch := state.cameraPitch, distance := 8.0 }
  gridExtent := 2.5
  gridStep := 0.5
  gridMajorStep := 1.0
  axisLength := 2.8
  axisLineWidth := 2.0 * screenScale
  gridLineWidth := 1.0 * screenScale
}

private def drawSphereRingsAt (view : MathView3D.View) (center : Vec3) (radius : Float)
    (color : Color) : CanvasM Unit := do
  rotDrawCircle3D center Vec3.unitX radius view 48 color 1.5
  rotDrawCircle3D center Vec3.unitY radius view 48 color 1.5
  rotDrawCircle3D center Vec3.unitZ radius view 48 color 1.5

private def drawAABBWireframe (view : MathView3D.View) (aabb : AABB) (color : Color)
    (lineWidth : Float := 1.6) : CanvasM Unit := do
  let min := aabb.min
  let max := aabb.max
  let corners : Array Vec3 := #[
    Vec3.mk min.x min.y min.z,
    Vec3.mk max.x min.y min.z,
    Vec3.mk max.x max.y min.z,
    Vec3.mk min.x max.y min.z,
    Vec3.mk min.x min.y max.z,
    Vec3.mk max.x min.y max.z,
    Vec3.mk max.x max.y max.z,
    Vec3.mk min.x max.y max.z
  ]
  let edges : Array (Nat × Nat) := #[
    (0, 1), (1, 2), (2, 3), (3, 0),
    (4, 5), (5, 6), (6, 7), (7, 4),
    (0, 4), (1, 5), (2, 6), (3, 7)
  ]
  for (i, j) in edges do
    MathView3D.drawLine3D view (corners.getD i Vec3.zero) (corners.getD j Vec3.zero)
      color lineWidth

private def drawPlaneQuad (view : MathView3D.View) (plane : Plane) (size : Float) : CanvasM Unit := do
  let center := plane.origin
  let (u, v) := basisFromAxis plane.normal
  let p1 := (center.add (u.scale size)).add (v.scale size)
  let p2 := (center.add (u.scale (-size))).add (v.scale size)
  let p3 := (center.add (u.scale (-size))).add (v.scale (-size))
  let p4 := (center.add (u.scale size)).add (v.scale (-size))
  MathView3D.drawLine3D view p1 p2 (Color.gray 0.4) 1.0
  MathView3D.drawLine3D view p2 p3 (Color.gray 0.4) 1.0
  MathView3D.drawLine3D view p3 p4 (Color.gray 0.4) 1.0
  MathView3D.drawLine3D view p4 p1 (Color.gray 0.4) 1.0

/-- Render the ray casting playground. -/
def renderRayCastingPlayground (state : RayCastingPlaygroundState)
    (view : MathView3D.View) (screenScale : Float) (fontMedium fontSmall : Font) : CanvasM Unit := do
  let h := view.height

  -- Scene primitives
  let sphere := Sphere.mk' (Vec3.mk 1.2 0.6 0.2) 0.6
  let aabb := AABB.fromCenterExtents (Vec3.mk (-0.9) 0.4 1.1) (Vec3.mk 0.5 0.4 0.6)
  let plane := Plane.fromNormalPoint Vec3.unitY (Vec3.mk 0.0 (-0.6) 0.0)
  let tri := Triangle.mk' (Vec3.mk (-1.6) 0.2 (-1.0)) (Vec3.mk (-0.2) 0.7 (-0.6)) (Vec3.mk (-0.8) 0.2 0.6)

  drawPlaneQuad view plane 2.2
  drawSphereRingsAt view sphere.center sphere.radius (Color.rgba 0.3 0.7 1.0 0.7)
  drawAABBWireframe view aabb (Color.rgba 0.8 0.5 0.2 0.9)
  MathView3D.drawLine3D view tri.v0 tri.v1 (Color.rgba 0.8 0.7 0.2 0.9) (2.0 * screenScale)
  MathView3D.drawLine3D view tri.v1 tri.v2 (Color.rgba 0.8 0.7 0.2 0.9) (2.0 * screenScale)
  MathView3D.drawLine3D view tri.v2 tri.v0 (Color.rgba 0.8 0.7 0.2 0.9) (2.0 * screenScale)

  -- Ray
  let ray := Ray.mk' state.rayOrigin (state.rayTarget.sub state.rayOrigin)
  let rayEnd := ray.origin.add (ray.direction.scale 6.0)
  MathView3D.drawLine3D view ray.origin rayEnd (Color.rgba 1.0 0.6 0.3 0.9) (2.2 * screenScale)

  match rotProject3Dto2D view state.rayOrigin with
  | some (rx, ry) =>
      setFillColor Color.white
      fillPath (Afferent.Path.circle (Point.mk rx ry) (6.0 * screenScale))
  | none => pure ()
  match rotProject3Dto2D view state.rayTarget with
  | some (tx, ty) =>
      setFillColor (Color.rgba 1.0 0.6 0.3 1.0)
      fillPath (Afferent.Path.circle (Point.mk tx ty) (5.0 * screenScale))
  | none => pure ()

  -- Intersections
  let sphereHit := Intersection.raySphere ray sphere
  let aabbHit := Intersection.rayAABBHit ray aabb
  let planeHit := Intersection.rayPlane ray plane
  let triHit := Intersection.rayTriangle ray tri

  let drawHit (hit : RayHit) (color : Color) : CanvasM Unit := do
    match rotProject3Dto2D view hit.point with
    | some (hx, hy) =>
        setFillColor color
        fillPath (Afferent.Path.circle (Point.mk hx hy) (5.0 * screenScale))
    | none => pure ()
    let normalEnd := hit.point.add (hit.normal.scale 0.4)
    MathView3D.drawLine3D view hit.point normalEnd color (2.0 * screenScale)

  if let some hit := sphereHit then
    drawHit hit (Color.rgba 0.3 0.7 1.0 1.0)
  if let some hit := aabbHit then
    drawHit hit (Color.rgba 0.8 0.5 0.2 1.0)
  if let some hit := planeHit then
    drawHit hit (Color.rgba 0.6 0.9 0.6 1.0)
  if let some hit := triHit then
    drawHit hit (Color.rgba 0.9 0.8 0.2 1.0)

  -- Info panel
  let infoY := h - 160 * screenScale
  setFillColor VecColor.label
  let sphereText := match sphereHit with | some h => s!"sphere t={formatFloat h.t}" | none => "sphere: none"
  let aabbText := match aabbHit with | some h => s!"aabb t={formatFloat h.t}" | none => "aabb: none"
  let planeText := match planeHit with | some h => s!"plane t={formatFloat h.t}" | none => "plane: none"
  let triText := match triHit with | some h => s!"triangle t={formatFloat h.t}" | none => "triangle: none"
  fillTextXY sphereText (20 * screenScale) infoY fontSmall
  fillTextXY aabbText (20 * screenScale) (infoY + 20 * screenScale) fontSmall
  fillTextXY planeText (20 * screenScale) (infoY + 40 * screenScale) fontSmall
  fillTextXY triText (20 * screenScale) (infoY + 60 * screenScale) fontSmall

  -- Title and instructions
  fillTextXY "RAY CASTING PLAYGROUND" (20 * screenScale) (30 * screenScale) fontMedium
  setFillColor (Color.gray 0.6)
  fillTextXY "Drag origin/target | Right-drag: rotate view" (20 * screenScale) (55 * screenScale) fontSmall

/-- Create the ray casting playground widget. -/
def rayCastingPlaygroundWidget (env : DemoEnv) (state : RayCastingPlaygroundState)
    : Afferent.Arbor.WidgetBuilder := do
  let config := rayCastingPlaygroundMathViewConfig state env.screenScale
  MathView3D.mathView3D config env.fontSmall (fun view => do
    renderRayCastingPlayground state view env.screenScale env.fontMedium env.fontSmall
  )

end Demos.Linalg
