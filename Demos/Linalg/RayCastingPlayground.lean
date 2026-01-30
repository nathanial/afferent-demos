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

open Afferent CanvasM Linalg

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

private def drawLine3D (a b : Vec3) (yaw pitch : Float) (origin : Float × Float)
    (scale : Float) (color : Color) (lineWidth : Float := 2.0) : CanvasM Unit := do
  let p1 := rotProject3Dto2D a yaw pitch origin scale
  let p2 := rotProject3Dto2D b yaw pitch origin scale
  setStrokeColor color
  setLineWidth lineWidth
  let path := Afferent.Path.empty
    |>.moveTo (Point.mk p1.1 p1.2)
    |>.lineTo (Point.mk p2.1 p2.2)
  strokePath path

private def drawSphereRingsAt (center : Vec3) (radius : Float) (yaw pitch : Float)
    (origin : Float × Float) (scale : Float) (color : Color) : CanvasM Unit := do
  rotDrawCircle3D center Vec3.unitX radius yaw pitch origin scale 48 color 1.5
  rotDrawCircle3D center Vec3.unitY radius yaw pitch origin scale 48 color 1.5
  rotDrawCircle3D center Vec3.unitZ radius yaw pitch origin scale 48 color 1.5

private def drawAABBWireframe (aabb : AABB) (yaw pitch : Float) (origin : Float × Float)
    (scale : Float) (color : Color) : CanvasM Unit := do
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
    drawLine3D (corners.getD i Vec3.zero) (corners.getD j Vec3.zero)
      yaw pitch origin scale color 1.6

private def drawPlaneQuad (plane : Plane) (size : Float) (yaw pitch : Float)
    (origin : Float × Float) (scale : Float) : CanvasM Unit := do
  let center := plane.origin
  let (u, v) := basisFromAxis plane.normal
  let p1 := (center.add (u.scale size)).add (v.scale size)
  let p2 := (center.add (u.scale (-size))).add (v.scale size)
  let p3 := (center.add (u.scale (-size))).add (v.scale (-size))
  let p4 := (center.add (u.scale size)).add (v.scale (-size))
  drawLine3D p1 p2 yaw pitch origin scale (Color.gray 0.4) 1.0
  drawLine3D p2 p3 yaw pitch origin scale (Color.gray 0.4) 1.0
  drawLine3D p3 p4 yaw pitch origin scale (Color.gray 0.4) 1.0
  drawLine3D p4 p1 yaw pitch origin scale (Color.gray 0.4) 1.0

/-- Render the ray casting playground. -/
def renderRayCastingPlayground (state : RayCastingPlaygroundState)
    (w h : Float) (screenScale : Float) (fontMedium fontSmall : Font) : CanvasM Unit := do
  let origin : Float × Float := (w / 2, h / 2)
  let scale : Float := 70.0 * screenScale

  -- Ground grid (XZ plane)
  setStrokeColor (Color.gray 0.15)
  setLineWidth 1.0
  for i in [:11] do
    let offset := (i.toFloat - 5.0) * 0.5
    let p1 := rotProject3Dto2D (Vec3.mk (-2.5) 0 offset) state.cameraYaw state.cameraPitch origin scale
    let p2 := rotProject3Dto2D (Vec3.mk 2.5 0 offset) state.cameraYaw state.cameraPitch origin scale
    let path := Afferent.Path.empty
      |>.moveTo (Point.mk p1.1 p1.2)
      |>.lineTo (Point.mk p2.1 p2.2)
    strokePath path
    let p3 := rotProject3Dto2D (Vec3.mk offset 0 (-2.5)) state.cameraYaw state.cameraPitch origin scale
    let p4 := rotProject3Dto2D (Vec3.mk offset 0 2.5) state.cameraYaw state.cameraPitch origin scale
    let path2 := Afferent.Path.empty
      |>.moveTo (Point.mk p3.1 p3.2)
      |>.lineTo (Point.mk p4.1 p4.2)
    strokePath path2

  rotDraw3DAxes state.cameraYaw state.cameraPitch origin scale 2.8 fontSmall

  -- Scene primitives
  let sphere := Sphere.mk' (Vec3.mk 1.2 0.6 0.2) 0.6
  let aabb := AABB.fromCenterExtents (Vec3.mk (-0.9) 0.4 1.1) (Vec3.mk 0.5 0.4 0.6)
  let plane := Plane.fromNormalPoint Vec3.unitY (Vec3.mk 0.0 (-0.6) 0.0)
  let tri := Triangle.mk' (Vec3.mk (-1.6) 0.2 (-1.0)) (Vec3.mk (-0.2) 0.7 (-0.6)) (Vec3.mk (-0.8) 0.2 0.6)

  drawPlaneQuad plane 2.2 state.cameraYaw state.cameraPitch origin scale
  drawSphereRingsAt sphere.center sphere.radius state.cameraYaw state.cameraPitch origin scale (Color.rgba 0.3 0.7 1.0 0.7)
  drawAABBWireframe aabb state.cameraYaw state.cameraPitch origin scale (Color.rgba 0.8 0.5 0.2 0.9)
  drawLine3D tri.v0 tri.v1 state.cameraYaw state.cameraPitch origin scale (Color.rgba 0.8 0.7 0.2 0.9) 2.0
  drawLine3D tri.v1 tri.v2 state.cameraYaw state.cameraPitch origin scale (Color.rgba 0.8 0.7 0.2 0.9) 2.0
  drawLine3D tri.v2 tri.v0 state.cameraYaw state.cameraPitch origin scale (Color.rgba 0.8 0.7 0.2 0.9) 2.0

  -- Ray
  let ray := Ray.mk' state.rayOrigin (state.rayTarget.sub state.rayOrigin)
  let rayEnd := ray.origin.add (ray.direction.scale 6.0)
  drawLine3D ray.origin rayEnd state.cameraYaw state.cameraPitch origin scale (Color.rgba 1.0 0.6 0.3 0.9) 2.2

  let (rx, ry) := rotProject3Dto2D state.rayOrigin state.cameraYaw state.cameraPitch origin scale
  let (tx, ty) := rotProject3Dto2D state.rayTarget state.cameraYaw state.cameraPitch origin scale
  setFillColor Color.white
  fillPath (Afferent.Path.circle (Point.mk rx ry) 6.0)
  setFillColor (Color.rgba 1.0 0.6 0.3 1.0)
  fillPath (Afferent.Path.circle (Point.mk tx ty) 5.0)

  -- Intersections
  let sphereHit := Intersection.raySphere ray sphere
  let aabbHit := Intersection.rayAABBHit ray aabb
  let planeHit := Intersection.rayPlane ray plane
  let triHit := Intersection.rayTriangle ray tri

  let drawHit (hit : RayHit) (color : Color) : CanvasM Unit := do
    let (hx, hy) := rotProject3Dto2D hit.point state.cameraYaw state.cameraPitch origin scale
    setFillColor color
    fillPath (Afferent.Path.circle (Point.mk hx hy) 5.0)
    let normalEnd := hit.point.add (hit.normal.scale 0.4)
    drawLine3D hit.point normalEnd state.cameraYaw state.cameraPitch origin scale color 2.0

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
  Afferent.Arbor.custom (spec := {
    measure := fun _ _ => (0, 0)
    collect := fun _ => #[]
    draw := some (fun layout => do
      withContentRect layout fun w h => do
        resetTransform
        renderRayCastingPlayground state w h env.screenScale env.fontMedium env.fontSmall
    )
  }) (style := { flexItem := some (Trellis.FlexItem.growing 1) })

end Demos.Linalg
