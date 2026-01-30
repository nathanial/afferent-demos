/-
  Frustum Culling Demo - visibility of spheres and AABBs against a camera frustum.
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
import Linalg.Vec4
import Linalg.Mat4
import Linalg.Geometry.Frustum
import Linalg.Geometry.Sphere
import Linalg.Geometry.AABB

open Afferent CanvasM Linalg

namespace Demos.Linalg

/-- State for frustum culling demo. -/
structure FrustumCullingDemoState where
  viewYaw : Float := 0.7
  viewPitch : Float := 0.35
  camYaw : Float := 0.9
  camPitch : Float := 0.2
  camDist : Float := 6.0
  dragging : Bool := false
  lastMouseX : Float := 0.0
  lastMouseY : Float := 0.0
  deriving Inhabited

/-- Initial state. -/
def frustumCullingDemoInitialState : FrustumCullingDemoState := {}

private def drawLine3D (a b : Vec3) (yaw pitch : Float) (origin : Float × Float)
    (scale : Float) (color : Color) (lineWidth : Float := 1.5) : CanvasM Unit := do
  let p1 := rotProject3Dto2D a yaw pitch origin scale
  let p2 := rotProject3Dto2D b yaw pitch origin scale
  setStrokeColor color
  setLineWidth lineWidth
  let path := Afferent.Path.empty
    |>.moveTo (Point.mk p1.1 p1.2)
    |>.lineTo (Point.mk p2.1 p2.2)
  strokePath path

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
      yaw pitch origin scale color 1.5

private def containmentColor : Frustum.Containment -> Color
  | .inside => Color.rgba 0.3 0.9 0.5 0.9
  | .intersects => Color.rgba 1.0 0.85 0.3 0.9
  | .outside => Color.rgba 1.0 0.4 0.4 0.9

private def drawSphereMarker (sphere : Sphere) (yaw pitch : Float) (origin : Float × Float)
    (scale : Float) (color : Color) : CanvasM Unit := do
  let (sx, sy) := rotProject3Dto2D sphere.center yaw pitch origin scale
  let r := sphere.radius * scale
  setStrokeColor color
  setLineWidth 2.0
  strokePath (Afferent.Path.circle (Point.mk sx sy) r)
  setFillColor color
  fillPath (Afferent.Path.circle (Point.mk sx sy) 4.0)

/-- Render the frustum culling demo. -/
def renderFrustumCullingDemo (state : FrustumCullingDemoState)
    (w h : Float) (screenScale : Float) (fontMedium fontSmall : Font) : CanvasM Unit := do
  let origin : Float × Float := (w / 2, h / 2)
  let scale : Float := 70.0 * screenScale

  -- Culling camera
  let cy := Float.cos state.camPitch
  let sy := Float.sin state.camPitch
  let eye := Vec3.mk (Float.cos state.camYaw * cy * state.camDist)
    (sy * state.camDist)
    (Float.sin state.camYaw * cy * state.camDist)
  let view := Mat4.lookAt eye Vec3.zero Vec3.unitY
  let aspect := if h > 0.0 then w / h else 1.0
  let proj := Mat4.perspective (60.0 * Linalg.Float.pi / 180.0) aspect 0.5 8.0
  let vp := proj * view
  let frustum := Frustum.fromViewProjection vp

  let corners := match vp.inverse with
    | some inv => Frustum.corners inv
    | none => #[]

  -- Draw frustum
  if corners.size == 8 then
    let edges : Array (Nat × Nat) := #[
      (0, 1), (1, 2), (2, 3), (3, 0),
      (4, 5), (5, 6), (6, 7), (7, 4),
      (0, 4), (1, 5), (2, 6), (3, 7)
    ]
    for (i, j) in edges do
      drawLine3D (corners.getD i Vec3.zero) (corners.getD j Vec3.zero)
        state.viewYaw state.viewPitch origin scale (Color.gray 0.6) 1.2

  -- Objects to cull
  let spheres : Array Sphere := #[
    Sphere.mk' (Vec3.mk (-2.0) 0.4 (-1.5)) 0.6,
    Sphere.mk' (Vec3.mk (-0.6) 0.3 (-2.5)) 0.5,
    Sphere.mk' (Vec3.mk 1.2 0.2 (-3.5)) 0.7,
    Sphere.mk' (Vec3.mk 2.5 0.2 0.2) 0.6,
    Sphere.mk' (Vec3.mk 0.4 1.2 (-1.0)) 0.5
  ]
  let aabbs : Array AABB := #[
    AABB.fromCenterExtents (Vec3.mk (-1.5) 0.2 1.5) (Vec3.mk 0.4 0.4 0.4),
    AABB.fromCenterExtents (Vec3.mk 0.8 0.2 1.2) (Vec3.mk 0.6 0.3 0.5),
    AABB.fromCenterExtents (Vec3.mk 2.2 0.4 (-2.0)) (Vec3.mk 0.5 0.5 0.5),
    AABB.fromCenterExtents (Vec3.mk (-2.6) 0.3 (-3.2)) (Vec3.mk 0.5 0.4 0.6)
  ]

  let mut visibleCount := 0
  let mut intersectCount := 0

  for s in spheres do
    let containment := frustum.testSphere s
    if containment == .inside then visibleCount := visibleCount + 1
    else if containment == .intersects then intersectCount := intersectCount + 1
    drawSphereMarker s state.viewYaw state.viewPitch origin scale (containmentColor containment)

  for b in aabbs do
    let containment := frustum.testAABB b
    if containment == .inside then visibleCount := visibleCount + 1
    else if containment == .intersects then intersectCount := intersectCount + 1
    drawAABBWireframe b state.viewYaw state.viewPitch origin scale (containmentColor containment)

  -- Draw camera position
  let (cx, cy2) := rotProject3Dto2D eye state.viewYaw state.viewPitch origin scale
  setFillColor Color.white
  fillPath (Afferent.Path.circle (Point.mk cx cy2) 5.0)

  -- Info panel
  let infoY := h - 140 * screenScale
  setFillColor VecColor.label
  fillTextXY s!"inside: {visibleCount}  partial: {intersectCount}"
    (20 * screenScale) infoY fontSmall
  fillTextXY s!"camera yaw={formatFloat state.camYaw}, pitch={formatFloat state.camPitch}"
    (20 * screenScale) (infoY + 20 * screenScale) fontSmall

  -- Title and instructions
  fillTextXY "FRUSTUM CULLING DEMO" (20 * screenScale) (30 * screenScale) fontMedium
  setFillColor (Color.gray 0.6)
  fillTextXY "Drag: rotate view | I/K/J/L: move camera | +/-: distance" (20 * screenScale) (55 * screenScale) fontSmall

/-- Create the frustum culling widget. -/
def frustumCullingDemoWidget (env : DemoEnv) (state : FrustumCullingDemoState)
    : Afferent.Arbor.WidgetBuilder := do
  Afferent.Arbor.custom (spec := {
    measure := fun _ _ => (0, 0)
    collect := fun _ => #[]
    draw := some (fun layout => do
      withContentRect layout fun w h => do
        resetTransform
        renderFrustumCullingDemo state w h env.screenScale env.fontMedium env.fontSmall
    )
  }) (style := { flexItem := some (Trellis.FlexItem.growing 1) })

end Demos.Linalg
