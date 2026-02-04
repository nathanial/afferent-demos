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
import AfferentMath.Widget.MathView3D

open Afferent CanvasM Linalg
open Afferent.Widget
open AfferentMath.Widget

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

def frustumCullingMathViewConfig (state : FrustumCullingDemoState) (screenScale : Float)
    : MathView3D.Config := {
  style := { flexItem := some (Trellis.FlexItem.growing 1) }
  camera := { yaw := state.viewYaw, pitch := state.viewPitch, distance := 9.0 }
  showGrid := false
  showAxes := false
  axisLineWidth := 2.0 * screenScale
}

private def drawAABBWireframe (view : MathView3D.View) (aabb : AABB) (color : Color)
    (lineWidth : Float := 1.5) : CanvasM Unit := do
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

private def containmentColor : Frustum.Containment -> Color
  | .inside => Color.rgba 0.3 0.9 0.5 0.9
  | .intersects => Color.rgba 1.0 0.85 0.3 0.9
  | .outside => Color.rgba 1.0 0.4 0.4 0.9

private def drawSphereMarker (view : MathView3D.View) (sphere : Sphere)
    (color : Color) (screenScale : Float) : CanvasM Unit := do
  match rotProject3Dto2D view sphere.center with
  | some (sx, sy) =>
      let r := sphere.radius * 70.0 * screenScale
      setStrokeColor color
      setLineWidth 2.0
      strokePath (Afferent.Path.circle (Point.mk sx sy) r)
      setFillColor color
      fillPath (Afferent.Path.circle (Point.mk sx sy) (4.0 * screenScale))
  | none => pure ()

/-- Render the frustum culling demo. -/
def renderFrustumCullingDemo (state : FrustumCullingDemoState)
    (view : MathView3D.View) (screenScale : Float) (fontMedium fontSmall : Font) : CanvasM Unit := do
  let w := view.width
  let h := view.height

  -- Culling camera
  let cy := Float.cos state.camPitch
  let sy := Float.sin state.camPitch
  let eye := Vec3.mk (Float.cos state.camYaw * cy * state.camDist)
    (sy * state.camDist)
    (Float.sin state.camYaw * cy * state.camDist)
  let viewMatrix := Mat4.lookAt eye Vec3.zero Vec3.unitY
  let aspect := if h > 0.0 then w / h else 1.0
  let proj := Mat4.perspective (60.0 * Linalg.Float.pi / 180.0) aspect 0.5 8.0
  let vp := proj * viewMatrix
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
      MathView3D.drawLine3D view (corners.getD i Vec3.zero) (corners.getD j Vec3.zero)
        (Color.gray 0.6) (1.2 * screenScale)

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
    drawSphereMarker view s (containmentColor containment) screenScale

  for b in aabbs do
    let containment := frustum.testAABB b
    if containment == .inside then visibleCount := visibleCount + 1
    else if containment == .intersects then intersectCount := intersectCount + 1
    drawAABBWireframe view b (containmentColor containment)

  -- Draw camera position
  match rotProject3Dto2D view eye with
  | some (cx, cy2) =>
      setFillColor Color.white
      fillPath (Afferent.Path.circle (Point.mk cx cy2) (5.0 * screenScale))
  | none => pure ()

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
  let config := frustumCullingMathViewConfig state env.screenScale
  MathView3D.mathView3D config env.fontSmall (fun view => do
    renderFrustumCullingDemo state view env.screenScale env.fontMedium env.fontSmall
  )

end Demos.Linalg
