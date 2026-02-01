/-
  Transform Hierarchy Demo - hierarchical transforms with local/world gizmos.
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
import Linalg.Quat
import Linalg.Mat4
import Linalg.Transform

open Afferent CanvasM Linalg

namespace Demos.Linalg

/-- State for transform hierarchy demo. -/
structure TransformHierarchyState where
  blend : Float := 0.0
  animating : Bool := true
  time : Float := 0.0
  showLocalGizmos : Bool := true
  showWorldGizmos : Bool := true
  deriving Inhabited

/-- Initial state. -/
def transformHierarchyInitialState : TransformHierarchyState := {}

private def upperLen : Float := 2.2
private def lowerLen : Float := 1.6
private def handLen : Float := 1.0

private def vec3ToVec2 (v : Vec3) : Vec2 := Vec2.mk v.x v.y

private def mkArmPose (rootAngle elbowAngle wristAngle : Float) (rootOffset : Vec3)
    (wristScale : Vec3) : Array Linalg.Transform :=
  let root := Linalg.Transform.mk' rootOffset (Quat.fromAxisAngle Vec3.unitZ rootAngle) Vec3.one
  let elbow := Linalg.Transform.mk' (Vec3.mk upperLen 0.0 0.0)
    (Quat.fromAxisAngle Vec3.unitZ elbowAngle) Vec3.one
  let wrist := Linalg.Transform.mk' (Vec3.mk lowerLen 0.0 0.0)
    (Quat.fromAxisAngle Vec3.unitZ wristAngle) wristScale
  #[root, elbow, wrist]

private def poseA : Array Linalg.Transform :=
  mkArmPose (-0.5) 0.9 (-0.6) (Vec3.mk (-1.6) (-0.4) 0.0) (Vec3.mk 1.0 1.0 1.0)

private def poseB : Array Linalg.Transform :=
  mkArmPose (0.4) (-0.8) (0.8) (Vec3.mk (-1.6) (-0.4) 0.0) (Vec3.mk 0.8 1.1 1.0)

private def lerpPose (a b : Array Linalg.Transform) (t : Float) : Array Linalg.Transform := Id.run do
  let mut result : Array Linalg.Transform := #[]
  for i in [:a.size] do
    let ta := a.getD i Linalg.Transform.identity
    let tb := b.getD i Linalg.Transform.identity
    result := result.push (Linalg.Transform.lerp ta tb t)
  return result

private def drawLineWorld (a b : Vec2) (origin : Float × Float) (scale : Float)
    (color : Color) (lineWidth : Float := 2.0) : CanvasM Unit := do
  let (sx, sy) := worldToScreen a origin scale
  let (ex, ey) := worldToScreen b origin scale
  setStrokeColor color
  setLineWidth lineWidth
  let path := Afferent.Path.empty
    |>.moveTo (Point.mk sx sy)
    |>.lineTo (Point.mk ex ey)
  strokePath path

private def drawAxes (t : Linalg.Transform) (origin : Float × Float) (scale : Float)
    (axisLength : Float := 0.7) : CanvasM Unit := do
  let mat := t.toMat4
  let o := mat.transformPoint Vec3.zero
  let xEnd := mat.transformPoint (Vec3.mk axisLength 0.0 0.0)
  let yEnd := mat.transformPoint (Vec3.mk 0.0 axisLength 0.0)
  drawLineWorld (vec3ToVec2 o) (vec3ToVec2 xEnd) origin scale VecColor.xAxis 2.0
  drawLineWorld (vec3ToVec2 o) (vec3ToVec2 yEnd) origin scale VecColor.yAxis 2.0

/-- Render the transform hierarchy visualization. -/
def renderTransformHierarchy (state : TransformHierarchyState)
    (w h : Float) (screenScale : Float) (fontMedium fontSmall : Font) : CanvasM Unit := do
  let origin : Float × Float := (w / 2, h / 2)
  let scale : Float := 80.0 * screenScale

  drawGrid2D {
    origin := origin
    scale := scale
    width := w
    height := h
    majorSpacing := 2.0
  } fontSmall

  let localPose := lerpPose poseA poseB state.blend
  let rootLocal := localPose.getD 0 Linalg.Transform.identity
  let elbowLocal := localPose.getD 1 Linalg.Transform.identity
  let wristLocal := localPose.getD 2 Linalg.Transform.identity

  let hierarchy :=
    let h0 := Linalg.TransformHierarchy.singleton rootLocal
    match Linalg.TransformHierarchy.addChild h0 elbowLocal 0 with
    | some (h1, elbowIdx) =>
        match Linalg.TransformHierarchy.addChild h1 wristLocal elbowIdx with
        | some (h2, _) => h2
        | none => h1
    | none => h0

  let rootWorld := (Linalg.TransformHierarchy.getWorld hierarchy 0).getD Linalg.Transform.identity
  let elbowWorld := (Linalg.TransformHierarchy.getWorld hierarchy 1).getD Linalg.Transform.identity
  let wristWorld := (Linalg.TransformHierarchy.getWorld hierarchy 2).getD Linalg.Transform.identity

  let rootMat := rootWorld.toMat4
  let elbowMat := elbowWorld.toMat4
  let wristMat := wristWorld.toMat4

  let rootPos := vec3ToVec2 (rootMat.transformPoint Vec3.zero)
  let elbowPos := vec3ToVec2 (elbowMat.transformPoint Vec3.zero)
  let wristPos := vec3ToVec2 (wristMat.transformPoint Vec3.zero)
  let handPos := vec3ToVec2 (wristMat.transformPoint (Vec3.mk handLen 0.0 0.0))

  if state.showWorldGizmos then
    drawLineWorld Vec2.zero (Vec2.mk 1.2 0.0) origin scale VecColor.xAxis 2.0
    drawLineWorld Vec2.zero (Vec2.mk 0.0 1.2) origin scale VecColor.yAxis 2.0

  if state.showLocalGizmos then
    drawAxes rootWorld origin scale 0.7
    drawAxes elbowWorld origin scale 0.6
    drawAxes wristWorld origin scale 0.5

  drawLineWorld rootPos elbowPos origin scale (Color.rgba 0.3 0.8 1.0 0.9) 4.0
  drawLineWorld elbowPos wristPos origin scale (Color.rgba 0.4 0.9 0.4 0.9) 4.0
  drawLineWorld wristPos handPos origin scale (Color.rgba 1.0 0.7 0.3 0.9) 4.0

  drawMarker rootPos origin scale (Color.rgba 0.9 0.9 0.9 1.0) 9.0
  drawMarker elbowPos origin scale (Color.rgba 0.9 0.9 0.9 1.0) 8.0
  drawMarker wristPos origin scale (Color.rgba 0.9 0.9 0.9 1.0) 7.0
  drawMarker handPos origin scale (Color.rgba 1.0 0.8 0.3 1.0) 7.0

  fillTextXY "TRANSFORM HIERARCHY" (20 * screenScale) (30 * screenScale) fontMedium
  setFillColor (Color.gray 0.6)
  let animText := if state.animating then "animating" else "paused"
  fillTextXY
    s!"Space: {animText} | L: local gizmos | W: world gizmos | [: blend - | ]: blend +"
    (20 * screenScale) (55 * screenScale) fontSmall

  let infoY := h - 120 * screenScale
  setFillColor VecColor.label
  fillTextXY s!"blend: {formatFloat state.blend}" (20 * screenScale) infoY fontSmall
  fillTextXY s!"root world: {formatVec2 rootPos}" (20 * screenScale) (infoY + 20 * screenScale) fontSmall
  fillTextXY s!"elbow world: {formatVec2 elbowPos}" (20 * screenScale) (infoY + 40 * screenScale) fontSmall
  fillTextXY s!"wrist world: {formatVec2 wristPos}" (20 * screenScale) (infoY + 60 * screenScale) fontSmall

/-- Create the transform hierarchy widget. -/
def transformHierarchyWidget (env : DemoEnv) (state : TransformHierarchyState)
    : Afferent.Arbor.WidgetBuilder := do
  Afferent.Arbor.custom (spec := {
    measure := fun _ _ => (0, 0)
    collect := fun _ => #[]
    draw := some (fun layout => do
      withContentRect layout fun w h => do
        resetTransform
        renderTransformHierarchy state w h env.screenScale env.fontMedium env.fontSmall
    )
  }) (style := { flexItem := some (Trellis.FlexItem.growing 1) })

end Demos.Linalg
