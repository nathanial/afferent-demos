/-
  Swept Collision Demo - continuous collision detection for moving shapes.
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
import Linalg.Geometry.AABB
import Linalg.Physics

open Afferent CanvasM Linalg

namespace Demos.Linalg

/-- Swept collision modes. -/
inductive SweptMode where
  | sphere
  | aabb
  deriving BEq, Inhabited

/-- Drag targets. -/
inductive SweptDragTarget where
  | startPos
  | endPos
  | staticCenter
  deriving BEq, Inhabited

/-- State for swept collision demo. -/
structure SweptCollisionDemoState where
  mode : SweptMode := .sphere
  startPos : Vec2 := Vec2.mk (-4.0) (-1.5)
  endPos : Vec2 := Vec2.mk (4.0) (1.2)
  radius : Float := 0.7
  halfExtents : Vec2 := Vec2.mk 0.9 0.6
  staticCenter : Vec2 := Vec2.mk 1.2 0.2
  staticExtents : Vec2 := Vec2.mk 1.4 0.9
  animating : Bool := true
  time : Float := 0.0
  dragging : Option SweptDragTarget := none
  showDiscrete : Bool := true
  deriving Inhabited

/-- Initial state. -/
def sweptCollisionDemoInitialState : SweptCollisionDemoState := {}

private def vec2ToVec3 (v : Vec2) : Vec3 := Vec3.mk v.x v.y 0.0
private def vec3ToVec2 (v : Vec3) : Vec2 := Vec2.mk v.x v.y

private def aabbIntersects (a b : AABB) : Bool :=
  a.min.x <= b.max.x && a.max.x >= b.min.x &&
  a.min.y <= b.max.y && a.max.y >= b.min.y &&
  a.min.z <= b.max.z && a.max.z >= b.min.z

private def drawRectWorld (center extents : Vec2) (origin : Float × Float) (scale : Float)
    (stroke : Color) (fill : Option Color := none) (lineWidth : Float := 2.0) : CanvasM Unit := do
  let min := center.sub extents
  let max := center.add extents
  let (sx1, sy1) := worldToScreen min origin scale
  let (sx2, sy2) := worldToScreen max origin scale
  let x := Float.min sx1 sx2
  let y := Float.min sy1 sy2
  let w := Float.abs' (sx2 - sx1)
  let h := Float.abs' (sy2 - sy1)
  match fill with
  | some c =>
      setFillColor c
      fillPath (Afferent.Path.rectangleXYWH x y w h)
  | none => pure ()
  setStrokeColor stroke
  setLineWidth lineWidth
  strokePath (Afferent.Path.rectangleXYWH x y w h)

private def drawCircleWorld (center : Vec2) (radius : Float) (origin : Float × Float)
    (scale : Float) (stroke : Color) (lineWidth : Float := 2.0) : CanvasM Unit := do
  let (sx, sy) := worldToScreen center origin scale
  let sr := radius * scale
  setStrokeColor stroke
  setLineWidth lineWidth
  strokePath (Afferent.Path.circle (Point.mk sx sy) sr)

private def drawCapsule (start finish : Vec2) (radius : Float) (origin : Float × Float)
    (scale : Float) : CanvasM Unit := do
  let s := worldToScreen start origin scale
  let f := worldToScreen finish origin scale
  setStrokeColor (Color.rgba 0.4 0.9 0.5 0.7)
  setLineWidth 2.0
  let path := Afferent.Path.empty
    |>.moveTo (Point.mk s.1 s.2)
    |>.lineTo (Point.mk f.1 f.2)
  strokePath path
  drawCircleWorld start radius origin scale (Color.rgba 0.4 0.9 0.5 0.7) 1.5
  drawCircleWorld finish radius origin scale (Color.rgba 0.4 0.9 0.5 0.7) 1.5

/-- Render swept collision demo. -/
def renderSweptCollisionDemo (state : SweptCollisionDemoState)
    (w h : Float) (screenScale : Float) (fontMedium fontSmall : Font) : CanvasM Unit := do
  let origin : Float × Float := (w / 2, h / 2)
  let scale : Float := 70.0 * screenScale

  drawGrid2D {
    origin := origin
    scale := scale
    width := w
    height := h
    majorSpacing := 2.0
  } fontSmall

  let t := if state.animating then
      0.5 + 0.5 * Float.sin state.time
    else
      0.5
  let current := Vec2.lerp state.startPos state.endPos t

  let staticAABB := AABB.fromCenterExtents (vec2ToVec3 state.staticCenter)
    (vec2ToVec3 state.staticExtents)

  let hit :=
    match state.mode with
    | .sphere => SweptCollision.sphereVsAABB (vec2ToVec3 state.startPos) (vec2ToVec3 state.endPos)
        state.radius staticAABB
    | .aabb => SweptCollision.aabbVsAABB (vec2ToVec3 state.startPos) (vec2ToVec3 state.endPos)
        (vec2ToVec3 state.halfExtents) staticAABB

  drawRectWorld state.staticCenter state.staticExtents origin scale (Color.rgba 0.8 0.4 0.4 0.9)
    (some (Color.rgba 0.8 0.4 0.4 0.12))

  match state.mode with
  | .sphere =>
      drawCapsule state.startPos state.endPos state.radius origin scale
      drawCircleWorld current state.radius origin scale (Color.rgba 0.2 0.8 1.0 0.9) 2.4
      drawCircleWorld state.startPos state.radius origin scale (Color.gray 0.6) 1.4
      drawCircleWorld state.endPos state.radius origin scale (Color.gray 0.6) 1.4
  | .aabb =>
      drawRectWorld state.startPos state.halfExtents origin scale (Color.gray 0.6) none 1.2
      drawRectWorld state.endPos state.halfExtents origin scale (Color.gray 0.6) none 1.2
      drawRectWorld current state.halfExtents origin scale (Color.rgba 0.2 0.8 1.0 0.9)
        (some (Color.rgba 0.2 0.8 1.0 0.12))
      let cornersStart := AABB.fromCenterExtents (vec2ToVec3 state.startPos) (vec2ToVec3 state.halfExtents)
      let cornersEnd := AABB.fromCenterExtents (vec2ToVec3 state.endPos) (vec2ToVec3 state.halfExtents)
      let minStart := vec3ToVec2 cornersStart.min
      let maxStart := vec3ToVec2 cornersStart.max
      let minEnd := vec3ToVec2 cornersEnd.min
      let maxEnd := vec3ToVec2 cornersEnd.max
      drawDashedLine (worldToScreen minStart origin scale) (worldToScreen minEnd origin scale)
        (Color.rgba 0.4 0.9 0.5 0.7) 6.0 4.0 1.2
      drawDashedLine (worldToScreen maxStart origin scale) (worldToScreen maxEnd origin scale)
        (Color.rgba 0.4 0.9 0.5 0.7) 6.0 4.0 1.2

  match hit with
  | some hhit =>
      let p2 := vec3ToVec2 hhit.point
      drawMarker p2 origin scale (Color.rgba 1.0 0.9 0.2 1.0) 8.0
      let n2 := Vec2.mk hhit.normal.x hhit.normal.y
      let endN := p2.add (n2.scale 1.0)
      drawArrow2D (worldToScreen p2 origin scale) (worldToScreen endN origin scale)
        { color := Color.rgba 1.0 0.9 0.2 1.0, lineWidth := 2.0 }
  | none => pure ()

  let discreteHit :=
    if state.showDiscrete then
      (match state.mode with
      | .sphere =>
          staticAABB.distance (vec2ToVec3 current) <= state.radius
      | .aabb =>
          let moving := AABB.fromCenterExtents (vec2ToVec3 current) (vec2ToVec3 state.halfExtents)
          aabbIntersects moving staticAABB)
    else
      false

  let tunneling := state.showDiscrete && hit.isSome && !discreteHit

  let infoY := h - 120 * screenScale
  setFillColor VecColor.label
  fillTextXY s!"mode: {if state.mode == .sphere then "Sphere" else "AABB"}" (20 * screenScale)
    infoY fontSmall
  match hit with
  | some hhit =>
      fillTextXY s!"swept hit t={formatFloat hhit.t}" (20 * screenScale) (infoY + 20 * screenScale) fontSmall
      fillTextXY s!"point={formatVec2 (vec3ToVec2 hhit.point)}" (20 * screenScale) (infoY + 40 * screenScale) fontSmall
  | none =>
      fillTextXY "swept hit: none" (20 * screenScale) (infoY + 20 * screenScale) fontSmall
  let discreteText := if state.showDiscrete then
      s!"discrete collision: {discreteHit}"
    else
      "discrete collision: hidden"
  let tunnelText := if state.showDiscrete then
      s!"tunneling: {tunneling}"
    else
      "tunneling: n/a"
  fillTextXY discreteText (20 * screenScale) (infoY + 60 * screenScale) fontSmall
  fillTextXY tunnelText (20 * screenScale) (infoY + 80 * screenScale) fontSmall

  fillTextXY "SWEPT COLLISION DEMO" (20 * screenScale) (30 * screenScale) fontMedium
  setFillColor (Color.gray 0.6)
  let animText := if state.animating then "animating" else "paused"
  fillTextXY s!"Space: {animText} | M: mode | D: discrete | Drag: start/end/static" (20 * screenScale) (55 * screenScale)
    fontSmall

/-- Create swept collision widget. -/
def sweptCollisionDemoWidget (env : DemoEnv) (state : SweptCollisionDemoState)
    : Afferent.Arbor.WidgetBuilder := do
  Afferent.Arbor.custom (spec := {
    measure := fun _ _ => (0, 0)
    collect := fun _ => #[]
    draw := some (fun layout => do
      withContentRect layout fun w h => do
        resetTransform
        renderSweptCollisionDemo state w h env.screenScale env.fontMedium env.fontSmall
    )
  }) (style := { flexItem := some (Trellis.FlexItem.growing 1) })

end Demos.Linalg
