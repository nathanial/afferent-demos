/-
  Primitive Overlap Tester - overlap detection between basic primitives.
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
import Linalg.Geometry.Circle
import Linalg.Geometry.AABB
import Linalg.Geometry.AABB2D
import Linalg.Geometry.Sphere
import Linalg.Geometry.Intersection
import Linalg.Geometry.Collision2D
import AfferentMath.Widget.MathView2D

open Afferent CanvasM Linalg
open Afferent.Widget
open AfferentMath.Widget

namespace Demos.Linalg

/-- Overlap test mode. -/
inductive OverlapMode where
  | sphereSphere
  | aabbAabb
  | sphereAabb
  deriving BEq, Inhabited

/-- Drag target for overlap tester. -/
inductive OverlapDragTarget where
  | none
  | shapeA
  | shapeB
  deriving BEq, Inhabited

/-- State for primitive overlap tester. -/
structure PrimitiveOverlapTesterState where
  mode : OverlapMode := .sphereSphere
  centerA : Vec2 := Vec2.mk (-1.4) 0.2
  centerB : Vec2 := Vec2.mk 1.2 0.8
  radiusA : Float := 1.0
  radiusB : Float := 0.8
  aabbExtentsA : Vec2 := Vec2.mk 1.0 0.8
  aabbExtentsB : Vec2 := Vec2.mk 0.9 0.6
  dragging : OverlapDragTarget := .none
  deriving Inhabited

/-- Initial state. -/
def primitiveOverlapTesterInitialState : PrimitiveOverlapTesterState := {}

def primitiveOverlapMathViewConfig (screenScale : Float) : MathView2D.Config := {
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

private def drawCircleWorld (center : Vec2) (radius : Float) (origin : Float × Float)
    (scale : Float) (stroke : Color) (fill : Option Color := none) (lineWidth : Float := 2.0) : CanvasM Unit := do
  let (sx, sy) := worldToScreen center origin scale
  let sr := radius * scale
  match fill with
  | some c =>
      setFillColor c
      fillPath (Afferent.Path.circle (Point.mk sx sy) sr)
  | none => pure ()
  setStrokeColor stroke
  setLineWidth lineWidth
  strokePath (Afferent.Path.circle (Point.mk sx sy) sr)

private def drawAABBWorld (center extents : Vec2) (origin : Float × Float) (scale : Float)
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

private def drawArrowWorld (start finish : Vec2) (origin : Float × Float) (scale : Float)
    (color : Color) : CanvasM Unit := do
  let s := worldToScreen start origin scale
  let f := worldToScreen finish origin scale
  drawArrow2D s f { color := color, lineWidth := 2.0 }

/-- Render the overlap tester. -/
def renderPrimitiveOverlapTester (state : PrimitiveOverlapTesterState)
    (view : MathView2D.View) (screenScale : Float) (fontMedium fontSmall : Font) : CanvasM Unit := do
  let w := view.width
  let h := view.height
  let origin : Float × Float := (view.origin.x, view.origin.y)
  let scale := view.scale

  let infoY := h - 160 * screenScale
  setFillColor VecColor.label

  match state.mode with
  | .sphereSphere =>
      let sphereA := Sphere.mk' (Vec3.mk state.centerA.x 0.0 state.centerA.y) state.radiusA
      let sphereB := Sphere.mk' (Vec3.mk state.centerB.x 0.0 state.centerB.y) state.radiusB
      let colliding := Intersection.sphereSphere sphereA sphereB
      let penetration := Intersection.sphereSpherePenetration sphereA sphereB
      let diff := sphereB.center.sub sphereA.center
      let normal := if diff.length < Float.epsilon then Vec3.unitX else diff.normalize
      let normal2D := Vec2.mk normal.x normal.z

      drawCircleWorld state.centerA state.radiusA origin scale (Color.rgba 0.3 0.8 1.0 0.9)
        (if colliding then some (Color.rgba 0.3 0.8 1.0 0.15) else none)
      drawCircleWorld state.centerB state.radiusB origin scale (Color.rgba 1.0 0.6 0.3 0.9)
        (if colliding then some (Color.rgba 1.0 0.6 0.3 0.15) else none)

      if colliding then
        let arrowEnd := state.centerA.add (normal2D.scale penetration)
        drawArrowWorld state.centerA arrowEnd origin scale (Color.rgba 0.9 0.9 0.2 1.0)

      let circleA := Circle.mk' state.centerA.x state.centerA.y state.radiusA
      let circleB := Circle.mk' state.centerB.x state.centerB.y state.radiusB
      let collision2D := SAT.circleCircle circleA circleB

      fillTextXY s!"Intersection.sphereSphere: {colliding}" (20 * screenScale) infoY fontSmall
      fillTextXY s!"penetration: {formatFloat penetration}" (20 * screenScale) (infoY + 20 * screenScale) fontSmall
      fillTextXY s!"Collision2D.circleCircle: {collision2D.colliding} depth={formatFloat collision2D.depth}"
        (20 * screenScale) (infoY + 40 * screenScale) fontSmall

  | .aabbAabb =>
      let aabbA := AABB2D.fromCenterExtents state.centerA state.aabbExtentsA
      let aabbB := AABB2D.fromCenterExtents state.centerB state.aabbExtentsB
      let colliding := aabbA.intersects aabbB

      drawAABBWorld state.centerA state.aabbExtentsA origin scale (Color.rgba 0.3 0.8 1.0 0.9)
        (if colliding then some (Color.rgba 0.3 0.8 1.0 0.12) else none)
      drawAABBWorld state.centerB state.aabbExtentsB origin scale (Color.rgba 1.0 0.6 0.3 0.9)
        (if colliding then some (Color.rgba 1.0 0.6 0.3 0.12) else none)

      let dx := state.centerB.x - state.centerA.x
      let dy := state.centerB.y - state.centerA.y
      let px := state.aabbExtentsA.x + state.aabbExtentsB.x - Float.abs' dx
      let py := state.aabbExtentsA.y + state.aabbExtentsB.y - Float.abs' dy
      if colliding then
        let (normal, depth) := if px < py then
            (Vec2.mk (if dx >= 0.0 then 1.0 else -1.0) 0.0, px)
          else
            (Vec2.mk 0.0 (if dy >= 0.0 then 1.0 else -1.0), py)
        let arrowEnd := state.centerA.add (normal.scale depth)
        drawArrowWorld state.centerA arrowEnd origin scale (Color.rgba 0.9 0.9 0.2 1.0)

        match aabbA.intersection aabbB with
        | some overlap =>
            let overlapCenter := overlap.center
            let overlapExtents := overlap.extents
            drawAABBWorld overlapCenter overlapExtents origin scale (Color.rgba 0.9 0.9 0.2 0.8)
              (some (Color.rgba 0.9 0.9 0.2 0.2)) 1.0
        | none => pure ()

      fillTextXY s!"Intersection.aabbAABB: {colliding}" (20 * screenScale) infoY fontSmall
      fillTextXY s!"overlapX={formatFloat px}, overlapY={formatFloat py}" (20 * screenScale) (infoY + 20 * screenScale) fontSmall

  | .sphereAabb =>
      let sphere := Sphere.mk' (Vec3.mk state.centerA.x 0.0 state.centerA.y) state.radiusA
      let aabb := AABB.fromCenterExtents (Vec3.mk state.centerB.x 0.0 state.centerB.y)
        (Vec3.mk state.aabbExtentsB.x 0.4 state.aabbExtentsB.y)
      let colliding := Intersection.sphereAABB sphere aabb

      drawCircleWorld state.centerA state.radiusA origin scale (Color.rgba 0.3 0.8 1.0 0.9)
        (if colliding then some (Color.rgba 0.3 0.8 1.0 0.12) else none)
      drawAABBWorld state.centerB state.aabbExtentsB origin scale (Color.rgba 1.0 0.6 0.3 0.9)
        (if colliding then some (Color.rgba 1.0 0.6 0.3 0.12) else none)

      if colliding then
        let closest := aabb.closestPoint sphere.center
        let delta := sphere.center.sub closest
        let dist := delta.length
        let normal := if dist < Float.epsilon then Vec3.unitX else delta.scale (1.0 / dist)
        let depth := sphere.radius - dist
        let normal2D := Vec2.mk normal.x normal.z
        let arrowEnd := state.centerA.add (normal2D.scale depth)
        drawArrowWorld state.centerA arrowEnd origin scale (Color.rgba 0.9 0.9 0.2 1.0)

      fillTextXY s!"Intersection.sphereAABB: {colliding}" (20 * screenScale) infoY fontSmall

  -- Title and instructions
  fillTextXY "PRIMITIVE OVERLAP TESTER" (20 * screenScale) (30 * screenScale) fontMedium
  setFillColor (Color.gray 0.6)
  fillTextXY "1: sphere-sphere | 2: aabb-aabb | 3: sphere-aabb | Drag A/B" (20 * screenScale) (55 * screenScale) fontSmall

/-- Create the overlap tester widget. -/
def primitiveOverlapTesterWidget (env : DemoEnv) (state : PrimitiveOverlapTesterState)
    : Afferent.Arbor.WidgetBuilder := do
  let config := primitiveOverlapMathViewConfig env.screenScale
  MathView2D.mathView2D config env.fontSmall (fun view => do
    renderPrimitiveOverlapTester state view env.screenScale env.fontMedium env.fontSmall
  )

end Demos.Linalg
