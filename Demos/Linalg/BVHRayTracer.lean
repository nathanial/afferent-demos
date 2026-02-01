/-
  BVH Ray Tracer - visualize BVH traversal versus brute force ray tests.
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
import Linalg.Geometry.Triangle
import Linalg.Geometry.Ray
import Linalg.Geometry.Intersection
import Linalg.Spatial.BVH

open Afferent CanvasM Linalg

namespace Demos.Linalg

open Linalg.Spatial

instance : Inhabited Triangle where
  default := { v0 := Vec3.zero, v1 := Vec3.zero, v2 := Vec3.zero }

instance : Linalg.Spatial.Bounded3D Triangle where
  bounds := fun t =>
    let (min, max) := Triangle.boundingBox t
    AABB.fromMinMax min max

/-- State for BVH ray tracer demo. -/
structure BVHRayTracerState where
  triangles : Array Triangle
  rayOrigin : Vec2 := Vec2.zero
  maxT : Float := 6.0
  showNodes : Bool := true
  showVisited : Bool := true
  deriving Inhabited

private def triangleAt (center : Vec2) (size : Float) : Triangle :=
  let a := Vec3.mk (center.x - size) (center.y - size * 0.6) 0.0
  let b := Vec3.mk (center.x + size * 0.9) (center.y - size * 0.3) 0.0
  let c := Vec3.mk (center.x) (center.y + size) 0.0
  Triangle.mk' a b c

private def sampleTriangles : Array Triangle :=
  let centers := #[
    Vec2.mk (-3.2) (-1.5), Vec2.mk (-1.5) 1.2, Vec2.mk (0.8) (-0.6),
    Vec2.mk (2.5) 1.3, Vec2.mk (3.2) (-1.2), Vec2.mk (-2.2) 2.6
  ]
  centers.mapIdx fun i c =>
    let size := 0.6 + 0.15 * Float.sin (i.toFloat * 0.9)
    triangleAt c size

/-- Initial state. -/
def bvhRayTracerInitialState : BVHRayTracerState := {
  triangles := sampleTriangles
  rayOrigin := Vec2.mk 0.2 0.4
}

private def drawLineWorld (a b : Vec2) (origin : Float × Float) (scale : Float)
    (color : Color) (lineWidth : Float := 1.6) : CanvasM Unit := do
  let (sx, sy) := worldToScreen a origin scale
  let (ex, ey) := worldToScreen b origin scale
  setStrokeColor color
  setLineWidth lineWidth
  let path := Afferent.Path.empty
    |>.moveTo (Point.mk sx sy)
    |>.lineTo (Point.mk ex ey)
  strokePath path

private def drawTriangle2D (tri : Triangle) (origin : Float × Float) (scale : Float)
    (color : Color) : CanvasM Unit := do
  let a := Vec2.mk tri.v0.x tri.v0.y
  let b := Vec2.mk tri.v1.x tri.v1.y
  let c := Vec2.mk tri.v2.x tri.v2.y
  let (ax, ay) := worldToScreen a origin scale
  let (bx, by') := worldToScreen b origin scale
  let (cx, cy) := worldToScreen c origin scale
  setStrokeColor color
  setLineWidth 1.6
  let path := Afferent.Path.empty
    |>.moveTo (Point.mk ax ay)
    |>.lineTo (Point.mk bx by')
    |>.lineTo (Point.mk cx cy)
    |>.closePath
  strokePath path

private def drawRectWorld (b : AABB) (origin : Float × Float) (scale : Float)
    (color : Color) (lineWidth : Float := 1.0) : CanvasM Unit := do
  let min2 := Vec2.mk b.min.x b.min.y
  let max2 := Vec2.mk b.max.x b.max.y
  let (sx1, sy1) := worldToScreen min2 origin scale
  let (sx2, sy2) := worldToScreen max2 origin scale
  let x := Float.min sx1 sx2
  let y := Float.min sy1 sy2
  let w := Float.abs' (sx2 - sx1)
  let h := Float.abs' (sy2 - sy1)
  setStrokeColor color
  setLineWidth lineWidth
  strokePath (Afferent.Path.rectangleXYWH x y w h)

private partial def drawBVHNode (node : BVHNode) (origin : Float × Float) (scale : Float)
    (depth : Nat) (color : Color) : CanvasM Unit := do
  drawRectWorld node.bounds origin scale color 1.0
  match node with
  | .leaf _ _ => pure ()
  | .branch _ left right =>
      let next := Color.rgba color.r color.g color.b (Float.max (color.a - 0.08) 0.1)
      drawBVHNode left origin scale (depth + 1) next
      drawBVHNode right origin scale (depth + 1) next

private partial def collectVisitedNodes (node : BVHNode) (ray : Ray) (acc : Array AABB) : Array AABB :=
  match Intersection.rayAABB ray node.bounds with
  | none => acc
  | some _ =>
      let acc' := acc.push node.bounds
      match node with
      | .leaf _ _ => acc'
      | .branch _ left right =>
          let acc'' := collectVisitedNodes left ray acc'
          collectVisitedNodes right ray acc''

private partial def countBVHTriangleTests (node : BVHNode) (ray : Ray) : Nat :=
  match Intersection.rayAABB ray node.bounds with
  | none => 0
  | some _ =>
      match node with
      | .leaf _ indices => indices.size
      | .branch _ left right =>
          countBVHTriangleTests left ray + countBVHTriangleTests right ray

private def bruteForceHit (tris : Array Triangle) (ray : Ray) : Option RayHit × Nat :=
  tris.foldl (fun (best, count) tri =>
    let count' := count + 1
    match Intersection.rayTriangle ray tri with
    | none => (best, count')
    | some hit =>
        match best with
        | some b =>
            if hit.t < b.t then (some hit, count') else (best, count')
        | none =>
            (some hit, count')
  ) (none, 0)

/-- Render BVH ray tracer demo. -/
def renderBVHRayTracer (state : BVHRayTracerState)
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

  let bvh := BVH.build state.triangles BVHConfig.triangles
  let rayOrigin := Vec3.mk state.rayOrigin.x state.rayOrigin.y 3.5
  let rayDir := Vec3.mk 0.3 (-0.6) (-1.0)
  let ray := Ray.mk' rayOrigin rayDir
  let triHit := fun idx =>
    if h : idx < state.triangles.size then
      Intersection.rayTriangle ray state.triangles[idx]
    else none
  let bvhHit := BVH.rayCast bvh ray triHit
  let anyHit := BVH.rayAny bvh ray state.maxT (fun idx =>
    match triHit idx with
    | some hit => hit.t <= state.maxT
    | none => false)
  let (bruteHit, bruteTests) := bruteForceHit state.triangles ray
  let bvhTests := countBVHTriangleTests bvh.root ray
  let visited := collectVisitedNodes bvh.root ray #[]

  for tri in state.triangles do
    drawTriangle2D tri origin scale (Color.rgba 0.6 0.7 0.9 0.9)

  if state.showNodes then
    drawBVHNode bvh.root origin scale 0 (Color.rgba 0.3 0.6 0.9 0.18)

  if state.showVisited then
    for b in visited do
      drawRectWorld b origin scale (Color.rgba 1.0 0.6 0.2 0.7) 2.0

  let hitPos := match bvhHit with
    | some hit => some (Vec2.mk hit.point.x hit.point.y)
    | none => none
  let rayStart2 := Vec2.mk state.rayOrigin.x state.rayOrigin.y
  let rayEndPos := match hitPos with
    | some p => p
    | none => rayStart2.add (Vec2.mk (ray.direction.x * state.maxT) (ray.direction.y * state.maxT))
  drawLineWorld rayStart2 rayEndPos origin scale (Color.rgba 1.0 0.9 0.2 1.0) 2.6
  drawMarker rayStart2 origin scale (Color.rgba 1.0 0.9 0.2 1.0) 6.0
  match hitPos with
  | some p => drawMarker p origin scale (Color.rgba 1.0 0.5 0.2 1.0) 9.0
  | none => pure ()

  let infoY := h - 150 * screenScale
  setFillColor VecColor.label
  fillTextXY s!"BVH nodes tested: {visited.size}  BVH tris tested: {bvhTests}"
    (20 * screenScale) infoY fontSmall
  fillTextXY s!"Brute force tests: {bruteTests}  BVH hit: {bvhHit.isSome}  any: {anyHit}"
    (20 * screenScale) (infoY + 20 * screenScale) fontSmall
  match bruteHit with
  | some hit =>
      fillTextXY s!"Brute hit t={formatFloat hit.t}" (20 * screenScale) (infoY + 40 * screenScale) fontSmall
  | none =>
      fillTextXY "Brute hit: none" (20 * screenScale) (infoY + 40 * screenScale) fontSmall

  fillTextXY "BVH RAY TRACER" (20 * screenScale) (30 * screenScale) fontMedium
  setFillColor (Color.gray 0.6)
  fillTextXY "Move mouse: ray origin | V: visited | B: nodes | R: reset"
    (20 * screenScale) (55 * screenScale) fontSmall

/-- Create BVH ray tracer widget. -/
def bvhRayTracerWidget (env : DemoEnv) (state : BVHRayTracerState)
    : Afferent.Arbor.WidgetBuilder := do
  Afferent.Arbor.custom (spec := {
    measure := fun _ _ => (0, 0)
    collect := fun _ => #[]
    draw := some (fun layout => do
      withContentRect layout fun w h => do
        resetTransform
        renderBVHRayTracer state w h env.screenScale env.fontMedium env.fontSmall
    )
  }) (style := { flexItem := some (Trellis.FlexItem.growing 1) })

end Demos.Linalg
