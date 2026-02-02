/-
  Octree Viewer 3D - visualize 3D spatial partitioning with projected boxes.
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
import Linalg.Geometry.Intersection
import Linalg.Spatial.Octree

open Afferent CanvasM Linalg

namespace Demos.Linalg

open Linalg.Spatial

/-- State for octree viewer. -/
structure OctreeViewer3DState where
  items : Array AABB
  queryCenter : Vec3 := Vec3.zero
  queryExtents : Vec3 := Vec3.mk 1.5 1.0 1.2
  spawnPhase : Float := 0.0
  yaw : Float := 0.0
  pitch : Float := 0.0
  config : TreeConfig := TreeConfig.default
  showNodes : Bool := true
  deriving Inhabited

private def cube (center : Vec3) (size : Float) : AABB :=
  AABB.fromCenterExtents center (Vec3.mk size size size)

private def sampleItems : Array AABB :=
  (Array.range 18).map fun i =>
    let t := i.toFloat
    let x := Float.sin (t * 0.5) * 3.0
    let y := Float.cos (t * 0.35) * 2.5
    let z := Float.sin (t * 0.2) * 2.2
    let size := 0.25 + 0.1 * Float.sin (t * 0.7)
    cube (Vec3.mk x y z) size

/-- Initial state. -/
def octreeViewer3DInitialState : OctreeViewer3DState := {
  items := sampleItems
  queryCenter := Vec3.mk 0.5 (-0.2) 0.4
}

private def rotateVec3 (v : Vec3) (yaw pitch : Float) : Vec3 :=
  let cy := Float.cos yaw
  let sy := Float.sin yaw
  let cx := Float.cos pitch
  let sx := Float.sin pitch
  let x1 := v.x * cy + v.z * sy
  let z1 := -v.x * sy + v.z * cy
  let y1 := v.y
  let y2 := y1 * cx - z1 * sx
  let z2 := y1 * sx + z1 * cx
  Vec3.mk x1 y2 z2

private def project3D (yaw pitch : Float) (v : Vec3) : Vec2 :=
  -- Simple isometric-ish projection
  let r := rotateVec3 v yaw pitch
  let x := r.x + r.z * 0.6
  let y := r.y + r.z * 0.25
  Vec2.mk x y

private def drawLineWorld (a b : Vec2) (origin : Float × Float) (scale : Float)
    (color : Color) (lineWidth : Float := 1.4) : CanvasM Unit := do
  let (sx, sy) := worldToScreen a origin scale
  let (ex, ey) := worldToScreen b origin scale
  setStrokeColor color
  setLineWidth lineWidth
  let path := Afferent.Path.empty
    |>.moveTo (Point.mk sx sy)
    |>.lineTo (Point.mk ex ey)
  strokePath path

private def drawBox (b : AABB) (origin : Float × Float) (scale : Float)
    (yaw pitch : Float) (color : Color) (lineWidth : Float := 1.2) : CanvasM Unit := do
  let min := b.min
  let max := b.max
  let corners : Array Vec3 := #[
    Vec3.mk min.x min.y min.z, Vec3.mk max.x min.y min.z,
    Vec3.mk max.x max.y min.z, Vec3.mk min.x max.y min.z,
    Vec3.mk min.x min.y max.z, Vec3.mk max.x min.y max.z,
    Vec3.mk max.x max.y max.z, Vec3.mk min.x max.y max.z
  ]
  let p := corners.map (project3D yaw pitch)
  let edges : Array (Nat × Nat) := #[
    (0, 1), (1, 2), (2, 3), (3, 0),
    (4, 5), (5, 6), (6, 7), (7, 4),
    (0, 4), (1, 5), (2, 6), (3, 7)
  ]
  for (a, b) in edges do
    drawLineWorld p[a]! p[b]! origin scale color lineWidth

private def nodeColor (depth : Nat) : Color :=
  let t := Float.min (depth.toFloat / 6.0) 1.0
  Color.rgba (0.2 + 0.4 * t) (0.7 - 0.3 * t) (0.9 - 0.5 * t) 0.6

private partial def drawOctreeNode (node : OctreeNode) (origin : Float × Float)
    (scale : Float) (depth : Nat) (yaw pitch : Float) : CanvasM Unit := do
  let bounds := match node with
    | .internal b _ => b
    | .leaf b _ => b
  drawBox bounds origin scale yaw pitch (nodeColor depth) 1.1
  match node with
  | .leaf _ _ => pure ()
  | .internal _ children =>
      for child in children do
        match child with
        | some c => drawOctreeNode c origin scale (depth + 1) yaw pitch
        | none => pure ()

private def buildOctree (items : Array AABB) (config : TreeConfig) : Octree :=
  Octree.build items config

/-- Render octree viewer. -/
def renderOctreeViewer3D (state : OctreeViewer3DState)
    (w h : Float) (screenScale : Float) (fontMedium fontSmall : Font) : CanvasM Unit := do
  let origin : Float × Float := (w / 2, h / 2)
  let scale : Float := 60.0 * screenScale

  drawGrid2D {
    origin := origin
    scale := scale
    width := w
    height := h
    majorSpacing := 2.0
  } fontSmall

  let tree := buildOctree state.items state.config
  if state.showNodes then
    drawOctreeNode tree.root origin scale 0 state.yaw state.pitch

  let queryBox := AABB.fromCenterExtents state.queryCenter state.queryExtents
  let broadHits := tree.queryAABB queryBox
  let exactHits := broadHits.filter fun idx =>
    if h : idx < state.items.size then
      Intersection.aabbAABB state.items[idx]! queryBox
    else false

  for i in [:state.items.size] do
    let item := state.items[i]!
    let center := item.center
    let pos2 := project3D state.yaw state.pitch center
    let isExact := exactHits.contains i
    let isBroad := broadHits.contains i
    let color :=
      if isExact then
        Color.rgba 1.0 0.85 0.2 1.0
      else if isBroad then
        Color.rgba 0.95 0.55 0.2 0.95
      else
        Color.rgba 0.8 0.9 1.0 0.9
    drawMarker pos2 origin scale color 7.0

  drawBox queryBox origin scale state.yaw state.pitch (Color.rgba 0.9 0.8 0.2 0.9) 2.0

  let octant := octantFor tree.bounds.center state.queryCenter

  let infoY := h - 150 * screenScale
  setFillColor VecColor.label
  fillTextXY
    s!"objects: {state.items.size}  hits: {exactHits.size}  candidates: {broadHits.size}"
    (20 * screenScale) infoY fontSmall
  fillTextXY s!"query: {formatVec3 state.queryCenter}  extents: {formatVec3 state.queryExtents}"
    (20 * screenScale) (infoY + 20 * screenScale) fontSmall
  fillTextXY s!"octant: {octant.val}  depth: {tree.maxDepth}" (20 * screenScale)
    (infoY + 40 * screenScale) fontSmall
  fillTextXY s!"yaw: {formatFloat state.yaw}  pitch: {formatFloat state.pitch}"
    (20 * screenScale) (infoY + 60 * screenScale) fontSmall

  fillTextXY "OCTREE VIEWER 3D" (20 * screenScale) (30 * screenScale) fontMedium
  setFillColor (Color.gray 0.6)
  fillTextXY "Click: add box | X: remove | WASD/UJ move | +/- size | arrows rotate | V toggle nodes"
    (20 * screenScale) (55 * screenScale) fontSmall
  fillTextXY "Legend: exact hits = yellow, broad-phase candidates = orange"
    (20 * screenScale) (75 * screenScale) fontSmall

/-- Create octree viewer widget. -/
def octreeViewer3DWidget (env : DemoEnv) (state : OctreeViewer3DState)
    : Afferent.Arbor.WidgetBuilder := do
  Afferent.Arbor.custom (spec := {
    measure := fun _ _ => (0, 0)
    collect := fun _ => #[]
    draw := some (fun layout => do
      withContentRect layout fun w h => do
        resetTransform
        renderOctreeViewer3D state w h env.screenScale env.fontMedium env.fontSmall
    )
  }) (style := { flexItem := some (Trellis.FlexItem.growing 1) })

end Demos.Linalg
