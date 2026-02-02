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
import Linalg.Vec3
import Linalg.Geometry.AABB
import Linalg.Geometry.Intersection
import Linalg.Spatial.Octree

open Afferent CanvasM Linalg
open Afferent.Widget

namespace Demos.Linalg

open Linalg.Spatial

/-- State for octree viewer. -/
structure OctreeViewer3DState where
  items : Array AABB
  queryCenter : Vec3 := Vec3.zero
  queryExtents : Vec3 := Vec3.mk 1.5 1.0 1.2
  spawnPhase : Float := 0.0
  cameraYaw : Float := 0.6
  cameraPitch : Float := 0.35
  cameraDistance : Float := 10.0
  cameraTarget : Vec3 := Vec3.zero
  cameraFov : Float := Float.pi / 3
  cameraNear : Float := 0.2
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

def octreeViewer3DMathViewConfig (state : OctreeViewer3DState) (screenScale : Float)
    : MathView3D.Config := {
  style := { flexItem := some (Trellis.FlexItem.growing 1) }
  camera := {
    yaw := state.cameraYaw
    pitch := state.cameraPitch
    distance := state.cameraDistance
    target := state.cameraTarget
  }
  fov := state.cameraFov
  near := state.cameraNear
  gridExtent := 7.0
  gridStep := 1.0
  gridMajorStep := 3.0
  showAxes := false
  axisLineWidth := 2.0 * screenScale
  gridLineWidth := 1.0 * screenScale
}

private def drawBox (view : MathView3D.View) (b : AABB)
    (color : Color) (lineWidth : Float := 1.2) : CanvasM Unit := do
  let min := b.min
  let max := b.max
  let corners : Array Vec3 := #[
    Vec3.mk min.x min.y min.z, Vec3.mk max.x min.y min.z,
    Vec3.mk max.x max.y min.z, Vec3.mk min.x max.y min.z,
    Vec3.mk min.x min.y max.z, Vec3.mk max.x min.y max.z,
    Vec3.mk max.x max.y max.z, Vec3.mk min.x max.y max.z
  ]
  let edges : Array (Nat × Nat) := #[
    (0, 1), (1, 2), (2, 3), (3, 0),
    (4, 5), (5, 6), (6, 7), (7, 4),
    (0, 4), (1, 5), (2, 6), (3, 7)
  ]
  for (a, b) in edges do
    MathView3D.drawLine3D view corners[a]! corners[b]! color lineWidth

private def drawGroundGrid (view : MathView3D.View) (screenScale : Float) : CanvasM Unit := do
  let gridCount : Nat := 7
  let step : Float := 1.0
  let extent := gridCount.toFloat * step
  for i in [: (gridCount * 2 + 1)] do
    let offset := (i.toFloat - gridCount.toFloat) * step
    let color :=
      if offset == 0.0 then Color.rgba 0.6 0.7 0.9 0.45 else Color.rgba 0.4 0.5 0.7 0.3
    let width := if offset == 0.0 then 2.0 * screenScale else 1.0 * screenScale
    let a1 := Vec3.mk (-extent) offset 0.0
    let b1 := Vec3.mk extent offset 0.0
    let a2 := Vec3.mk offset (-extent) 0.0
    let b2 := Vec3.mk offset extent 0.0
    MathView3D.drawLine3D view a1 b1 color width
    MathView3D.drawLine3D view a2 b2 color width

def screenToWorldOnPlane (view : MathView3D.View) (x y planeZ : Float) : Option Vec3 :=
  MathView3D.screenToWorldOnPlane view (x, y) (Vec3.mk 0.0 0.0 planeZ) Vec3.unitZ

private def nodeColor (depth : Nat) : Color :=
  let t := Float.min (depth.toFloat / 6.0) 1.0
  Color.rgba (0.2 + 0.4 * t) (0.7 - 0.3 * t) (0.9 - 0.5 * t) 0.6

private partial def drawOctreeNode (node : OctreeNode) (view : MathView3D.View) (depth : Nat)
    : CanvasM Unit := do
  let bounds := match node with
    | .internal b _ => b
    | .leaf b _ => b
  drawBox view bounds (nodeColor depth) 1.1
  match node with
  | .leaf _ _ => pure ()
  | .internal _ children =>
      for child in children do
        match child with
        | some c => drawOctreeNode c view (depth + 1)
        | none => pure ()

private def buildOctree (items : Array AABB) (config : TreeConfig) : Octree :=
  Octree.build items config

/-- Render octree viewer. -/
def renderOctreeViewer3D (state : OctreeViewer3DState)
    (view : MathView3D.View) (screenScale : Float) (fontMedium fontSmall : Font) : CanvasM Unit := do
  let w := view.width
  let h := view.height

  drawGroundGrid view screenScale

  let tree := buildOctree state.items state.config
  if state.showNodes then
    drawOctreeNode tree.root view 0

  let queryBox := AABB.fromCenterExtents state.queryCenter state.queryExtents
  let broadHits := tree.queryAABB queryBox
  let exactHits := broadHits.filter fun idx =>
    if idx < state.items.size then
      Intersection.aabbAABB state.items[idx]! queryBox
    else false

  for i in [:state.items.size] do
    let item := state.items[i]!
    let center := item.center
    let isExact := exactHits.contains i
    let isBroad := broadHits.contains i
    let color :=
      if isExact then
        Color.rgba 1.0 0.85 0.2 1.0
      else if isBroad then
        Color.rgba 0.95 0.55 0.2 0.95
      else
        Color.rgba 0.8 0.9 1.0 0.9
    match MathView3D.worldToScreen view center with
    | some pos2 =>
        setFillColor color
        fillPath (Afferent.Path.circle (Point.mk pos2.1 pos2.2) (7.0 * screenScale))
    | none => pure ()

  drawBox view queryBox (Color.rgba 0.9 0.8 0.2 0.9) (2.0 * screenScale)

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
  fillTextXY
    s!"camera: yaw {formatFloat state.cameraYaw}  pitch {formatFloat state.cameraPitch}  dist {formatFloat state.cameraDistance}"
    (20 * screenScale) (infoY + 60 * screenScale) fontSmall

  fillTextXY "OCTREE VIEWER 3D" (20 * screenScale) (30 * screenScale) fontMedium
  setFillColor (Color.gray 0.6)
  fillTextXY "Click: add box | X: remove | WASD/UJ move | +/- size | arrows rotate | [/] zoom | V toggle nodes"
    (20 * screenScale) (55 * screenScale) fontSmall
  fillTextXY "Legend: exact hits = yellow, broad-phase candidates = orange"
    (20 * screenScale) (75 * screenScale) fontSmall

/-- Create octree viewer widget. -/
def octreeViewer3DWidget (env : DemoEnv) (state : OctreeViewer3DState)
    : Afferent.Arbor.WidgetBuilder := do
  let config := octreeViewer3DMathViewConfig state env.screenScale
  MathView3D.mathView3D config env.fontSmall (fun view => do
    renderOctreeViewer3D state view env.screenScale env.fontMedium env.fontSmall
  )

end Demos.Linalg
