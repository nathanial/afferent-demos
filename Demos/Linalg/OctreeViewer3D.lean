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

private structure CameraFrame where
  pos : Vec3
  right : Vec3
  up : Vec3
  forward : Vec3
  fov : Float
  near : Float
deriving Repr

private def cameraPosition (state : OctreeViewer3DState) : Vec3 :=
  let cy := Float.cos state.cameraYaw
  let sy := Float.sin state.cameraYaw
  let cp := Float.cos state.cameraPitch
  let sp := Float.sin state.cameraPitch
  let x := state.cameraDistance * sy * cp
  let y := state.cameraDistance * sp
  let z := state.cameraDistance * cy * cp
  state.cameraTarget.add (Vec3.mk x y z)

private def cameraFrame (state : OctreeViewer3DState) : CameraFrame :=
  let pos := cameraPosition state
  let forward := (state.cameraTarget.sub pos).normalize
  let rightRaw := Vec3.cross Vec3.unitY forward
  let right :=
    if rightRaw.lengthSquared < 0.000001 then Vec3.unitX else rightRaw.normalize
  let up := (Vec3.cross forward right).normalize
  {
    pos
    right
    up
    forward
    fov := state.cameraFov
    near := state.cameraNear
  }

private def projectToScreen (frame : CameraFrame) (aspect : Float)
    (origin : Float × Float) (halfW halfH : Float) (p : Vec3) : Option (Float × Float) :=
  let rel := p.sub frame.pos
  let xCam := rel.dot frame.right
  let yCam := rel.dot frame.up
  let zCam := rel.dot frame.forward
  if zCam <= frame.near then none
  else
    let f := 1.0 / Float.tan (frame.fov * 0.5)
    let xNdc := (xCam / zCam) * (f / aspect)
    let yNdc := (yCam / zCam) * f
    let (ox, oy) := origin
    let sx := ox + xNdc * halfW
    let sy := oy - yNdc * halfH
    some (sx, sy)

private def drawLineScreen (start finish : Float × Float) (color : Color)
    (lineWidth : Float := 1.4) : CanvasM Unit := do
  let (sx, sy) := start
  let (ex, ey) := finish
  setStrokeColor color
  setLineWidth lineWidth
  let path := Afferent.Path.empty
    |>.moveTo (Point.mk sx sy)
    |>.lineTo (Point.mk ex ey)
  strokePath path

private def drawLine3D (frame : CameraFrame) (aspect : Float)
    (origin : Float × Float) (halfW halfH : Float) (a b : Vec3)
    (color : Color) (lineWidth : Float := 1.2) : CanvasM Unit := do
  match projectToScreen frame aspect origin halfW halfH a,
        projectToScreen frame aspect origin halfW halfH b with
  | some p1, some p2 => drawLineScreen p1 p2 color lineWidth
  | _, _ => pure ()

private def drawMarkerScreen (pos : Float × Float) (color : Color) (radius : Float) : CanvasM Unit := do
  let (sx, sy) := pos
  setFillColor color
  fillPath (Afferent.Path.circle (Point.mk sx sy) radius)

private def drawBox (frame : CameraFrame) (aspect : Float)
    (origin : Float × Float) (halfW halfH : Float) (b : AABB)
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
    drawLine3D frame aspect origin halfW halfH corners[a]! corners[b]! color lineWidth

private def drawGroundGrid (frame : CameraFrame) (aspect : Float)
    (origin : Float × Float) (halfW halfH : Float) (screenScale : Float) : CanvasM Unit := do
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
    drawLine3D frame aspect origin halfW halfH a1 b1 color width
    drawLine3D frame aspect origin halfW halfH a2 b2 color width

def screenToWorldOnPlane (state : OctreeViewer3DState)
    (x y w h planeZ : Float) : Option Vec3 :=
  let frame := cameraFrame state
  let aspect := if h > 0.0 then w / h else 1.0
  let halfW := w / 2
  let halfH := h / 2
  let ndcX := if halfW == 0.0 then 0.0 else (x - halfW) / halfW
  let ndcY := if halfH == 0.0 then 0.0 else -((y - halfH) / halfH)
  let f := 1.0 / Float.tan (frame.fov * 0.5)
  let dirCam := Vec3.mk (ndcX * aspect / f) (ndcY / f) 1.0
  let dirWorld :=
    (frame.right.scale dirCam.x).add (frame.up.scale dirCam.y)
      |>.add (frame.forward.scale dirCam.z)
      |>.normalize
  if Float.abs' dirWorld.z < Float.epsilon then none
  else
    let t := (planeZ - frame.pos.z) / dirWorld.z
    if t <= 0.0 then none
    else some (frame.pos.add (dirWorld.scale t))

private def nodeColor (depth : Nat) : Color :=
  let t := Float.min (depth.toFloat / 6.0) 1.0
  Color.rgba (0.2 + 0.4 * t) (0.7 - 0.3 * t) (0.9 - 0.5 * t) 0.6

private partial def drawOctreeNode (node : OctreeNode) (frame : CameraFrame) (aspect : Float)
    (origin : Float × Float) (halfW halfH : Float) (depth : Nat) : CanvasM Unit := do
  let bounds := match node with
    | .internal b _ => b
    | .leaf b _ => b
  drawBox frame aspect origin halfW halfH bounds (nodeColor depth) 1.1
  match node with
  | .leaf _ _ => pure ()
  | .internal _ children =>
      for child in children do
        match child with
        | some c => drawOctreeNode c frame aspect origin halfW halfH (depth + 1)
        | none => pure ()

private def buildOctree (items : Array AABB) (config : TreeConfig) : Octree :=
  Octree.build items config

/-- Render octree viewer. -/
def renderOctreeViewer3D (state : OctreeViewer3DState)
    (w h : Float) (screenScale : Float) (fontMedium fontSmall : Font) : CanvasM Unit := do
  let origin : Float × Float := (w / 2, h / 2)
  let halfW := w / 2
  let halfH := h / 2
  let aspect := if h > 0.0 then w / h else 1.0
  let frame := cameraFrame state

  drawGroundGrid frame aspect origin halfW halfH screenScale

  let tree := buildOctree state.items state.config
  if state.showNodes then
    drawOctreeNode tree.root frame aspect origin halfW halfH 0

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
    match projectToScreen frame aspect origin halfW halfH center with
    | some pos2 => drawMarkerScreen pos2 color (7.0 * screenScale)
    | none => pure ()

  drawBox frame aspect origin halfW halfH queryBox (Color.rgba 0.9 0.8 0.2 0.9) (2.0 * screenScale)

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
