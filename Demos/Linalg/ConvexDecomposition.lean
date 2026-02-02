/-
  Convex Decomposition Demo - visualize convex decomposition pieces and hulls.
-/
import Afferent
import Afferent.Widget
import Afferent.Arbor
import Demos.Core.Demo
import Demos.Linalg.Shared
import Demos.Linalg.RotationShared
import Trellis
import Linalg.Core
import Linalg.Vec3
import Linalg.Geometry.Mesh
import Linalg.Geometry.AABB
import Linalg.Geometry.ConvexHull3D
import Linalg.Geometry.ConvexDecomposition

open Afferent CanvasM Linalg

namespace Demos.Linalg

inductive ConvexMeshPreset where
  | cluster
  | stairs
  | bridge
  deriving BEq, Inhabited

def convexMeshPresetName : ConvexMeshPreset -> String
  | .cluster => "Cluster"
  | .stairs => "Stairs"
  | .bridge => "Bridge"

def nextConvexMeshPreset : ConvexMeshPreset -> ConvexMeshPreset
  | .cluster => .stairs
  | .stairs => .bridge
  | .bridge => .cluster

/-- State for convex decomposition demo. -/
structure ConvexDecompositionState where
  cameraYaw : Float := 0.6
  cameraPitch : Float := 0.35
  dragging : Bool := false
  lastMouseX : Float := 0.0
  lastMouseY : Float := 0.0
  showMesh : Bool := true
  showHulls : Bool := true
  showBounds : Bool := true
  showAxes : Bool := true
  showGrid : Bool := true
  meshPreset : ConvexMeshPreset := .cluster
  config : Linalg.ConvexDecompositionConfig := {
    maxTrianglesPerPart := 12
    maxDepth := 6
    minSplitExtent := 0.2
  }
  deriving Inhabited

def convexDecompositionInitialState : ConvexDecompositionState := {}

private def appendBox (verts : Array Vec3) (inds : Array Nat)
    (center extents : Vec3) : Array Vec3 × Array Nat := Id.run do
  let min := center.sub extents
  let max := center.add extents
  let base := verts.size
  let newVerts : Array Vec3 := #[
    Vec3.mk min.x min.y min.z,
    Vec3.mk max.x min.y min.z,
    Vec3.mk max.x max.y min.z,
    Vec3.mk min.x max.y min.z,
    Vec3.mk min.x min.y max.z,
    Vec3.mk max.x min.y max.z,
    Vec3.mk max.x max.y max.z,
    Vec3.mk min.x max.y max.z
  ]
  let boxIndices : Array Nat := #[
    0, 1, 2,  0, 2, 3,  -- back
    4, 6, 5,  4, 7, 6,  -- front
    1, 5, 6,  1, 6, 2,  -- right
    0, 3, 7,  0, 7, 4,  -- left
    3, 2, 6,  3, 6, 7,  -- top
    0, 4, 5,  0, 5, 1   -- bottom
  ]
  let mut outVerts := verts.append newVerts
  let mut outInds := inds
  for idx in boxIndices do
    outInds := outInds.push (base + idx)
  return (outVerts, outInds)

private def buildMeshForPreset (preset : ConvexMeshPreset) : Mesh := Id.run do
  let mut verts : Array Vec3 := #[]
  let mut inds : Array Nat := #[]
  match preset with
  | .cluster =>
      let (v0, i0) := appendBox verts inds (Vec3.mk 0.0 0.0 0.0) (Vec3.mk 0.9 0.3 0.6)
      verts := v0
      inds := i0
      let (v1, i1) := appendBox verts inds (Vec3.mk 1.2 0.2 (-0.4)) (Vec3.mk 0.4 0.5 0.4)
      verts := v1
      inds := i1
      let (v2, i2) := appendBox verts inds (Vec3.mk (-1.1) 0.25 0.8) (Vec3.mk 0.5 0.35 0.5)
      verts := v2
      inds := i2
      let (v3, i3) := appendBox verts inds (Vec3.mk 0.2 0.95 0.2) (Vec3.mk 0.3 0.5 0.3)
      verts := v3
      inds := i3
      let (v4, i4) := appendBox verts inds (Vec3.mk (-0.6) 0.55 (-1.0)) (Vec3.mk 0.45 0.35 0.45)
      verts := v4
      inds := i4
  | .stairs =>
      for i in [:6] do
        let t := i.toFloat
        let center := Vec3.mk (-1.7 + t * 0.7) (0.15 + t * 0.22) (Float.sin (t * 0.6) * 0.3)
        let extents := Vec3.mk 0.4 (0.18 + t * 0.03) 0.55
        let (v, i2) := appendBox verts inds center extents
        verts := v
        inds := i2
  | .bridge =>
      let (v0, i0) := appendBox verts inds (Vec3.mk (-1.3) 0.5 0.0) (Vec3.mk 0.4 0.9 0.4)
      verts := v0
      inds := i0
      let (v1, i1) := appendBox verts inds (Vec3.mk 1.3 0.5 0.0) (Vec3.mk 0.4 0.9 0.4)
      verts := v1
      inds := i1
      let (v2, i2) := appendBox verts inds (Vec3.mk 0.0 1.35 0.0) (Vec3.mk 1.6 0.22 0.4)
      verts := v2
      inds := i2
      let (v3, i3) := appendBox verts inds (Vec3.mk 0.0 0.2 0.9) (Vec3.mk 0.65 0.25 0.35)
      verts := v3
      inds := i3
      let (v4, i4) := appendBox verts inds (Vec3.mk 0.0 0.2 (-0.9)) (Vec3.mk 0.65 0.25 0.35)
      verts := v4
      inds := i4
  return Mesh.fromVerticesIndices verts inds

private def piecePalette : Array Color := #[
  Color.rgba 0.2 0.8 1.0 0.9,
  Color.rgba 0.9 0.6 0.2 0.9,
  Color.rgba 0.4 0.9 0.4 0.9,
  Color.rgba 0.9 0.35 0.4 0.9,
  Color.rgba 0.7 0.6 1.0 0.9,
  Color.rgba 0.3 0.9 0.7 0.9,
  Color.rgba 0.9 0.8 0.3 0.9,
  Color.rgba 0.5 0.85 0.95 0.9
]

private def pieceColor (idx : Nat) : Color :=
  piecePalette[idx % piecePalette.size]!

private def drawLine3D (a b : Vec3) (yaw pitch : Float) (origin : Float × Float)
    (scale : Float) (color : Color) (lineWidth : Float := 1.6) : CanvasM Unit := do
  let p1 := rotProject3Dto2D a yaw pitch origin scale
  let p2 := rotProject3Dto2D b yaw pitch origin scale
  setStrokeColor color
  setLineWidth lineWidth
  let path := Afferent.Path.empty
    |>.moveTo (Point.mk p1.1 p1.2)
    |>.lineTo (Point.mk p2.1 p2.2)
  strokePath path

private def drawAABBWireframe (aabb : AABB) (yaw pitch : Float) (origin : Float × Float)
    (scale : Float) (color : Color) (lineWidth : Float := 1.4) : CanvasM Unit := do
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
      yaw pitch origin scale color lineWidth

private def collectHullEdges (faces : Array (Nat × Nat × Nat)) : Array (Nat × Nat) := Id.run do
  let mut edges : Array (Nat × Nat) := #[]
  let mut keys : Array (Nat × Nat) := #[]
  for (a, b, c) in faces do
    let key1 := if a <= b then (a, b) else (b, a)
    if !keys.any (fun k => k == key1) then
      keys := keys.push key1
      edges := edges.push (a, b)
    let key2 := if b <= c then (b, c) else (c, b)
    if !keys.any (fun k => k == key2) then
      keys := keys.push key2
      edges := edges.push (b, c)
    let key3 := if c <= a then (c, a) else (a, c)
    if !keys.any (fun k => k == key3) then
      keys := keys.push key3
      edges := edges.push (c, a)
  return edges

private def drawHullWireframe (hull : ConvexHull3D) (yaw pitch : Float)
    (origin : Float × Float) (scale : Float) (color : Color) : CanvasM Unit := do
  if hull.faces.isEmpty then return
  let edges := collectHullEdges hull.faces
  for (a, b) in edges do
    let p1 := hull.points.getD a Vec3.zero
    let p2 := hull.points.getD b Vec3.zero
    drawLine3D p1 p2 yaw pitch origin scale color 1.8

private def drawMeshWireframe (mesh : Mesh) (yaw pitch : Float)
    (origin : Float × Float) (scale : Float) (color : Color) : CanvasM Unit := do
  let triCount := mesh.triangleCount
  for i in [:triCount] do
    match Mesh.triangle? mesh i with
    | none => pure ()
    | some tri =>
        drawLine3D tri.v0 tri.v1 yaw pitch origin scale color 1.0
        drawLine3D tri.v1 tri.v2 yaw pitch origin scale color 1.0
        drawLine3D tri.v2 tri.v0 yaw pitch origin scale color 1.0

private def drawGroundGrid (yaw pitch : Float) (origin : Float × Float)
    (scale : Float) (screenScale : Float) : CanvasM Unit := do
  setStrokeColor (Color.gray 0.18)
  setLineWidth (1.0 * screenScale)
  for i in [:13] do
    let offset := (i.toFloat - 6.0) * 0.6
    let p1 := rotProject3Dto2D (Vec3.mk (-3.6) 0 offset) yaw pitch origin scale
    let p2 := rotProject3Dto2D (Vec3.mk 3.6 0 offset) yaw pitch origin scale
    let path := Afferent.Path.empty
      |>.moveTo (Point.mk p1.1 p1.2)
      |>.lineTo (Point.mk p2.1 p2.2)
    strokePath path
    let p3 := rotProject3Dto2D (Vec3.mk offset 0 (-3.6)) yaw pitch origin scale
    let p4 := rotProject3Dto2D (Vec3.mk offset 0 3.6) yaw pitch origin scale
    let path2 := Afferent.Path.empty
      |>.moveTo (Point.mk p3.1 p3.2)
      |>.lineTo (Point.mk p4.1 p4.2)
    strokePath path2

/-- Render convex decomposition demo. -/
def renderConvexDecomposition (state : ConvexDecompositionState)
    (w h : Float) (screenScale : Float) (fontMedium fontSmall : Font) : CanvasM Unit := do
  let origin : Float × Float := (w / 2, h / 2)
  let scale : Float := 70.0 * screenScale
  let mesh := buildMeshForPreset state.meshPreset
  let pieces := Linalg.ConvexDecomposition.decompose mesh state.config

  if state.showGrid then
    drawGroundGrid state.cameraYaw state.cameraPitch origin scale screenScale
  if state.showAxes then
    rotDraw3DAxes state.cameraYaw state.cameraPitch origin scale 2.8 fontSmall

  if state.showMesh then
    drawMeshWireframe mesh state.cameraYaw state.cameraPitch origin scale (Color.gray 0.35)

  for i in [:pieces.size] do
    let piece := pieces[i]!
    let color := pieceColor i
    if state.showBounds then
      drawAABBWireframe piece.bounds state.cameraYaw state.cameraPitch origin scale (color.withAlpha 0.35) 1.3
    if state.showHulls then
      drawHullWireframe piece.hull state.cameraYaw state.cameraPitch origin scale color

  let infoY := h - 160 * screenScale
  let maxTriText := if state.config.maxTrianglesPerPart == 0 then "none"
    else s!"{state.config.maxTrianglesPerPart}"
  setFillColor VecColor.label
  fillTextXY
    s!"mesh: {convexMeshPresetName state.meshPreset}  triangles: {mesh.triangleCount}  pieces: {pieces.size}"
    (20 * screenScale) infoY fontSmall
  fillTextXY
    s!"max tris/part: {maxTriText}  max depth: {state.config.maxDepth}  min split: {formatFloat state.config.minSplitExtent}"
    (20 * screenScale) (infoY + 20 * screenScale) fontSmall
  fillTextXY
    s!"hulls: {state.showHulls}  bounds: {state.showBounds}  mesh: {state.showMesh}"
    (20 * screenScale) (infoY + 40 * screenScale) fontSmall

  fillTextXY "CONVEX DECOMPOSITION" (20 * screenScale) (30 * screenScale) fontMedium
  setFillColor (Color.gray 0.6)
  fillTextXY
    "Drag: rotate | H: hulls | B: bounds | M: mesh | G: grid | C: preset"
    (20 * screenScale) (55 * screenScale) fontSmall
  fillTextXY
    "[ ]: tris/part | , .: depth | +/-: split extent | X: axes | R: reset"
    (20 * screenScale) (75 * screenScale) fontSmall

/-- Create the convex decomposition widget. -/
def convexDecompositionWidget (env : DemoEnv) (state : ConvexDecompositionState)
    : Afferent.Arbor.WidgetBuilder := do
  Afferent.Arbor.custom (spec := {
    measure := fun _ _ => (0, 0)
    collect := fun _ => #[]
    draw := some (fun layout => do
      withContentRect layout fun w h => do
        resetTransform
        renderConvexDecomposition state w h env.screenScale env.fontMedium env.fontSmall
    )
  }) (style := { flexItem := some (Trellis.FlexItem.growing 1) })

end Demos.Linalg
