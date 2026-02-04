/-
  Convex Decomposition Demo - visualize convex decomposition pieces and hulls.
-/
import Afferent
import Afferent.Widget
import Afferent.Arbor
import Demos.Core.Demo
import Demos.Linalg.Shared
import Trellis
import Linalg.Core
import Linalg.Vec3
import Linalg.Mat4
import Linalg.Geometry.Mesh
import Linalg.Geometry.AABB
import Linalg.Geometry.Triangle
import Linalg.Geometry.ConvexHull3D
import Linalg.Geometry.ConvexDecomposition
import Linalg.Geometry.Ray
import Linalg.Geometry.Intersection
import AfferentMath.Widget.MathView3D

open Afferent CanvasM Linalg
open Afferent.Widget
open AfferentMath.Widget

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
  showSamples : Bool := true
  showVoxels : Bool := false
  showConcavity : Bool := true
  voxelResolution : Nat := 10
  concavityThreshold : Float := 0.03
  meshPreset : ConvexMeshPreset := .cluster
  config : Linalg.ConvexDecompositionConfig := {
    maxTrianglesPerPart := 12
    maxDepth := 6
    minSplitExtent := 0.2
  }
  deriving Inhabited

def convexDecompositionInitialState : ConvexDecompositionState := {}

def convexDecompositionMathViewConfig (state : ConvexDecompositionState) (screenScale : Float)
    : MathView3D.Config := {
  style := { flexItem := some (Trellis.FlexItem.growing 1) }
  camera := { yaw := state.cameraYaw, pitch := state.cameraPitch, distance := 7.0 }
  fov := 60.0 * Float.pi / 180.0
  showGrid := false
  showAxes := false
  axisLineWidth := 2.0 * screenScale
}
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

private def lerpColor (c1 c2 : Color) (t : Float) : Color :=
  let t' := Float.max 0.0 (Float.min 1.0 t)
  Color.rgba
    (c1.r + (c2.r - c1.r) * t')
    (c1.g + (c2.g - c1.g) * t')
    (c1.b + (c2.b - c1.b) * t')
    (c1.a + (c2.a - c1.a) * t')

private def concavityColor (value maxValue : Float) : Color :=
  let t := if maxValue > 0.0001 then value / maxValue else 0.0
  let t' := Float.max 0.0 (Float.min 1.0 t)
  if t' < 0.25 then
    lerpColor (Color.rgba 0.15 0.35 0.8 0.9) (Color.rgba 0.1 0.75 0.9 0.9) (t' * 4.0)
  else if t' < 0.5 then
    lerpColor (Color.rgba 0.1 0.75 0.9 0.9) (Color.rgba 0.2 0.9 0.45 0.9) ((t' - 0.25) * 4.0)
  else if t' < 0.75 then
    lerpColor (Color.rgba 0.2 0.9 0.45 0.9) (Color.rgba 0.95 0.85 0.2 0.9) ((t' - 0.5) * 4.0)
  else
    lerpColor (Color.rgba 0.95 0.85 0.2 0.9) (Color.rgba 0.95 0.3 0.2 0.95) ((t' - 0.75) * 4.0)

private structure ConcavitySample where
  pos : Vec3
  concavity : Float

private structure VoxelSample where
  pos : Vec3
  hullDist : Float
  insideMesh : Bool

private abbrev ProjectionCtx := MathView3D.View

private def makeProjectionCtx (state : ConvexDecompositionState) (screenScale : Float)
    (w h : Float) : ProjectionCtx :=
  let config := convexDecompositionMathViewConfig state screenScale
  MathView3D.viewForSize config w h

private def projectPoint (ctx : ProjectionCtx) (p : Vec3) : Float × Float :=
  MathView3D.worldToScreen ctx p |>.getD (0.0, 0.0)

private def drawLine3D (a b : Vec3) (ctx : ProjectionCtx)
    (color : Color) (lineWidth : Float := 1.6) : CanvasM Unit := do
  MathView3D.drawLine3D ctx a b color lineWidth

private def drawAABBWireframe (aabb : AABB) (ctx : ProjectionCtx)
    (color : Color) (lineWidth : Float := 1.4) : CanvasM Unit := do
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
    drawLine3D (corners.getD i Vec3.zero) (corners.getD j Vec3.zero) ctx color lineWidth

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

private def drawHullWireframe (hull : ConvexHull3D) (ctx : ProjectionCtx)
    (color : Color) : CanvasM Unit := do
  if hull.faces.isEmpty then return
  let edges := collectHullEdges hull.faces
  for (a, b) in edges do
    let p1 := hull.points.getD a Vec3.zero
    let p2 := hull.points.getD b Vec3.zero
    drawLine3D p1 p2 ctx color 1.8

private def drawMeshWireframe (mesh : Mesh) (ctx : ProjectionCtx)
    (color : Color) : CanvasM Unit := do
  let triCount := mesh.triangleCount
  for i in [:triCount] do
    match Mesh.triangle? mesh i with
    | none => pure ()
    | some tri =>
        drawLine3D tri.v0 tri.v1 ctx color 1.0
        drawLine3D tri.v1 tri.v2 ctx color 1.0
        drawLine3D tri.v2 tri.v0 ctx color 1.0

private def drawPoint3D (p : Vec3) (ctx : ProjectionCtx)
    (color : Color) (radius : Float := 3.0) : CanvasM Unit := do
  match MathView3D.worldToScreen ctx p with
  | some (sx, sy) =>
      setFillColor color
      fillPath (Afferent.Path.circle (Point.mk sx sy) radius)
  | none => pure ()

private def hullCentroid (hull : ConvexHull3D) : Vec3 :=
  if hull.points.isEmpty then
    Vec3.zero
  else
    let sum := hull.points.foldl (fun acc p => acc.add p) Vec3.zero
    sum.scale (1.0 / hull.points.size.toFloat)

private def buildHullPlanes (hull : ConvexHull3D) : Array (Vec3 × Float) := Id.run do
  if hull.faces.isEmpty || hull.points.isEmpty then
    return #[]
  let center := hullCentroid hull
  let mut planes : Array (Vec3 × Float) := #[]
  for (a, b, c) in hull.faces do
    let p0 := hull.points.getD a Vec3.zero
    let p1 := hull.points.getD b Vec3.zero
    let p2 := hull.points.getD c Vec3.zero
    let mut normal := (p1 - p0).cross (p2 - p0)
    if normal.lengthSquared < 1e-8 then
      continue
    normal := normal.normalize
    if normal.dot (center - p0) > 0.0 then
      normal := normal.neg
    let offset := normal.dot p0
    planes := planes.push (normal, offset)
  return planes

private def hullSignedDistance (planes : Array (Vec3 × Float)) (p : Vec3) : Float := Id.run do
  if planes.isEmpty then
    return 0.0
  let mut maxDist := (-1.0e9)
  for (normal, offset) in planes do
    let dist := normal.dot p - offset
    if dist > maxDist then
      maxDist := dist
  return maxDist

private def concavityDistance (planes : Array (Vec3 × Float)) (p : Vec3) : Float :=
  let maxDist := hullSignedDistance planes p
  if maxDist < 0.0 then -maxDist else 0.0

private def sampleTriangleCentroids (mesh : Mesh) : Array Vec3 := Id.run do
  let mut points : Array Vec3 := #[]
  let triCount := mesh.triangleCount
  for i in [:triCount] do
    match Mesh.triangle? mesh i with
    | none => ()
    | some tri => points := points.push tri.centroid
  return points

private def buildConcavitySamples (planes : Array (Vec3 × Float)) (points : Array Vec3)
    : Array ConcavitySample := Id.run do
  let mut samples : Array ConcavitySample := #[]
  for p in points do
    samples := samples.push { pos := p, concavity := concavityDistance planes p }
  return samples

private def sampleVoxelCenters (bounds : AABB) (resolution : Nat) : Array Vec3 := Id.run do
  if resolution == 0 then
    return #[]
  let res := Nat.max 2 resolution
  let size := bounds.size
  let step := Vec3.mk (size.x / res.toFloat) (size.y / res.toFloat) (size.z / res.toFloat)
  let mut points : Array Vec3 := #[]
  for ix in [:res] do
    for iy in [:res] do
      for iz in [:res] do
        let p := Vec3.mk
          (bounds.min.x + (ix.toFloat + 0.5) * step.x)
          (bounds.min.y + (iy.toFloat + 0.5) * step.y)
          (bounds.min.z + (iz.toFloat + 0.5) * step.z)
        points := points.push p
  return points

private def meshTriangles (mesh : Mesh) : Array Triangle :=
  match Mesh.triangles mesh with
  | none => #[]
  | some tris => tris

private def pointInsideMesh (tris : Array Triangle) (p : Vec3) : Bool := Id.run do
  if tris.isEmpty then
    return false
  let jitter := Vec3.mk 0.0003 0.0001 0.0002
  let ray := Ray.mk' (p.add jitter) Vec3.unitX
  let mut hits : Nat := 0
  for tri in tris do
    match Intersection.rayTriangle ray tri with
    | some hit =>
        if hit.t > 1.0e-4 then
          hits := hits + 1
    | none => ()
  return hits % 2 == 1

private def buildVoxelSamples (planes : Array (Vec3 × Float)) (tris : Array Triangle)
    (points : Array Vec3) : Array VoxelSample := Id.run do
  let mut samples : Array VoxelSample := #[]
  for p in points do
    let dist := hullSignedDistance planes p
    if dist <= 0.0 then
      let inside := pointInsideMesh tris p
      samples := samples.push { pos := p, hullDist := dist, insideMesh := inside }
  return samples

private def drawGroundGrid (ctx : ProjectionCtx) (screenScale : Float) : CanvasM Unit := do
  setStrokeColor (Color.gray 0.18)
  setLineWidth (1.0 * screenScale)
  for i in [:13] do
    let offset := (i.toFloat - 6.0) * 0.6
    let p1 := projectPoint ctx (Vec3.mk (-3.6) 0 offset)
    let p2 := projectPoint ctx (Vec3.mk 3.6 0 offset)
    let path := Afferent.Path.empty
      |>.moveTo (Point.mk p1.1 p1.2)
      |>.lineTo (Point.mk p2.1 p2.2)
    strokePath path
    let p3 := projectPoint ctx (Vec3.mk offset 0 (-3.6))
    let p4 := projectPoint ctx (Vec3.mk offset 0 3.6)
    let path2 := Afferent.Path.empty
      |>.moveTo (Point.mk p3.1 p3.2)
      |>.lineTo (Point.mk p4.1 p4.2)
    strokePath path2

private def drawAxesPerspective (ctx : ProjectionCtx) (axisLength : Float)
    (fontSmall : Font) : CanvasM Unit := do
  let xEnd := Vec3.mk axisLength 0.0 0.0
  let yEnd := Vec3.mk 0.0 axisLength 0.0
  let zEnd := Vec3.mk 0.0 0.0 axisLength

  drawLine3D Vec3.zero xEnd ctx VecColor.xAxis 2.0
  drawLine3D Vec3.zero yEnd ctx VecColor.yAxis 2.0
  drawLine3D Vec3.zero zEnd ctx VecColor.zAxis 2.0

  let (xx, xy) := projectPoint ctx xEnd
  let (yx, yy) := projectPoint ctx yEnd
  let (zx, zy) := projectPoint ctx zEnd

  setFillColor VecColor.xAxis
  fillTextXY "X" (xx + 6) (xy - 6) fontSmall
  setFillColor VecColor.yAxis
  fillTextXY "Y" (yx + 6) (yy - 6) fontSmall
  setFillColor VecColor.zAxis
  fillTextXY "Z" (zx + 6) (zy - 6) fontSmall

/-- Render convex decomposition demo. -/
def renderConvexDecomposition (state : ConvexDecompositionState)
    (view : MathView3D.View) (screenScale : Float) (fontMedium fontSmall : Font) : CanvasM Unit := do
  let w := view.width
  let h := view.height
  let projCtx := makeProjectionCtx state screenScale w h
  let mesh := buildMeshForPreset state.meshPreset
  let hull := ConvexHull3D.quickHull mesh.vertices
  let planes := buildHullPlanes hull
  let triSamples :=
    if state.showSamples then
      buildConcavitySamples planes (sampleTriangleCentroids mesh)
    else
      #[]
  let tris := if state.showVoxels then meshTriangles mesh else #[]
  let voxelSamples :=
    if state.showVoxels then
      buildVoxelSamples planes tris (sampleVoxelCenters mesh.bounds state.voxelResolution)
    else
      #[]
  let pieces := Linalg.ConvexDecomposition.decompose mesh state.config
  let mut maxConcavity : Float := 0.001
  if state.showConcavity then
    for sample in triSamples do
      if sample.concavity > maxConcavity then
        maxConcavity := sample.concavity
    for sample in voxelSamples do
      if !sample.insideMesh then
        let dist := -sample.hullDist
        if dist > maxConcavity then
          maxConcavity := dist

  if state.showGrid then
    drawGroundGrid projCtx screenScale
  if state.showAxes then
    drawAxesPerspective projCtx 2.8 fontSmall

  if state.showMesh then
    drawMeshWireframe mesh projCtx (Color.gray 0.35)

  if state.showVoxels then
    for sample in voxelSamples do
      if sample.insideMesh then
        drawPoint3D sample.pos projCtx (Color.rgba 0.3 0.3 0.35 0.35) (2.0 * screenScale)
      else
        let dist := -sample.hullDist
        if dist >= state.concavityThreshold then
          let color := if state.showConcavity then concavityColor dist maxConcavity
            else Color.rgba 0.9 0.55 0.2 0.7
          drawPoint3D sample.pos projCtx color (2.4 * screenScale)

  for i in [:pieces.size] do
    let piece := pieces[i]!
    let color := pieceColor i
    if state.showBounds then
      drawAABBWireframe piece.bounds projCtx (color.withAlpha 0.35) 1.3
    if state.showHulls then
      drawHullWireframe piece.hull projCtx color

  if state.showSamples then
    for sample in triSamples do
      if sample.concavity >= state.concavityThreshold then
        let color := if state.showConcavity then concavityColor sample.concavity maxConcavity
          else Color.rgba 0.85 0.85 0.85 0.75
        drawPoint3D sample.pos projCtx color (3.0 * screenScale)

  let infoY := h - 210 * screenScale
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
    s!"hulls: {state.showHulls}  bounds: {state.showBounds}  mesh: {state.showMesh}  grid: {state.showGrid}"
    (20 * screenScale) (infoY + 40 * screenScale) fontSmall
  fillTextXY
    s!"samples: {state.showSamples} ({triSamples.size})  voxels: {state.showVoxels} ({voxelSamples.size})  concavity: {state.showConcavity}"
    (20 * screenScale) (infoY + 60 * screenScale) fontSmall
  fillTextXY
    s!"voxel res: {state.voxelResolution}  threshold: {formatFloat state.concavityThreshold}  max concavity: {formatFloat maxConcavity}"
    (20 * screenScale) (infoY + 80 * screenScale) fontSmall

  fillTextXY "CONVEX DECOMPOSITION" (20 * screenScale) (30 * screenScale) fontMedium
  setFillColor (Color.gray 0.6)
  fillTextXY
    "Drag: rotate | H: hulls | B: bounds | M: mesh | G: grid | C: preset"
    (20 * screenScale) (55 * screenScale) fontSmall
  fillTextXY
    "[ ]: tris/part | , .: depth | +/-: split extent | X: axes | R: reset"
    (20 * screenScale) (75 * screenScale) fontSmall
  fillTextXY
    "P: samples | V: voxels | T: concavity | U/J: voxel res | I/K: threshold"
    (20 * screenScale) (95 * screenScale) fontSmall

/-- Create the convex decomposition widget. -/
def convexDecompositionWidget (env : DemoEnv) (state : ConvexDecompositionState)
    : Afferent.Arbor.WidgetBuilder := do
  let config := convexDecompositionMathViewConfig state env.screenScale
  MathView3D.mathView3D config env.fontSmall (fun view => do
    renderConvexDecomposition state view env.screenScale env.fontMedium env.fontSmall
  )

end Demos.Linalg
