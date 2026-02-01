/-
  KD-Tree Nearest Neighbor Demo - visualize splits and search queries.
-/
import Afferent
import Afferent.Widget
import Afferent.Arbor
import Demos.Core.Demo
import Demos.Linalg.Shared
import Trellis
import Linalg.Core
import Linalg.Vec2
import Linalg.Geometry.AABB2D
import Linalg.Spatial.KDTree

open Afferent CanvasM Linalg

namespace Demos.Linalg

open Linalg.Spatial

/-- State for KD-tree nearest neighbor demo. -/
structure KDTreeNearestNeighborState where
  points : Array Vec2
  queryPoint : Vec2 := Vec2.zero
  radius : Float := 1.4
  kCount : Nat := 4
  maxLeaf : Nat := 6
  dragging : Bool := false
  deriving Inhabited

private def samplePoints : Array Vec2 :=
  (Array.range 32).map fun i =>
    let t := i.toFloat
    let x := Float.sin (t * 0.55) * 3.2 + Float.cos (t * 0.12) * 1.1
    let y := Float.cos (t * 0.42) * 2.7 + Float.sin (t * 0.18) * 1.0
    Vec2.mk x y

/-- Initial state. -/
def kdTreeNearestNeighborInitialState : KDTreeNearestNeighborState := {
  points := samplePoints
  queryPoint := Vec2.mk 0.5 0.4
}

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

private def drawBounds (b : AABB2D) (origin : Float × Float) (scale : Float)
    (color : Color) : CanvasM Unit := do
  let (sx1, sy1) := worldToScreen b.min origin scale
  let (sx2, sy2) := worldToScreen b.max origin scale
  let x := Float.min sx1 sx2
  let y := Float.min sy1 sy2
  let w := Float.abs' (sx2 - sx1)
  let h := Float.abs' (sy2 - sy1)
  setStrokeColor color
  setLineWidth 1.0
  strokePath (Afferent.Path.rectangleXYWH x y w h)

private def splitColor (depth : Nat) : Color :=
  let t := Float.min (depth.toFloat / 8.0) 1.0
  Color.rgba (0.3 + 0.4 * t) (0.7 - 0.3 * t) (0.9 - 0.5 * t) 0.6

private partial def drawKDNode (node : KDNode2D) (bounds : AABB2D)
    (origin : Float × Float) (scale : Float) (depth : Nat) : CanvasM Unit := do
  match node with
  | .leaf _ =>
      drawBounds bounds origin scale (Color.rgba 0.2 0.4 0.7 0.2)
  | .branch axis split left right =>
      let color := splitColor depth
      match axis with
      | ⟨0, _⟩ =>
          let start := Vec2.mk split bounds.min.y
          let finish := Vec2.mk split bounds.max.y
          drawLineWorld start finish origin scale color 1.4
          let leftBounds := AABB2D.fromMinMax bounds.min (Vec2.mk split bounds.max.y)
          let rightBounds := AABB2D.fromMinMax (Vec2.mk split bounds.min.y) bounds.max
          drawKDNode left leftBounds origin scale (depth + 1)
          drawKDNode right rightBounds origin scale (depth + 1)
      | ⟨1, _⟩ =>
          let start := Vec2.mk bounds.min.x split
          let finish := Vec2.mk bounds.max.x split
          drawLineWorld start finish origin scale color 1.4
          let leftBounds := AABB2D.fromMinMax bounds.min (Vec2.mk bounds.max.x split)
          let rightBounds := AABB2D.fromMinMax (Vec2.mk bounds.min.x split) bounds.max
          drawKDNode left leftBounds origin scale (depth + 1)
          drawKDNode right rightBounds origin scale (depth + 1)

private structure SplitVisit where
  bounds : AABB2D
  axis : Fin 2
  splitValue : Float
  visitedFar : Bool

private partial def nearestWithPath (node : KDNode2D) (bounds : AABB2D) (points : Array Vec2)
    (query : Vec2) (best : Option Nat × Float) : Option Nat × Float × Array SplitVisit :=
  match node with
  | .leaf indices =>
      let (bestIdx, bestDist) := indices.foldl (fun (idxOpt, dist) idx =>
        if h : idx < points.size then
          let pos := points[idx]
          let d := query.distanceSquared pos
          if d < dist then (some idx, d) else (idxOpt, dist)
        else (idxOpt, dist)
      ) best
      (bestIdx, bestDist, #[])
  | .branch axis split left right =>
      let queryValue := match axis with
        | ⟨0, _⟩ => query.x
        | ⟨1, _⟩ => query.y
      let (nearChild, farChild, nearBounds, farBounds) :=
        match axis with
        | ⟨0, _⟩ =>
            if queryValue < split then
              (left, right,
                AABB2D.fromMinMax bounds.min (Vec2.mk split bounds.max.y),
                AABB2D.fromMinMax (Vec2.mk split bounds.min.y) bounds.max)
            else
              (right, left,
                AABB2D.fromMinMax (Vec2.mk split bounds.min.y) bounds.max,
                AABB2D.fromMinMax bounds.min (Vec2.mk split bounds.max.y))
        | ⟨1, _⟩ =>
            if queryValue < split then
              (left, right,
                AABB2D.fromMinMax bounds.min (Vec2.mk bounds.max.x split),
                AABB2D.fromMinMax (Vec2.mk bounds.min.x split) bounds.max)
            else
              (right, left,
                AABB2D.fromMinMax (Vec2.mk bounds.min.x split) bounds.max,
                AABB2D.fromMinMax bounds.min (Vec2.mk bounds.max.x split))
      let (bestIdx1, bestDist1, visits1) := nearestWithPath nearChild nearBounds points query best
      let distToPlane := (queryValue - split) * (queryValue - split)
      let (bestIdx2, bestDist2, visits2, visitedFar) :=
        if distToPlane < bestDist1 then
          let (bIdx, bDist, v2) := nearestWithPath farChild farBounds points query (bestIdx1, bestDist1)
          (bIdx, bDist, v2, true)
        else
          (bestIdx1, bestDist1, #[], false)
      let visit : SplitVisit := { bounds := bounds, axis := axis, splitValue := split, visitedFar := visitedFar }
      (bestIdx2, bestDist2, (visits1 ++ visits2).push visit)

private def drawVisitedSplits (visits : Array SplitVisit) (origin : Float × Float) (scale : Float)
    : CanvasM Unit := do
  for visit in visits do
    let color := if visit.visitedFar then Color.rgba 1.0 0.6 0.2 0.8 else Color.rgba 0.3 0.9 0.5 0.8
    match visit.axis with
    | ⟨0, _⟩ =>
        let start := Vec2.mk visit.splitValue visit.bounds.min.y
        let finish := Vec2.mk visit.splitValue visit.bounds.max.y
        drawLineWorld start finish origin scale color 2.4
    | ⟨1, _⟩ =>
        let start := Vec2.mk visit.bounds.min.x visit.splitValue
        let finish := Vec2.mk visit.bounds.max.x visit.splitValue
        drawLineWorld start finish origin scale color 2.4

/-- Render KD-tree nearest neighbor demo. -/
def renderKDTreeNearestNeighbor (state : KDTreeNearestNeighborState)
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

  let tree := KDTree2D.build state.points state.maxLeaf
  drawKDNode tree.root tree.bounds origin scale 0

  let (bestIdx, bestDist, visits) := nearestWithPath tree.root tree.bounds state.points
    state.queryPoint (none, Float.infinity)
  drawVisitedSplits visits origin scale

  let radiusHits := KDTree2D.withinRadius tree state.points state.queryPoint state.radius
  let kHits := KDTree2D.kNearest tree state.points state.queryPoint state.kCount

  for i in [:state.points.size] do
    let p := state.points[i]!
    let inRadius := radiusHits.contains i
    let inK := kHits.contains i
    let color :=
      if inK then Color.rgba 0.2 0.9 0.5 1.0
      else if inRadius then Color.rgba 0.9 0.8 0.2 1.0
      else Color.white
    drawMarker p origin scale color 7.0

  match bestIdx with
  | some idx =>
      if h : idx < state.points.size then
        let p := state.points[idx]
        drawMarker p origin scale (Color.rgba 1.0 0.5 0.2 1.0) 10.0
        drawDashedLine (worldToScreen state.queryPoint origin scale) (worldToScreen p origin scale)
          (Color.rgba 1.0 0.5 0.2 0.8) 6.0 4.0 2.0
      else pure ()
  | none => pure ()

  drawMarker state.queryPoint origin scale (Color.rgba 0.4 0.9 1.0 1.0) 7.0
  let (cx, cy) := worldToScreen state.queryPoint origin scale
  setStrokeColor (Color.rgba 0.9 0.8 0.2 0.7)
  setLineWidth 1.8
  strokePath (Afferent.Path.circle (Point.mk cx cy) (state.radius * scale))

  let infoY := h - 140 * screenScale
  setFillColor VecColor.label
  fillTextXY s!"points: {state.points.size}  nearest dist²={formatFloat bestDist}"
    (20 * screenScale) infoY fontSmall
  fillTextXY s!"radius: {formatFloat state.radius}  k: {state.kCount}  leaf: {state.maxLeaf}"
    (20 * screenScale) (infoY + 20 * screenScale) fontSmall
  fillTextXY s!"query: {formatVec2 state.queryPoint}" (20 * screenScale) (infoY + 40 * screenScale) fontSmall

  fillTextXY "KD-TREE NEAREST NEIGHBOR" (20 * screenScale) (30 * screenScale) fontMedium
  setFillColor (Color.gray 0.6)
  fillTextXY "Click/drag: query | +/- radius | [ ] k | R reset" (20 * screenScale) (55 * screenScale)
    fontSmall

/-- Create KD-tree nearest neighbor widget. -/
def kdTreeNearestNeighborWidget (env : DemoEnv) (state : KDTreeNearestNeighborState)
    : Afferent.Arbor.WidgetBuilder := do
  Afferent.Arbor.custom (spec := {
    measure := fun _ _ => (0, 0)
    collect := fun _ => #[]
    draw := some (fun layout => do
      withContentRect layout fun w h => do
        resetTransform
        renderKDTreeNearestNeighbor state w h env.screenScale env.fontMedium env.fontSmall
    )
  }) (style := { flexItem := some (Trellis.FlexItem.growing 1) })

end Demos.Linalg
