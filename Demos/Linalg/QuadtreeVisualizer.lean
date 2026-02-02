/-
  Quadtree Visualizer - 2D spatial queries with quadtree overlay.
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
import Linalg.Spatial.Quadtree

open Afferent CanvasM Linalg

namespace Demos.Linalg

open Linalg.Spatial

/-- Query modes for quadtree visualization. -/
inductive QuadtreeQueryMode where
  | rect
  | circle
  deriving BEq, Inhabited

/-- State for the quadtree visualizer. -/
structure QuadtreeVisualizerState where
  points : Array Vec2
  queryMode : QuadtreeQueryMode := .rect
  queryCenter : Vec2 := Vec2.zero
  queryExtents : Vec2 := Vec2.mk 1.6 1.1
  queryRadius : Float := 1.6
  config : TreeConfig := TreeConfig.dense
  kCount : Nat := 4
  showNearest : Bool := true
  hoverPos : Option Vec2 := none
  deriving Inhabited

private def samplePoints : Array Vec2 :=
  (Array.range 26).map fun i =>
    let t := i.toFloat
    let x := Float.sin (t * 0.6) * 3.6 + Float.cos (t * 0.15) * 1.2
    let y := Float.cos (t * 0.45) * 2.8 + Float.sin (t * 0.25) * 0.9
    Vec2.mk x y

/-- Initial state. -/
def quadtreeVisualizerInitialState : QuadtreeVisualizerState := {
  points := samplePoints
  queryCenter := Vec2.mk 0.5 (-0.2)
}

private def drawRectWorld (b : AABB2D) (origin : Float × Float) (scale : Float)
    (color : Color) (lineWidth : Float := 1.2) : CanvasM Unit := do
  let (sx1, sy1) := worldToScreen b.min origin scale
  let (sx2, sy2) := worldToScreen b.max origin scale
  let x := Float.min sx1 sx2
  let y := Float.min sy1 sy2
  let w := Float.abs' (sx2 - sx1)
  let h := Float.abs' (sy2 - sy1)
  setStrokeColor color
  setLineWidth lineWidth
  strokePath (Afferent.Path.rectangleXYWH x y w h)

private def quadtreeColor (depth : Nat) : Color :=
  let t := Float.min (depth.toFloat / 8.0) 1.0
  Color.rgba (0.2 + 0.3 * t) (0.6 - 0.2 * t) (0.9 - 0.3 * t) 0.6

private partial def drawQuadtreeNode (node : QuadtreeNode) (origin : Float × Float)
    (scale : Float) (depth : Nat) : CanvasM Unit := do
  let bounds := match node with
    | .internal b _ => b
    | .leaf b _ => b
  drawRectWorld bounds origin scale (quadtreeColor depth)
  match node with
  | .leaf _ _ => pure ()
  | .internal _ children =>
      for child in children do
        match child with
        | some c => drawQuadtreeNode c origin scale (depth + 1)
        | none => pure ()

private def buildQuadtree (points : Array Vec2) (config : TreeConfig) : Quadtree :=
  let built := Quadtree.build points config
  let inserted := points.foldl (fun (tree, idx) p =>
    (Quadtree.insert tree idx p, idx + 1)
  ) (Quadtree.empty built.bounds config, 0) |>.1
  inserted

/-- Render quadtree visualizer. -/
def renderQuadtreeVisualizer (state : QuadtreeVisualizerState)
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

  let tree := buildQuadtree state.points state.config
  drawQuadtreeNode tree.root origin scale 0

  let queryRect := AABB2D.fromCenterExtents state.queryCenter state.queryExtents
  let (broadHits, exactHits) :=
    if state.queryMode == .rect then
      let broad := tree.queryRect queryRect
      let exact := broad.filter fun idx =>
        if h : idx < state.points.size then
          queryRect.containsPoint state.points[idx]!
        else false
      (broad, exact)
    else
      let broad := tree.queryCircle state.queryCenter state.queryRadius
      let radiusSq := state.queryRadius * state.queryRadius
      let exact := broad.filter fun idx =>
        if h : idx < state.points.size then
          (state.points[idx]!).distanceSquared state.queryCenter <= radiusSq
        else false
      (broad, exact)

  for i in [:state.points.size] do
    let p := state.points[i]!
    let isExact := exactHits.contains i
    let isBroad := broadHits.contains i
    let color :=
      if isExact then
        Color.rgba 1.0 0.85 0.2 1.0
      else if isBroad then
        Color.rgba 0.95 0.55 0.2 0.95
      else
        Color.white
    drawMarker p origin scale color 7.0

  if state.queryMode == .rect then
    drawRectWorld queryRect origin scale (Color.rgba 0.9 0.8 0.2 0.9) 2.0
  else
    let (cx, cy) := worldToScreen state.queryCenter origin scale
    setStrokeColor (Color.rgba 0.9 0.8 0.2 0.9)
    setLineWidth 2.0
    strokePath (Afferent.Path.circle (Point.mk cx cy) (state.queryRadius * scale))

  if state.showNearest then
    match state.hoverPos with
    | some pos =>
        let nearest := tree.kNearest state.points pos state.kCount
        for idx in nearest do
          if h : idx < state.points.size then
            let p := state.points[idx]
            drawMarker p origin scale (Color.rgba 0.3 0.9 0.6 1.0) 9.0
          else pure ()
        drawMarker pos origin scale (Color.rgba 0.4 0.9 1.0 1.0) 6.0
    | none => pure ()

  let infoY := h - 150 * screenScale
  setFillColor VecColor.label
  fillTextXY
    s!"points: {state.points.size}  hits: {exactHits.size}  candidates: {broadHits.size}"
    (20 * screenScale) infoY fontSmall
  fillTextXY s!"config: depth {state.config.maxDepth}, leaf {state.config.maxLeafItems}"
    (20 * screenScale) (infoY + 20 * screenScale) fontSmall
  fillTextXY s!"query center: {formatVec2 state.queryCenter}" (20 * screenScale)
    (infoY + 40 * screenScale) fontSmall
  fillTextXY s!"k nearest: {state.kCount}" (20 * screenScale) (infoY + 60 * screenScale) fontSmall

  fillTextXY "QUADTREE VISUALIZER" (20 * screenScale) (30 * screenScale) fontMedium
  setFillColor (Color.gray 0.6)
  let modeText := if state.queryMode == .rect then "rect" else "circle"
  fillTextXY
    s!"Click: add point | Right click: move query | Q: {modeText} | 1-3: config | +/- size"
    (20 * screenScale) (55 * screenScale) fontSmall
  fillTextXY
    "Legend: exact hits = yellow, broad-phase candidates = orange"
    (20 * screenScale) (75 * screenScale) fontSmall

/-- Create quadtree visualizer widget. -/
def quadtreeVisualizerWidget (env : DemoEnv) (state : QuadtreeVisualizerState)
    : Afferent.Arbor.WidgetBuilder := do
  Afferent.Arbor.custom (spec := {
    measure := fun _ _ => (0, 0)
    collect := fun _ => #[]
    draw := some (fun layout => do
      withContentRect layout fun w h => do
        resetTransform
        renderQuadtreeVisualizer state w h env.screenScale env.fontMedium env.fontSmall
    )
  }) (style := { flexItem := some (Trellis.FlexItem.growing 1) })

end Demos.Linalg
