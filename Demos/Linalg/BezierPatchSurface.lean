/- 
  Bezier Patch Surface - 4x4 control grid, tessellation, normals, and isocurves.
-/
import Afferent
import Afferent.Widget
import Afferent.Arbor
import Demos.Core.Demo
import Demos.Linalg.Shared
import Demos.Linalg.RotationShared
import Trellis
import Linalg.Core
import Linalg.Vec2
import Linalg.Vec3
import Linalg.Curves
import AfferentMath.Widget.MathView3D

open Afferent CanvasM Linalg
open Afferent.Widget
open AfferentMath.Widget

namespace Demos.Linalg

/-- Drag state for Bezier patch. -/
inductive PatchDragMode where
  | none
  | point (idx : Nat)
  | camera
  deriving BEq, Inhabited

/-- Create a default Bezier patch with a gentle bump. -/
private def defaultBezierPatch : Linalg.BezierPatch := Id.run do
  let base := Linalg.BezierPatch.flat 4.6 4.6
  let mut patch := base
  for row in [:4] do
    for col in [:4] do
      let p := base.getPoint row col
      let bump := if (row == 1 || row == 2) && (col == 1 || col == 2) then 1.0
        else if row == 1 || row == 2 || col == 1 || col == 2 then 0.4
        else 0.0
      patch := patch.setPoint row col (Vec3.mk p.x p.y bump)
  return patch

/-- State for Bezier patch demo. -/
structure BezierPatchSurfaceState where
  patch : Linalg.BezierPatch := defaultBezierPatch
  selected : Option Nat := none
  dragging : PatchDragMode := .none
  cameraYaw : Float := 0.6
  cameraPitch : Float := 0.45
  tessellation : Nat := 10
  showNormals : Bool := true
  lastMouseX : Float := 0.0
  lastMouseY : Float := 0.0
  deriving Inhabited


def bezierPatchSurfaceInitialState : BezierPatchSurfaceState := {}

def bezierPatchSurfaceMathViewConfig (state : BezierPatchSurfaceState) (screenScale : Float)
    : MathView3D.Config := {
  style := { flexItem := some (Trellis.FlexItem.growing 1) }
  camera := { yaw := state.cameraYaw, pitch := state.cameraPitch, distance := 9.0 }
  originOffset := (0.0, 20.0 * screenScale)
  showGrid := false
  showAxes := false
  axisLineWidth := 2.0 * screenScale
}

/-- Mini editor rect in screen space. -/
structure MiniRect where
  x : Float
  y : Float
  w : Float
  h : Float

private def miniRect (w _h screenScale : Float) : MiniRect :=
  { x := w - 260.0 * screenScale
    y := 110.0 * screenScale
    w := 220.0 * screenScale
    h := 220.0 * screenScale }

private def miniOrigin (rect : MiniRect) : Float × Float :=
  (rect.x + rect.w / 2, rect.y + rect.h / 2)

private def miniScale (rect : MiniRect) : Float :=
  rect.w / 5.8

private def drawArrow3DFrom (view : MathView3D.View) (start vec : Vec3)
    (config : ArrowConfig := {}) : CanvasM Unit := do
  match rotProject3Dto2D view start, rotProject3Dto2D view (start + vec) with
  | some s, some e => drawArrow2D s e config
  | _, _ => pure ()

private def drawPolyline3D (view : MathView3D.View) (points : Array Vec3)
    (color : Color) (lineWidth : Float := 1.5) : CanvasM Unit := do
  MathView3D.drawPolyline3D view points color lineWidth

private def drawPatchMesh (view : MathView3D.View) (patch : Linalg.BezierPatch)
    (rows cols : Nat) : CanvasM Unit := do
  let r := if rows < 2 then 2 else rows
  let c := if cols < 2 then 2 else cols
  let grid := Linalg.BezierPatch.sample patch r c
  setStrokeColor (Color.rgba 0.2 0.85 1.0 0.6)
  setLineWidth 1.2
  for row in grid do
    let rowPts := row
    drawPolyline3D view rowPts (Color.rgba 0.2 0.85 1.0 0.6) 1.2
  for col in [:c] do
    let mut colPts : Array Vec3 := #[]
    for rowIdx in [:r] do
      let row := grid.getD rowIdx #[]
      colPts := colPts.push (row.getD col Vec3.zero)
    drawPolyline3D view colPts (Color.rgba 0.2 0.85 1.0 0.6) 1.2

private def drawControlNet (view : MathView3D.View) (patch : Linalg.BezierPatch)
    (selected : Option Nat) : CanvasM Unit := do
  for row in [:4] do
    let mut rowPts : Array Vec3 := #[]
    for col in [:4] do
      rowPts := rowPts.push (patch.getPoint row col)
    drawPolyline3D view rowPts (Color.gray 0.4) 1.0
  for col in [:4] do
    let mut colPts : Array Vec3 := #[]
    for row in [:4] do
      colPts := colPts.push (patch.getPoint row col)
    drawPolyline3D view colPts (Color.gray 0.4) 1.0

  for idx in [:16] do
    let row := idx / 4
    let col := idx % 4
    let p := patch.getPoint row col
    let isSelected := match selected with | some i => i == idx | none => false
    match rotProject3Dto2D view p with
    | some (x, y) =>
        setFillColor (if isSelected then Color.yellow else Color.rgba 0.8 0.8 0.9 1.0)
        fillPath (Afferent.Path.circle (Point.mk x y) (if isSelected then 6.0 else 4.5))
    | none => pure ()

private def drawIsocurves (view : MathView3D.View) (patch : Linalg.BezierPatch) : CanvasM Unit := do
  let values : Array Float := #[0.25, 0.5, 0.75]
  for u in values do
    let curve := Linalg.BezierPatch.isocurveU patch u
    let pts := Id.run do
      let mut arr : Array Vec3 := #[]
      for i in [:40] do
        let t := i.toFloat / 39.0
        arr := arr.push (Linalg.Bezier3.evalVec3 curve t)
      return arr
    drawPolyline3D view pts (Color.rgba 0.4 0.9 0.4 0.6) 1.0
  for v in values do
    let curve := Linalg.BezierPatch.isocurveV patch v
    let pts := Id.run do
      let mut arr : Array Vec3 := #[]
      for i in [:40] do
        let t := i.toFloat / 39.0
        arr := arr.push (Linalg.Bezier3.evalVec3 curve t)
      return arr
    drawPolyline3D view pts (Color.rgba 0.9 0.5 0.4 0.6) 1.0

/-- Render Bezier patch surface demo. -/
def renderBezierPatchSurface (state : BezierPatchSurfaceState)
    (view : MathView3D.View) (screenScale : Float) (fontMedium fontSmall : Font) : CanvasM Unit := do
  let w := view.width
  let h := view.height

  rotDraw3DAxes view 2.0 fontSmall
  drawPatchMesh view state.patch state.tessellation state.tessellation
  drawIsocurves view state.patch
  drawControlNet view state.patch state.selected

  -- Surface tangent/normal at center
  let center := Linalg.BezierPatch.eval state.patch 0.5 0.5
  let du := Linalg.BezierPatch.derivativeU state.patch 0.5 0.5 |>.normalize
  let dv := Linalg.BezierPatch.derivativeV state.patch 0.5 0.5 |>.normalize
  let n := Linalg.BezierPatch.normal state.patch 0.5 0.5
  drawArrow3DFrom view center (du.scale 0.8) { color := VecColor.xAxis, lineWidth := 2.0 }
  drawArrow3DFrom view center (dv.scale 0.8) { color := VecColor.yAxis, lineWidth := 2.0 }
  drawArrow3DFrom view center (n.scale 0.9) { color := Color.yellow, lineWidth := 2.2 }

  -- Normals
  if state.showNormals then
    let samples := Linalg.BezierPatch.sampleWithNormals state.patch 6 6
    for (pos, norm) in samples do
      drawArrow3DFrom view pos (norm.scale 0.4)
        { color := Color.rgba 1.0 0.9 0.4 0.6, lineWidth := 1.0 }

  -- Mini editor view (top-down)
  let rect := miniRect w h screenScale
  let mOrigin := miniOrigin rect
  let mScale := miniScale rect
  setStrokeColor (Color.gray 0.4)
  setLineWidth 1.0
  strokePath (Afferent.Path.rectangleXYWH rect.x rect.y rect.w rect.h)

  -- Axes in mini view
  setStrokeColor (Color.gray 0.35)
  let (ox, oy) := mOrigin
  strokePath (Afferent.Path.empty
    |>.moveTo (Point.mk rect.x oy)
    |>.lineTo (Point.mk (rect.x + rect.w) oy))
  strokePath (Afferent.Path.empty
    |>.moveTo (Point.mk ox rect.y)
    |>.lineTo (Point.mk ox (rect.y + rect.h)))

  for idx in [:16] do
    let row := idx / 4
    let col := idx % 4
    let p := state.patch.getPoint row col
    let (sx, sy) := worldToScreen (Vec2.mk p.x p.y) mOrigin mScale
    let isSelected := match state.selected with | some i => i == idx | none => false
    setFillColor (if isSelected then Color.yellow else Color.rgba 0.7 0.8 1.0 1.0)
    fillPath (Afferent.Path.circle (Point.mk sx sy) (if isSelected then 6.0 else 4.5))

  -- Labels
  setFillColor VecColor.label
  fillTextXY "BEZIER PATCH SURFACE" (20 * screenScale) (30 * screenScale) fontMedium
  setFillColor (Color.gray 0.6)
  fillTextXY "Drag points in mini view | Right-drag: camera | N: normals | [ / ]: tessellation | ↑/↓: raise/lower"
    (20 * screenScale) (55 * screenScale) fontSmall
  setFillColor VecColor.label
  fillTextXY s!"tessellation={state.tessellation}  normals={state.showNormals}" (20 * screenScale) (h - 40 * screenScale) fontSmall

/-- Create Bezier patch surface widget. -/
def bezierPatchSurfaceWidget (env : DemoEnv) (state : BezierPatchSurfaceState)
    : Afferent.Arbor.WidgetBuilder := do
  let config := bezierPatchSurfaceMathViewConfig state env.screenScale
  MathView3D.mathView3D config env.fontSmall (fun view => do
    renderBezierPatchSurface state view env.screenScale env.fontMedium env.fontSmall
  )

end Demos.Linalg
