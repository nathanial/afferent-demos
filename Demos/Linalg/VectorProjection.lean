/-
  Vector Projection Demo - Shows projection and reflection operations.
  Press P/R/B to switch modes, drag to move vectors.
-/
import Afferent
import Afferent.Widget
import Afferent.Arbor
import Demos.Core.Demo
import Demos.Linalg.Shared
import Trellis
import Linalg.Core
import Linalg.Vec2
import AfferentMath.Widget.MathView2D

open Afferent CanvasM Linalg
open Afferent.Widget
open AfferentMath.Widget

namespace Demos.Linalg

/-- Display modes for projection demo -/
inductive ProjectionMode where
  | projection
  | reflection
  | both
  deriving BEq, Inhabited

/-- Drag target for projection demo -/
inductive ProjDragTarget where
  | vectorV
  | vectorU
  deriving BEq, Inhabited

/-- State for vector projection demo -/
structure VectorProjectionState where
  vectorV : Vec2 := Vec2.mk 3.0 2.5
  vectorU : Vec2 := Vec2.mk 4.0 1.0
  showMode : ProjectionMode := .both
  dragging : Option ProjDragTarget := none
  deriving Inhabited

def vectorProjectionInitialState : VectorProjectionState := {}

def vectorProjectionMathViewConfig (screenScale : Float) : MathView2D.Config := {
  style := { flexItem := some (Trellis.FlexItem.growing 1) }
  scale := 50.0 * screenScale
  minorStep := 1.0
  majorStep := 2.0
  gridMinorColor := Color.gray 0.2
  gridMajorColor := Color.gray 0.4
  axisColor := Color.gray 0.6
  labelColor := VecColor.label
  labelPrecision := 0
}

/-- Render the vector projection visualization -/
def renderVectorProjection (state : VectorProjectionState)
    (view : MathView2D.View) (screenScale : Float) (fontMedium fontSmall : Font) : CanvasM Unit := do
  let w := view.width
  let h := view.height
  let origin : Float × Float := (view.origin.x, view.origin.y)
  let scale := view.scale

  -- Calculate projection and related vectors
  let proj := Vec2.project state.vectorV state.vectorU
  let perp := state.vectorV - proj
  let reflected :=
    if state.vectorU.length > Float.epsilon then
      (proj * 2.0) - state.vectorV
    else
      state.vectorV

  -- Draw the base vector U (extended line for reference)
  let uNorm := state.vectorU.normalize
  let lineExtent := 6.0
  let lineStart := uNorm * (-lineExtent)
  let lineEnd := uNorm * lineExtent
  let screenLineStart := worldToScreen lineStart origin scale
  let screenLineEnd := worldToScreen lineEnd origin scale
  drawDashedLine screenLineStart screenLineEnd (Color.gray 0.4) 10.0 5.0 1.5

  -- Draw vector U (the axis/normal) - drawn early so projection renders on top
  drawVectorArrow Vec2.zero state.vectorU origin scale
    { color := VecColor.vectorB, lineWidth := 3.0 }
  drawMarker state.vectorU origin scale VecColor.vectorB 8.0

  -- Show projection visualization
  if state.showMode == .projection || state.showMode == .both then
    -- Draw perpendicular drop line (from V to projection)
    let screenV := worldToScreen state.vectorV origin scale
    let screenProj := worldToScreen proj origin scale
    drawDashedLine screenV screenProj VecColor.perpendicular 4.0 4.0 1.5

    -- Draw right angle marker at projection point
    let perpDir := (state.vectorV - proj)
    if perpDir.length > 0.1 then
      let perpScreen := worldToScreen perp origin scale
      let projScreen := worldToScreen proj origin scale
      let uDirScreen := (screenLineEnd.1 - screenLineStart.1, screenLineEnd.2 - screenLineStart.2)
      let perpDirScreen := (perpScreen.1 - projScreen.1 + origin.1 - w/2,
                           perpScreen.2 - projScreen.2 + origin.2 - h/2)
      -- Normalize for right angle marker
      let uLen := Float.sqrt (uDirScreen.1 * uDirScreen.1 + uDirScreen.2 * uDirScreen.2)
      let pLen := Float.sqrt (perpDirScreen.1 * perpDirScreen.1 + perpDirScreen.2 * perpDirScreen.2)
      if uLen > 0.1 && pLen > 0.1 then
        let uDir := (uDirScreen.1 / uLen, uDirScreen.2 / uLen)
        let pDir := (perpDirScreen.1 / pLen, perpDirScreen.2 / pLen)
        drawRightAngleMarker projScreen uDir pDir (Color.gray 0.7) (12.0 * screenScale)

    -- Draw projection vector (green)
    drawVectorArrow Vec2.zero proj origin scale
      { color := VecColor.projection, lineWidth := 3.0 }
    drawMarker proj origin scale VecColor.projection 6.0

    -- Draw perpendicular component (red, from projection to V)
    if perp.length > 0.1 then
      drawVectorArrow proj perp origin scale
        { color := VecColor.perpendicular, lineWidth := 2.0 }

  -- Show reflection visualization
  if state.showMode == .reflection || state.showMode == .both then
    -- Draw reflected vector (yellow)
    drawVectorArrow Vec2.zero reflected origin scale
      { color := VecColor.reflection, lineWidth := 2.5 }
    drawMarker reflected origin scale VecColor.reflection 6.0

    -- Draw dashed line showing the reflection relationship
    let screenV := worldToScreen state.vectorV origin scale
    let screenRefl := worldToScreen reflected origin scale
    -- Line from V through midpoint to reflected
    drawDashedLine screenV screenRefl (Color.gray 0.4) 6.0 6.0 1.0

  -- Draw vector V (the input vector)
  drawVectorArrow Vec2.zero state.vectorV origin scale
    { color := VecColor.vectorA, lineWidth := 3.0 }
  drawMarker state.vectorV origin scale VecColor.vectorA 8.0

  -- Labels at vector tips
  let (vx, vy) := worldToScreen state.vectorV origin scale
  let (ux, uy) := worldToScreen state.vectorU origin scale

  setFillColor VecColor.vectorA
  fillTextXY "V" (vx + 12) (vy - 8) fontSmall

  setFillColor VecColor.vectorB
  fillTextXY "U" (ux + 12) (uy - 8) fontSmall

  if state.showMode == .projection || state.showMode == .both then
    let (px, py) := worldToScreen proj origin scale
    setFillColor VecColor.projection
    fillTextXY "proj" (px + 12) (py + 16) fontSmall

  if state.showMode == .reflection || state.showMode == .both then
    let (rx, ry) := worldToScreen reflected origin scale
    setFillColor VecColor.reflection
    fillTextXY "refl" (rx + 12) (ry - 8) fontSmall

  -- Info panel
  let infoY := h - 140 * screenScale
  setFillColor VecColor.label
  let modeName := match state.showMode with
    | .projection => "Projection Only"
    | .reflection => "Reflection Only"
    | .both => "Both"
  fillTextXY s!"Mode: {modeName}" (20 * screenScale) infoY fontSmall
  fillTextXY s!"V = {formatVec2 state.vectorV}" (20 * screenScale) (infoY + 22 * screenScale) fontSmall
  fillTextXY s!"U = {formatVec2 state.vectorU}  (projection axis)" (20 * screenScale) (infoY + 44 * screenScale) fontSmall
  fillTextXY s!"proj_U(V) = {formatVec2 proj}" (20 * screenScale) (infoY + 66 * screenScale) fontSmall
  fillTextXY s!"reflect_U(V) = {formatVec2 reflected}" (20 * screenScale) (infoY + 88 * screenScale) fontSmall
  let dotProduct := Vec2.dot state.vectorV state.vectorU
  fillTextXY s!"V · U = {formatFloat dotProduct}" (20 * screenScale) (infoY + 110 * screenScale) fontSmall

  -- Title and instructions
  fillTextXY "VECTOR PROJECTION & REFLECTION" (20 * screenScale) (30 * screenScale) fontMedium
  setFillColor (Color.gray 0.6)
  fillTextXY "Keys: P=Projection, R=Reflection, B=Both | Drag: move V or U" (20 * screenScale) (55 * screenScale) fontSmall

/-- Create the projection widget -/
def vectorProjectionWidget (env : DemoEnv) (state : VectorProjectionState)
    : Afferent.Arbor.WidgetBuilder := do
  let config := vectorProjectionMathViewConfig env.screenScale
  MathView2D.mathView2D config env.fontSmall (fun view => do
    renderVectorProjection state view env.screenScale env.fontMedium env.fontSmall
  )

end Demos.Linalg
