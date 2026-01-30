/-
  Vector Arithmetic Demo - Shows add, subtract, and scale operations.
  Press 1/2/3 to switch operations, drag to move vectors.
-/
import Afferent
import Afferent.Widget
import Afferent.Arbor
import Demos.Core.Demo
import Demos.Linalg.Shared
import Trellis
import Linalg.Core
import Linalg.Vec2

open Afferent CanvasM Linalg

namespace Demos.Linalg

/-- Operations available in arithmetic demo -/
inductive ArithmeticOperation where
  | add
  | sub
  | scale
  deriving BEq, Inhabited

/-- Drag target for arithmetic demo -/
inductive ArithDragTarget where
  | vectorA
  | vectorB
  deriving BEq, Inhabited

/-- State for vector arithmetic demo -/
structure VectorArithmeticState where
  vectorA : Vec2 := Vec2.mk 2.0 1.5
  vectorB : Vec2 := Vec2.mk 1.0 2.5
  operation : ArithmeticOperation := .add
  scaleFactor : Float := 1.5
  dragging : Option ArithDragTarget := none
  deriving Inhabited

def vectorArithmeticInitialState : VectorArithmeticState := {}

/-- Draw a parallelogram showing vector addition -/
def drawParallelogram (a b : Vec2) (origin : Float × Float) (scale : Float) : CanvasM Unit := do
  let o := Vec2.zero
  let screenO := worldToScreen o origin scale
  let screenA := worldToScreen a origin scale
  let screenB := worldToScreen b origin scale
  let screenAB := worldToScreen (a + b) origin scale

  setFillColor (Color.rgba 0.3 0.8 0.3 0.15)
  let path := Afferent.Path.empty
    |>.moveTo (Point.mk screenO.1 screenO.2)
    |>.lineTo (Point.mk screenA.1 screenA.2)
    |>.lineTo (Point.mk screenAB.1 screenAB.2)
    |>.lineTo (Point.mk screenB.1 screenB.2)
    |>.closePath
  fillPath path

  -- Dashed lines to show the parallelogram
  drawDashedLine screenA screenAB (Color.gray 0.5) 6.0 4.0 1.5
  drawDashedLine screenB screenAB (Color.gray 0.5) 6.0 4.0 1.5

/-- Render the vector arithmetic visualization -/
def renderVectorArithmetic (state : VectorArithmeticState)
    (w h : Float) (screenScale : Float) (fontMedium fontSmall : Font) : CanvasM Unit := do
  let origin : Float × Float := (w / 2, h / 2)
  let scale : Float := 50.0 * screenScale

  -- Draw grid
  drawGrid2D {
    origin := origin
    scale := scale
    width := w
    height := h
    majorSpacing := 2.0
  } fontSmall

  -- Calculate result based on operation
  let (result, opName, opSymbol) := match state.operation with
    | .add => (state.vectorA + state.vectorB, "Addition", "+")
    | .sub => (state.vectorA - state.vectorB, "Subtraction", "-")
    | .scale => (state.vectorA * state.scaleFactor, s!"Scale by {formatFloat state.scaleFactor}", "*")

  -- Draw operation-specific visualizations
  match state.operation with
  | .add =>
    -- Draw parallelogram
    drawParallelogram state.vectorA state.vectorB origin scale
    -- Draw B translated to tip of A
    let screenATip := worldToScreen state.vectorA origin scale
    let screenBTranslated := worldToScreen (state.vectorA + state.vectorB) origin scale
    drawDashedLine screenATip screenBTranslated VecColor.vectorB 5.0 3.0 2.0

  | .sub =>
    -- Draw B starting from tip of result pointing to A
    -- A - B = result means result + B = A
    let screenResult := worldToScreen result origin scale
    let screenA := worldToScreen state.vectorA origin scale
    drawDashedLine screenResult screenA VecColor.vectorB 5.0 3.0 2.0
    -- Show -B from origin
    drawVectorArrow Vec2.zero (-state.vectorB) origin scale
      { color := Color.rgba 1.0 0.5 1.0 0.5, lineWidth := 1.5 }

  | .scale =>
    -- Draw the original vector as dashed
    drawVectorArrow Vec2.zero state.vectorA origin scale
      { color := Color.rgba 0.0 1.0 1.0 0.4, lineWidth := 1.5 }

  -- Draw vector A
  drawVectorArrow Vec2.zero state.vectorA origin scale
    { color := VecColor.vectorA, lineWidth := 3.0 }
  drawMarker state.vectorA origin scale VecColor.vectorA 8.0

  -- Draw vector B (only for add/sub)
  if state.operation != .scale then
    drawVectorArrow Vec2.zero state.vectorB origin scale
      { color := VecColor.vectorB, lineWidth := 3.0 }
    drawMarker state.vectorB origin scale VecColor.vectorB 8.0

  -- Draw result vector
  drawVectorArrow Vec2.zero result origin scale
    { color := VecColor.result, lineWidth := 3.0 }
  drawMarker result origin scale VecColor.result 8.0

  -- Labels at vector tips
  let (ax, ay) := worldToScreen state.vectorA origin scale
  setFillColor VecColor.vectorA
  fillTextXY "A" (ax + 12) (ay - 8) fontSmall

  if state.operation != .scale then
    let (bx, byy) := worldToScreen state.vectorB origin scale
    setFillColor VecColor.vectorB
    fillTextXY "B" (bx + 12) (byy - 8) fontSmall

  let (rx, ry) := worldToScreen result origin scale
  setFillColor VecColor.result
  let resultLabel := match state.operation with
    | .add => "A+B"
    | .sub => "A-B"
    | .scale => s!"{formatFloat state.scaleFactor}*A"
  fillTextXY resultLabel (rx + 12) (ry - 8) fontSmall

  -- Info panel
  let infoY := h - 120 * screenScale
  setFillColor VecColor.label
  fillTextXY s!"Operation: {opName}" (20 * screenScale) infoY fontSmall
  fillTextXY s!"A = {formatVec2 state.vectorA}  |A| = {formatFloat state.vectorA.length}" (20 * screenScale) (infoY + 22 * screenScale) fontSmall
  if state.operation != .scale then
    fillTextXY s!"B = {formatVec2 state.vectorB}  |B| = {formatFloat state.vectorB.length}" (20 * screenScale) (infoY + 44 * screenScale) fontSmall
    fillTextXY s!"Result = {formatVec2 result}  |R| = {formatFloat result.length}" (20 * screenScale) (infoY + 66 * screenScale) fontSmall
  else
    fillTextXY s!"Scale factor = {formatFloat state.scaleFactor}" (20 * screenScale) (infoY + 44 * screenScale) fontSmall
    fillTextXY s!"Result = {formatVec2 result}  |R| = {formatFloat result.length}" (20 * screenScale) (infoY + 66 * screenScale) fontSmall

  -- Title and instructions
  fillTextXY s!"VECTOR ARITHMETIC: {opName.toUpper}" (20 * screenScale) (30 * screenScale) fontMedium
  setFillColor (Color.gray 0.6)
  let opText := match state.operation with
    | .add => "1"
    | .sub => "2"
    | .scale => "3"
  fillTextXY s!"Keys: 1=Add, 2=Sub, 3=Scale (current: {opText}) | +/-: adjust scale | Drag: move vectors" (20 * screenScale) (55 * screenScale) fontSmall

/-- Create the arithmetic widget -/
def vectorArithmeticWidget (env : DemoEnv) (state : VectorArithmeticState)
    : Afferent.Arbor.WidgetBuilder := do
  Afferent.Arbor.custom (spec := {
    measure := fun _ _ => (0, 0)
    collect := fun _ => #[]
    draw := some (fun layout => do
      withContentRect layout fun w h => do
        resetTransform
        renderVectorArithmetic state w h env.screenScale env.fontMedium env.fontSmall
    )
  }) (style := { flexItem := some (Trellis.FlexItem.growing 1) })

end Demos.Linalg
