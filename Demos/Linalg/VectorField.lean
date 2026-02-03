/-
  Vector Field Demo - Shows various 2D vector fields.
  Press 1-4 to switch field types.
-/
import Afferent
import Afferent.Widget
import Afferent.Widget.VectorField
import Afferent.Arbor
import Demos.Core.Demo
import Demos.Linalg.Shared
import Trellis
import Linalg.Core
import Linalg.Vec2

open Afferent CanvasM Linalg
open Afferent.Widget
open Afferent.Widget.VectorField

namespace Demos.Linalg

/-- Types of vector fields -/
inductive FieldType where
  | radial       -- Vectors point outward from origin
  | rotational   -- Vectors rotate around origin
  | gradient     -- Diagonal gradient field
  | saddle       -- Saddle point field
  deriving BEq, Inhabited

/-- State for vector field demo -/
structure VectorFieldState where
  fieldType : FieldType := .rotational
  gridResolution : Nat := 12
  arrowScale : Float := 0.4
  showMagnitude : Bool := true
  deriving Inhabited

def vectorFieldInitialState : VectorFieldState := {}

def vectorFieldMathViewConfig (screenScale : Float) : MathView2D.Config := {
  style := { flexItem := some (Trellis.FlexItem.growing 1) }
  scale := 40.0 * screenScale
  minorStep := 1.0
  majorStep := 2.0
  gridMinorColor := Color.gray 0.1
  gridMajorColor := Color.gray 0.15
  axisColor := Color.gray 0.25
  labelColor := Color.gray 0.4
  labelPrecision := 0
}

/-- Compute vector at a point based on field type -/
def computeFieldVector (fieldType : FieldType) (p : Vec2) : Vec2 :=
  match fieldType with
  | .radial =>
    -- Vectors point outward from origin, magnitude decreases with distance
    let len := p.length
    if len < 0.001 then Vec2.zero
    else p.normalize * (1.0 / (1.0 + len * 0.3))
  | .rotational =>
    -- Vectors rotate counterclockwise around origin
    let len := p.length
    if len < 0.001 then Vec2.zero
    else p.perpendicular.normalize * (1.0 / (1.0 + len * 0.2))
  | .gradient =>
    -- Constant diagonal field
    Vec2.mk 0.7 0.7
  | .saddle =>
    -- Saddle point: expand in x, contract in y
    Vec2.mk (p.x * 0.3) (-p.y * 0.3)

/-- Get field type name -/
def fieldTypeName (ft : FieldType) : String :=
  match ft with
  | .radial => "Radial (outward)"
  | .rotational => "Rotational (curl)"
  | .gradient => "Gradient (constant)"
  | .saddle => "Saddle Point"

/-- Render the vector field visualization -/
def renderVectorField (state : VectorFieldState)
    (view : MathView2D.View) (screenScale : Float) (fontMedium fontSmall : Font) : CanvasM Unit := do
  let w := view.width
  let h := view.height
  let origin : Float × Float := (view.origin.x, view.origin.y)
  let sampling : Sampling2D := {
    samplesX := state.gridResolution
    samplesY := state.gridResolution
    computeMax := true
  }
  let arrowStyle : ArrowStyle := {
    lineWidth := 1.5 * screenScale
    headLength := 6.0 * screenScale
    headAngle := 0.5
    scale := state.arrowScale
  }
  let colorScale := ColorScale.blueCyanGreenYellowRed
  let coloring : Coloring := {
    mode := if state.showMagnitude then
      .magnitude colorScale
    else
      .fixed Color.cyan
  }

  let maxMag ← drawField2D view (computeFieldVector state.fieldType) sampling arrowStyle coloring

  -- Draw origin marker
  setFillColor (Color.white)
  fillPath (Afferent.Path.circle (Point.mk origin.1 origin.2) (4.0 * screenScale))

  -- Info panel
  let infoY := h - 80 * screenScale
  setFillColor VecColor.label
  fillTextXY s!"Field: {fieldTypeName state.fieldType}" (20 * screenScale) infoY fontSmall
  fillTextXY s!"Grid: {state.gridResolution + 1} x {state.gridResolution + 1}" (20 * screenScale) (infoY + 22 * screenScale) fontSmall
  fillTextXY s!"Max magnitude: {formatFloat maxMag}" (20 * screenScale) (infoY + 44 * screenScale) fontSmall

  -- Title and instructions
  setFillColor VecColor.label
  fillTextXY s!"VECTOR FIELD: {fieldTypeName state.fieldType |>.toUpper}" (20 * screenScale) (30 * screenScale) fontMedium
  setFillColor (Color.gray 0.6)
  fillTextXY "Keys: 1=Radial, 2=Rotational, 3=Gradient, 4=Saddle | +/-: grid density" (20 * screenScale) (55 * screenScale) fontSmall

  -- Color legend
  if state.showMagnitude then
    let legendX := w - 180 * screenScale
    let legendY := 30 * screenScale
    setFillColor (Color.gray 0.7)
    fillTextXY "Magnitude:" legendX legendY fontSmall

    -- Draw color bar
    let barX := legendX
    let barY := legendY + 15 * screenScale
    let barW := 120 * screenScale
    let barH := 12 * screenScale
    let steps := 20
    for i in [:steps] do
      let t := i.toFloat / (steps - 1).toFloat
      let color := colorForScale colorScale t
      let x := barX + t * barW
      setFillColor color
      fillPath (Afferent.Path.rectangleXYWH x barY (barW / steps.toFloat + 1) barH)

    setFillColor (Color.gray 0.5)
    fillTextXY "low" barX (barY + barH + 12 * screenScale) fontSmall
    let (hiW, _) ← fontSmall.measureText "high"
    fillTextXY "high" (barX + barW - hiW) (barY + barH + 12 * screenScale) fontSmall

/-- Create the vector field widget -/
def vectorFieldWidget (env : DemoEnv) (state : VectorFieldState)
    : Afferent.Arbor.WidgetBuilder := do
  let config := vectorFieldMathViewConfig env.screenScale
  MathView2D.mathView2D config env.fontSmall (fun view => do
    renderVectorField state view env.screenScale env.fontMedium env.fontSmall
  )

end Demos.Linalg
