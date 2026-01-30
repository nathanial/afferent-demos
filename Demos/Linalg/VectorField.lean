/-
  Vector Field Demo - Shows various 2D vector fields.
  Press 1-4 to switch field types.
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

/-- Interpolate between two colors based on a value 0-1 -/
def lerpColor (c1 c2 : Color) (t : Float) : Color :=
  let t' := Float.max 0.0 (Float.min 1.0 t)
  Color.rgba
    (c1.r + (c2.r - c1.r) * t')
    (c1.g + (c2.g - c1.g) * t')
    (c1.b + (c2.b - c1.b) * t')
    (c1.a + (c2.a - c1.a) * t')

/-- Get color based on magnitude (blue -> cyan -> green -> yellow -> red) -/
def magnitudeColor (magnitude : Float) (maxMag : Float) : Color :=
  let t := if maxMag > 0.001 then magnitude / maxMag else 0.0
  let t' := Float.max 0.0 (Float.min 1.0 t)
  -- Multi-stop gradient
  if t' < 0.25 then
    lerpColor (Color.rgba 0.2 0.3 0.8 1.0) (Color.rgba 0.0 0.8 0.8 1.0) (t' * 4.0)
  else if t' < 0.5 then
    lerpColor (Color.rgba 0.0 0.8 0.8 1.0) (Color.rgba 0.2 0.9 0.2 1.0) ((t' - 0.25) * 4.0)
  else if t' < 0.75 then
    lerpColor (Color.rgba 0.2 0.9 0.2 1.0) (Color.rgba 0.9 0.9 0.0 1.0) ((t' - 0.5) * 4.0)
  else
    lerpColor (Color.rgba 0.9 0.9 0.0 1.0) (Color.rgba 0.9 0.2 0.2 1.0) ((t' - 0.75) * 4.0)

/-- Render the vector field visualization -/
def renderVectorField (state : VectorFieldState)
    (w h : Float) (screenScale : Float) (fontMedium fontSmall : Font) : CanvasM Unit := do
  let origin : Float × Float := (w / 2, h / 2)
  let scale : Float := 40.0 * screenScale

  -- Draw subtle grid
  drawGrid2D {
    origin := origin
    scale := scale
    width := w
    height := h
    majorSpacing := 2.0
    minorColor := Color.gray 0.1
    majorColor := Color.gray 0.15
    axisColor := Color.gray 0.25
    labelColor := Color.gray 0.4
  } fontSmall

  -- Calculate grid bounds
  let halfGridX := (state.gridResolution.toFloat / 2.0).floor
  let halfGridY := (state.gridResolution.toFloat / 2.0).floor

  -- First pass: find maximum magnitude for normalization
  let mut maxMag : Float := 0.001
  for i in [:state.gridResolution + 1] do
    for j in [:state.gridResolution + 1] do
      let gx := i.toFloat - halfGridX
      let gy := j.toFloat - halfGridY
      let p := Vec2.mk gx gy
      let v := computeFieldVector state.fieldType p
      let mag := v.length
      if mag > maxMag then
        maxMag := mag

  -- Second pass: draw arrows
  for i in [:state.gridResolution + 1] do
    for j in [:state.gridResolution + 1] do
      let gx := i.toFloat - halfGridX
      let gy := j.toFloat - halfGridY
      let p := Vec2.mk gx gy
      let v := computeFieldVector state.fieldType p
      let mag := v.length

      if mag > 0.001 then
        -- Scale arrow length
        let arrowVec := v.normalize * state.arrowScale
        let screenStart := worldToScreen p origin scale
        let screenEnd := worldToScreen (p + arrowVec) origin scale

        -- Color by magnitude
        let color := if state.showMagnitude then
          magnitudeColor mag maxMag
        else
          Color.cyan

        drawArrow2D screenStart screenEnd {
          color := color
          lineWidth := 1.5 * screenScale
          headLength := 6.0 * screenScale
          headAngle := 0.5
        }

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
    let color := magnitudeColor t 1.0
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
  Afferent.Arbor.custom (spec := {
    measure := fun _ _ => (0, 0)
    collect := fun _ => #[]
    draw := some (fun layout => do
      withContentRect layout fun w h => do
        resetTransform
        renderVectorField state w h env.screenScale env.fontMedium env.fontSmall
    )
  }) (style := { flexItem := some (Trellis.FlexItem.growing 1) })

end Demos.Linalg
