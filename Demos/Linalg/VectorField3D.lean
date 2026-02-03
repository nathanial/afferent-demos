/-
  Vector Field 3D Demo - Shows 3D vector fields with arrows and magnitude coloring.
  Keys:
    1-4: switch field types
    plus/minus: change XY grid density
    [ / ]: change Z slices
    M: toggle magnitude coloring
    V: toggle arrow length scaling by magnitude
-/
import Afferent
import Afferent.Widget
import Afferent.Widget.VectorField
import Afferent.Arbor
import Demos.Core.Demo
import Demos.Linalg.Shared
import Trellis
import Linalg.Core
import Linalg.Vec3

open Afferent CanvasM Linalg
open Afferent.Widget
open Afferent.Widget.VectorField

namespace Demos.Linalg

/-- Types of 3D vector fields. -/
inductive FieldType3D where
  | radial
  | swirl
  | saddle
  | helix
  deriving BEq, Inhabited

/-- State for 3D vector field demo. -/
structure VectorField3DState where
  fieldType : FieldType3D := .swirl
  samplesXY : Nat := 8
  samplesZ : Nat := 4
  arrowScale : Float := 0.55
  showMagnitude : Bool := true
  scaleByMagnitude : Bool := false
  deriving Inhabited

def vectorField3DInitialState : VectorField3DState := {}

/-- Get field type name. -/
def fieldType3DName (ft : FieldType3D) : String :=
  match ft with
  | .radial => "Radial"
  | .swirl => "Swirl"
  | .saddle => "Saddle"
  | .helix => "Helix"

/-- Compute vector at a 3D point based on field type. -/
def computeFieldVector3D (fieldType : FieldType3D) (p : Vec3) : Vec3 :=
  let len := p.length
  let falloff := 1.0 / (1.0 + len * 0.35)
  match fieldType with
  | .radial =>
      if len < 0.001 then Vec3.zero
      else p.normalize.scale falloff
  | .swirl =>
      let base := Vec3.mk (-p.z) 0.0 p.x
      if base.length < 0.001 then Vec3.zero
      else base.normalize.scale (falloff * 0.9)
  | .saddle =>
      Vec3.mk (p.x * 0.35) (-p.y * 0.35) (-p.z * 0.35)
  | .helix =>
      let base := Vec3.mk (-p.z) 0.0 p.x
      let up := Vec3.unitY.scale 0.6
      (base.add up).scale (falloff * 0.75)

/-- MathView3D config for vector field demo. -/
def vectorField3DMathViewConfig (screenScale : Float) : MathView3D.Config := {
  style := { flexItem := some (Trellis.FlexItem.growing 1) }
  camera := { yaw := 0.7, pitch := 0.35, distance := 9.0 }
  gridExtent := 3.0
  gridStep := 0.5
  gridMajorStep := 1.0
  axisLength := 3.5
  showAxes := true
  showAxisLabels := false
  gridLineWidth := 1.0 * screenScale
  axisLineWidth := 2.0 * screenScale
}

/-- Render overlay (HUD) for the 3D vector field demo. -/
def renderVectorField3DOverlay (state : VectorField3DState)
    (view : MathView3D.View) (maxMag : Float)
    (screenScale : Float) (fontMedium fontSmall : Font) : CanvasM Unit := do
  let w := view.width
  let h := view.height

  -- Title and instructions
  setFillColor VecColor.label
  fillTextXY "VECTOR FIELD 3D" (20 * screenScale) (30 * screenScale) fontMedium
  setFillColor (Color.gray 0.6)
  fillTextXY "Keys: 1-4 field | +/- density | [/] z-slices | M magnitude | V scale"
    (20 * screenScale) (55 * screenScale) fontSmall

  -- Info panel
  let infoY := h - 90 * screenScale
  setFillColor VecColor.label
  fillTextXY s!"Field: {fieldType3DName state.fieldType}" (20 * screenScale) infoY fontSmall
  fillTextXY s!"Samples: {state.samplesXY + 1} x {state.samplesXY + 1} x {state.samplesZ + 1}"
    (20 * screenScale) (infoY + 22 * screenScale) fontSmall
  fillTextXY s!"Max magnitude: {formatFloat maxMag}" (20 * screenScale) (infoY + 44 * screenScale) fontSmall

  -- Color legend
  if state.showMagnitude then
    let legendX := w - 180 * screenScale
    let legendY := 30 * screenScale
    setFillColor (Color.gray 0.7)
    fillTextXY "Magnitude:" legendX legendY fontSmall

    let barX := legendX
    let barY := legendY + 15 * screenScale
    let barW := 120 * screenScale
    let barH := 12 * screenScale
    let steps := 20
    for i in [:steps] do
      let t := i.toFloat / (steps - 1).toFloat
      let color := VectorField.colorForScale .viridis t
      let x := barX + t * barW
      setFillColor color
      fillPath (Afferent.Path.rectangleXYWH x barY (barW / steps.toFloat + 1) barH)

    setFillColor (Color.gray 0.5)
    fillTextXY "low" barX (barY + barH + 12 * screenScale) fontSmall
    let (hiW, _) ← fontSmall.measureText "high"
    fillTextXY "high" (barX + barW - hiW) (barY + barH + 12 * screenScale) fontSmall

/-- Create the 3D vector field widget. -/
def vectorField3DWidget (env : DemoEnv) (state : VectorField3DState)
    : Afferent.Arbor.WidgetBuilder := do
  let viewConfig := vectorField3DMathViewConfig env.screenScale
  let sampling : VectorField.Sampling3D := {
    samplesX := state.samplesXY
    samplesY := state.samplesXY
    samplesZ := state.samplesZ
    extent := 3.0
    computeMax := true
  }
  let arrows : VectorField.ArrowStyle := {
    lineWidth := 1.2 * env.screenScale
    headLength := 6.0 * env.screenScale
    headAngle := 0.5
    scale := state.arrowScale
    scaleMode := .cell
    scaleByMagnitude := state.scaleByMagnitude
  }
  let colorScale := VectorField.ColorScale.viridis
  let coloring : VectorField.Coloring := {
    mode := if state.showMagnitude then
      .magnitude colorScale
    else
      .fixed Color.cyan
  }
  let overlay := fun view maxMag =>
    renderVectorField3DOverlay state view maxMag env.screenScale env.fontMedium env.fontSmall

  let config : VectorField.Config3D := {
    view := viewConfig
    sampling := sampling
    arrows := arrows
    coloring := coloring
    overlay := some overlay
  }

  VectorField.vectorField3D config env.fontSmall (computeFieldVector3D state.fieldType)

end Demos.Linalg
