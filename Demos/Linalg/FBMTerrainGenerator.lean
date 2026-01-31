/-
  FBM Terrain Generator - 3D heightfield from FBM noise.
  Includes redistribution curve, terracing, and rendering toggles.
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
import Linalg.Noise

open Afferent CanvasM Linalg

namespace Demos.Linalg

inductive TerrainSlider where
  | scale
  | height
  | octaves
  | lacunarity
  | persistence
  | power
  | terrace
  deriving BEq, Inhabited

inductive TerrainDrag where
  | none
  | camera
  | slider (which : TerrainSlider)
  deriving BEq, Inhabited

structure FBMTerrainState where
  config : Noise.FractalConfig := {}
  scale : Float := 1.6
  heightScale : Float := 1.4
  power : Float := 1.0
  terraceLevels : Nat := 0
  showWireframe : Bool := true
  showNormals : Bool := false
  showTexture : Bool := true
  cameraYaw : Float := 0.6
  cameraPitch : Float := 0.55
  dragging : TerrainDrag := .none
  lastMouseX : Float := 0.0
  lastMouseY : Float := 0.0
  deriving Inhabited

def fbmTerrainInitialState : FBMTerrainState := {}

structure FBMTerrainSliderLayout where
  x : Float
  y : Float
  width : Float
  height : Float

structure FBMTerrainToggleLayout where
  x : Float
  y : Float
  size : Float

private def panelWidth (screenScale : Float) : Float :=
  270.0 * screenScale

private def panelX (w screenScale : Float) : Float :=
  w - panelWidth screenScale

def fbmTerrainSliderLayout (w h screenScale : Float) (idx : Nat) : FBMTerrainSliderLayout :=
  let startX := panelX w screenScale + 20.0 * screenScale
  let startY := 140.0 * screenScale
  let width := panelWidth screenScale - 40.0 * screenScale
  let height := 8.0 * screenScale
  let spacing := 34.0 * screenScale
  { x := startX, y := startY + idx.toFloat * spacing, width := width, height := height }

def fbmTerrainToggleLayout (w h screenScale : Float) (idx : Nat) : FBMTerrainToggleLayout :=
  let x := panelX w screenScale + 20.0 * screenScale
  let y := 88.0 * screenScale + idx.toFloat * 26.0 * screenScale
  let size := 16.0 * screenScale
  { x := x, y := y, size := size }

private def clamp01 (t : Float) : Float :=
  Float.clamp t 0.0 1.0

private def scaleFromSlider (t : Float) : Float :=
  0.6 + 3.0 * t

private def scaleToSlider (v : Float) : Float :=
  clamp01 ((v - 0.6) / 3.0)

private def heightFromSlider (t : Float) : Float :=
  0.4 + 2.2 * t

private def heightToSlider (v : Float) : Float :=
  clamp01 ((v - 0.4) / 2.2)

private def lacunarityFromSlider (t : Float) : Float :=
  1.5 + 2.2 * t

private def lacunarityToSlider (v : Float) : Float :=
  clamp01 ((v - 1.5) / 2.2)

private def persistenceFromSlider (t : Float) : Float :=
  0.25 + 0.65 * t

private def persistenceToSlider (v : Float) : Float :=
  clamp01 ((v - 0.25) / 0.65)

private def powerFromSlider (t : Float) : Float :=
  0.5 + 2.8 * t

private def powerToSlider (v : Float) : Float :=
  clamp01 ((v - 0.5) / 2.8)

private def terraceFromSlider (t : Float) : Nat :=
  let v := Float.floor (t * 12.0)
  let n := v.toUInt64.toNat
  if n > 12 then 12 else n

private def terraceToSlider (n : Nat) : Float :=
  clamp01 (n.toFloat / 12.0)

private def octavesFromSlider (t : Float) : Nat :=
  let v := Float.floor (1.0 + t * 7.0)
  let n := v.toUInt64.toNat
  if n < 1 then 1 else if n > 8 then 8 else n

private def octavesToSlider (n : Nat) : Float :=
  clamp01 ((n.toFloat - 1.0) / 7.0)

private def sliderLabel (which : TerrainSlider) : String :=
  match which with
  | .scale => "Noise Scale"
  | .height => "Height"
  | .octaves => "Octaves"
  | .lacunarity => "Lacunarity"
  | .persistence => "Persistence"
  | .power => "Redistribute"
  | .terrace => "Terrace"

private def sliderValueLabel (state : FBMTerrainState) (which : TerrainSlider) : String :=
  match which with
  | .scale => formatFloat state.scale
  | .height => formatFloat state.heightScale
  | .octaves => s!"{state.config.octaves}"
  | .lacunarity => formatFloat state.config.lacunarity
  | .persistence => formatFloat state.config.persistence
  | .power => formatFloat state.power
  | .terrace => s!"{state.terraceLevels}"

def fbmTerrainApplySlider (state : FBMTerrainState) (which : TerrainSlider) (t : Float)
    : FBMTerrainState :=
  let t := clamp01 t
  match which with
  | .scale => { state with scale := scaleFromSlider t }
  | .height => { state with heightScale := heightFromSlider t }
  | .octaves => { state with config := { state.config with octaves := octavesFromSlider t } }
  | .lacunarity => { state with config := { state.config with lacunarity := lacunarityFromSlider t } }
  | .persistence => { state with config := { state.config with persistence := persistenceFromSlider t } }
  | .power => { state with power := powerFromSlider t }
  | .terrace => { state with terraceLevels := terraceFromSlider t }

private def fbmTerrainSliderT (state : FBMTerrainState) (which : TerrainSlider) : Float :=
  match which with
  | .scale => scaleToSlider state.scale
  | .height => heightToSlider state.heightScale
  | .octaves => octavesToSlider state.config.octaves
  | .lacunarity => lacunarityToSlider state.config.lacunarity
  | .persistence => persistenceToSlider state.config.persistence
  | .power => powerToSlider state.power
  | .terrace => terraceToSlider state.terraceLevels

private def renderSlider (label value : String) (t : Float) (layout : FBMTerrainSliderLayout)
    (fontSmall : Font) (active : Bool := false) : CanvasM Unit := do
  let t := clamp01 t
  let knobX := layout.x + t * layout.width
  let knobY := layout.y + layout.height / 2.0
  let knobRadius := layout.height * 0.75
  let trackHeight := layout.height * 0.5

  setFillColor (if active then Color.gray 0.7 else Color.gray 0.5)
  fillPath (Afferent.Path.rectangleXYWH layout.x
    (layout.y + layout.height / 2.0 - trackHeight / 2.0)
    layout.width trackHeight)

  setFillColor (if active then Color.yellow else Color.gray 0.85)
  fillPath (Afferent.Path.circle (Point.mk knobX knobY) knobRadius)

  setFillColor (Color.gray 0.75)
  fillTextXY s!"{label}: {value}" layout.x (layout.y - 6.0) fontSmall

private def renderToggle (label : String) (value : Bool) (layout : FBMTerrainToggleLayout)
    (fontSmall : Font) : CanvasM Unit := do
  let box := Afferent.Path.rectangleXYWH layout.x layout.y layout.size layout.size
  setStrokeColor (Color.gray 0.6)
  setLineWidth 1.5
  strokePath box
  if value then
    setFillColor (Color.rgba 0.3 0.9 0.6 1.0)
    fillPath (Afferent.Path.rectangleXYWH (layout.x + 3) (layout.y + 3)
      (layout.size - 6) (layout.size - 6))
  setFillColor (Color.gray 0.8)
  fillTextXY label (layout.x + layout.size + 8.0) (layout.y + layout.size - 3.0) fontSmall

private def terrainColor (n01 : Float) : Color :=
  let t := Float.clamp n01 0.0 1.0
  if t < 0.25 then
    Color.rgba 0.05 0.2 0.45 1.0
  else if t < 0.45 then
    Color.rgba 0.12 0.35 0.22 1.0
  else if t < 0.7 then
    Color.rgba 0.4 0.35 0.22 1.0
  else
    Color.rgba 0.9 0.9 0.95 1.0

private def heightFromNoise (state : FBMTerrainState) (x z : Float) : Float :=
  let n := Noise.fbm3D (x * state.scale) (z * state.scale) 0.0 state.config
  let n := Noise.redistribute n state.power
  let n := if state.terraceLevels > 0 then Noise.terrace n state.terraceLevels else n
  n * state.heightScale

private def fillTriangle (p1 p2 p3 : Float × Float) (color : Color) : CanvasM Unit := do
  setFillColor color
  let path := Afferent.Path.empty
    |>.moveTo (Point.mk p1.1 p1.2)
    |>.lineTo (Point.mk p2.1 p2.2)
    |>.lineTo (Point.mk p3.1 p3.2)
    |>.closePath
  fillPath path

/-- Render FBM terrain. -/
def renderFBMTerrain (state : FBMTerrainState)
    (w h : Float) (screenScale : Float) (fontMedium fontSmall : Font) : CanvasM Unit := do
  let panelW := panelWidth screenScale
  let plotW := w - panelW
  let plotH := h
  let origin : Float × Float := (plotW / 2.0, plotH / 2.0 + 30.0 * screenScale)
  let worldSize := 6.0
  let scale := Float.min plotW plotH / worldSize * 0.45

  setFillColor (Color.gray 0.06)
  fillPath (Afferent.Path.rectangleXYWH 0 0 plotW plotH)

  let res := 32
  let step := worldSize / (res - 1).toFloat
  let mut heights : Array (Array Float) := #[]
  let mut projected : Array (Array (Float × Float)) := #[]
  for i in [:res] do
    let mut hRow : Array Float := #[]
    let mut pRow : Array (Float × Float) := #[]
    for j in [:res] do
      let x := (i.toFloat / (res - 1).toFloat - 0.5) * worldSize
      let z := (j.toFloat / (res - 1).toFloat - 0.5) * worldSize
      let y := heightFromNoise state x z
      hRow := hRow.push y
      let p := rotProject3Dto2D (Vec3.mk x y z) state.cameraYaw state.cameraPitch origin scale
      pRow := pRow.push p
    heights := heights.push hRow
    projected := projected.push pRow

  let heightAt := fun (i j : Nat) =>
    let row := heights.getD i #[]
    row.getD j 0.0

  if state.showTexture then
    for i in [:res - 1] do
      for j in [:res - 1] do
        let p00 := (projected.getD i #[]).getD j (0.0, 0.0)
        let p10 := (projected.getD (i + 1) #[]).getD j (0.0, 0.0)
        let p11 := (projected.getD (i + 1) #[]).getD (j + 1) (0.0, 0.0)
        let p01 := (projected.getD i #[]).getD (j + 1) (0.0, 0.0)
        let hA := heightAt i j
        let hB := heightAt (i + 1) j
        let hC := heightAt (i + 1) (j + 1)
        let hD := heightAt i (j + 1)
        let hNorm := Float.clamp ((hA + hB + hC + hD) / (4.0 * state.heightScale) + 0.5) 0.0 1.0
        let color := terrainColor hNorm
        fillTriangle p00 p10 p11 color
        fillTriangle p00 p11 p01 color

  if state.showWireframe then
    setStrokeColor (Color.gray 0.45)
    setLineWidth (1.0 * screenScale)
    for i in [:res] do
      let mut path := Afferent.Path.empty
      for j in [:res] do
        let p := (projected.getD i #[]).getD j (0.0, 0.0)
        path := if j == 0 then path.moveTo (Point.mk p.1 p.2)
          else path.lineTo (Point.mk p.1 p.2)
      strokePath path
    for j in [:res] do
      let mut path := Afferent.Path.empty
      for i in [:res] do
        let p := (projected.getD i #[]).getD j (0.0, 0.0)
        path := if i == 0 then path.moveTo (Point.mk p.1 p.2)
          else path.lineTo (Point.mk p.1 p.2)
      strokePath path

  if state.showNormals then
    let mut i := 1
    while i + 1 < res do
      let mut j := 1
      while j + 1 < res do
        let hL := heightAt (i - 1) j
        let hR := heightAt (i + 1) j
        let hD := heightAt i (j - 1)
        let hU := heightAt i (j + 1)
        let dx := (hR - hL) / (2.0 * step)
        let dz := (hU - hD) / (2.0 * step)
        let normal := Vec3.mk (-dx) 1.0 (-dz) |>.normalize
        let x := (i.toFloat / (res - 1).toFloat - 0.5) * worldSize
        let z := (j.toFloat / (res - 1).toFloat - 0.5) * worldSize
        let y := heightAt i j
        let start := rotProject3Dto2D (Vec3.mk x y z) state.cameraYaw state.cameraPitch origin scale
        let endPt := rotProject3Dto2D (Vec3.mk x y z + normal.scale 0.6)
          state.cameraYaw state.cameraPitch origin scale
        drawArrow2D start endPt { color := Color.rgba 0.9 0.8 0.2 0.7, lineWidth := 1.2 * screenScale }
        j := j + 3
      i := i + 3

  setStrokeColor (Color.gray 0.35)
  setLineWidth 1.0
  strokePath (Afferent.Path.rectangleXYWH 0 0 plotW plotH)

  let pX := panelX w screenScale
  setFillColor (Color.rgba 0.08 0.08 0.1 0.95)
  fillPath (Afferent.Path.rectangleXYWH pX 0 panelW h)

  setFillColor VecColor.label
  fillTextXY "FBM TERRAIN" (pX + 20 * screenScale) (36 * screenScale) fontMedium
  setFillColor (Color.gray 0.6)
  fillTextXY "fbm3D + redistribute + terrace" (pX + 20 * screenScale) (58 * screenScale) fontSmall

  let toggleA := fbmTerrainToggleLayout w h screenScale 0
  let toggleB := fbmTerrainToggleLayout w h screenScale 1
  let toggleC := fbmTerrainToggleLayout w h screenScale 2
  renderToggle "Wireframe" state.showWireframe toggleA fontSmall
  renderToggle "Texture" state.showTexture toggleB fontSmall
  renderToggle "Normals" state.showNormals toggleC fontSmall

  let sliders : Array TerrainSlider := #[
    .scale, .height, .octaves, .lacunarity, .persistence, .power, .terrace
  ]
  for i in [:sliders.size] do
    let which := sliders.getD i .scale
    let layout := fbmTerrainSliderLayout w h screenScale i
    let active := match state.dragging with
      | .slider s => s == which
      | _ => false
    let t := fbmTerrainSliderT state which
    renderSlider (sliderLabel which) (sliderValueLabel state which) t layout fontSmall active

  setFillColor (Color.gray 0.6)
  fillTextXY "Right-drag: rotate" (pX + 20 * screenScale)
    (h - 50 * screenScale) fontSmall
  fillTextXY "R: reset" (pX + 20 * screenScale) (h - 30 * screenScale) fontSmall

/-- Create the FBM terrain widget. -/
def fbmTerrainWidget (env : DemoEnv) (state : FBMTerrainState)
    : Afferent.Arbor.WidgetBuilder := do
  Afferent.Arbor.custom (spec := {
    measure := fun _ _ => (0, 0)
    collect := fun _ => #[]
    draw := some (fun layout => do
      withContentRect layout fun w h => do
        resetTransform
        renderFBMTerrain state w h env.screenScale env.fontMedium env.fontSmall
    )
  }) (style := { flexItem := some (Trellis.FlexItem.growing 1) })

end Demos.Linalg
