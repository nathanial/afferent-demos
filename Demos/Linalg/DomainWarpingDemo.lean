/-
  Domain Warping Demo - Before/after comparison of warped noise.
  Shows warp vectors and animated evolution.
-/
import Afferent
import Afferent.Widget
import Afferent.Arbor
import Demos.Core.Demo
import Demos.Linalg.Shared
import Trellis
import Linalg.Core
import Linalg.Vec2
import Linalg.Noise

open Afferent CanvasM Linalg

namespace Demos.Linalg

inductive WarpingSlider where
  | strength1
  | strength2
  | scale
  | speed
  deriving BEq, Inhabited

inductive WarpingDrag where
  | none
  | slider (which : WarpingSlider)
  deriving BEq, Inhabited

structure DomainWarpingState where
  strength1 : Float := 3.0
  strength2 : Float := 4.5
  scale : Float := 2.3
  speed : Float := 0.25
  animate : Bool := true
  showVectors : Bool := true
  useAdvanced : Bool := false
  time : Float := 0.0
  lastTime : Float := 0.0
  dragging : WarpingDrag := .none
  deriving Inhabited

def domainWarpingInitialState : DomainWarpingState := {}

structure DomainWarpingSliderLayout where
  x : Float
  y : Float
  width : Float
  height : Float

structure DomainWarpingToggleLayout where
  x : Float
  y : Float
  size : Float

private def panelWidth (screenScale : Float) : Float :=
  250.0 * screenScale

private def panelX (w screenScale : Float) : Float :=
  w - panelWidth screenScale

def domainWarpingSliderLayout (w h screenScale : Float) (idx : Nat) : DomainWarpingSliderLayout :=
  let startX := panelX w screenScale + 20.0 * screenScale
  let startY := 120.0 * screenScale
  let width := panelWidth screenScale - 40.0 * screenScale
  let height := 8.0 * screenScale
  let spacing := 34.0 * screenScale
  { x := startX, y := startY + idx.toFloat * spacing, width := width, height := height }

def domainWarpingToggleLayout (w h screenScale : Float) (idx : Nat) : DomainWarpingToggleLayout :=
  let x := panelX w screenScale + 20.0 * screenScale
  let y := 70.0 * screenScale + idx.toFloat * 26.0 * screenScale
  let size := 16.0 * screenScale
  { x := x, y := y, size := size }

private def clamp01 (t : Float) : Float :=
  Float.clamp t 0.0 1.0

private def strengthFromSlider (t : Float) : Float :=
  0.5 + 7.5 * t

private def strengthToSlider (v : Float) : Float :=
  clamp01 ((v - 0.5) / 7.5)

private def scaleFromSlider (t : Float) : Float :=
  0.6 + 4.4 * t

private def scaleToSlider (v : Float) : Float :=
  clamp01 ((v - 0.6) / 4.4)

private def speedFromSlider (t : Float) : Float :=
  0.05 + 0.9 * t

private def speedToSlider (v : Float) : Float :=
  clamp01 ((v - 0.05) / 0.9)

private def sliderLabel (which : WarpingSlider) : String :=
  match which with
  | .strength1 => "Strength 1"
  | .strength2 => "Strength 2"
  | .scale => "Scale"
  | .speed => "Speed"

private def sliderValueLabel (state : DomainWarpingState) (which : WarpingSlider) : String :=
  match which with
  | .strength1 => formatFloat state.strength1
  | .strength2 => formatFloat state.strength2
  | .scale => formatFloat state.scale
  | .speed => formatFloat state.speed

def domainWarpingApplySlider (state : DomainWarpingState) (which : WarpingSlider) (t : Float)
    : DomainWarpingState :=
  let t := clamp01 t
  match which with
  | .strength1 => { state with strength1 := strengthFromSlider t }
  | .strength2 => { state with strength2 := strengthFromSlider t }
  | .scale => { state with scale := scaleFromSlider t }
  | .speed => { state with speed := speedFromSlider t }

private def domainWarpingSliderT (state : DomainWarpingState) (which : WarpingSlider) : Float :=
  match which with
  | .strength1 => strengthToSlider state.strength1
  | .strength2 => strengthToSlider state.strength2
  | .scale => scaleToSlider state.scale
  | .speed => speedToSlider state.speed

private def renderSlider (label value : String) (t : Float) (layout : DomainWarpingSliderLayout)
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

private def renderToggle (label : String) (value : Bool) (layout : DomainWarpingToggleLayout)
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

private def renderNoisePanel (x y w h : Float) (label : String)
    (state : DomainWarpingState) (time : Float)
    (screenScale : Float) (fontSmall : Font) (useWarp : Bool) : CanvasM Unit := do
  setFillColor (Color.gray 0.08)
  fillPath (Afferent.Path.rectangleXYWH x y w h)

  let cellSize := 6.0 * screenScale
  let resX := Float.floor (w / cellSize)
  let resY := Float.floor (h / cellSize)
  let res := Nat.max 28 (Nat.min resX.toUInt64.toNat resY.toUInt64.toNat)
  let cellW := w / res.toFloat
  let cellH := h / res.toFloat

  for yi in [:res] do
    for xi in [:res] do
      let u := xi.toFloat / res.toFloat - 0.5
      let v := yi.toFloat / res.toFloat - 0.5
      let sx := u * state.scale + time * 0.2
      let sy := v * state.scale + time * 0.15
      let n := if useWarp then
        if state.useAdvanced then
          Noise.warp2DAdvanced sx sy state.strength1 state.strength2
        else
          Noise.warp2D sx sy state.strength1
      else
        Noise.fbm2D sx sy
      let n01 := Noise.normalize n
      setFillColor (Color.gray n01)
      fillPath (Afferent.Path.rectangleXYWH (x + xi.toFloat * cellW) (y + yi.toFloat * cellH)
        (cellW + 0.5) (cellH + 0.5))

  setStrokeColor (Color.gray 0.35)
  setLineWidth 1.0
  strokePath (Afferent.Path.rectangleXYWH x y w h)

  setFillColor (Color.gray 0.8)
  fillTextXY label (x + 8.0) (y + 18.0) fontSmall

private def renderWarpVectors (x y w h : Float) (state : DomainWarpingState) (time : Float)
    (screenScale : Float) : CanvasM Unit := do
  let grid := 8
  let stepX := w / grid.toFloat
  let stepY := h / grid.toFloat
  for i in [:grid + 1] do
    for j in [:grid + 1] do
      let px := x + i.toFloat * stepX
      let py := y + j.toFloat * stepY
      let u := (px - x) / w - 0.5
      let v := (py - y) / h - 0.5
      let sx := u * state.scale + time * 0.2
      let sy := v * state.scale + time * 0.15
      let qx := Noise.fbm2D sx sy
      let qy := Noise.fbm2D (sx + 5.2) (sy + 1.3)
      let vec := Vec2.mk qx qy * (state.strength1 * 0.35)
      let endX := px + vec.x * stepX
      let endY := py - vec.y * stepY
      drawArrow2D (px, py) (endX, endY) {
        color := Color.rgba 0.9 0.7 0.2 0.7
        lineWidth := 1.2 * screenScale
        headLength := 6.0 * screenScale
        headAngle := 0.6
      }

/-- Render domain warping demo. -/
def renderDomainWarpingDemo (state : DomainWarpingState)
    (w h : Float) (screenScale : Float) (fontMedium fontSmall : Font) : CanvasM Unit := do
  let panelW := panelWidth screenScale
  let contentW := w - panelW
  let pad := 18.0 * screenScale
  let viewW := (contentW - pad * 3.0) / 2.0
  let viewH := h - pad * 2.0
  let leftX := pad
  let rightX := pad * 2.0 + viewW
  let viewY := pad
  let t := state.time

  renderNoisePanel leftX viewY viewW viewH "Base FBM" state t screenScale fontSmall false
  renderNoisePanel rightX viewY viewW viewH "Warped" state t screenScale fontSmall true

  if state.showVectors then
    renderWarpVectors leftX viewY viewW viewH state t screenScale

  let pX := panelX w screenScale
  setFillColor (Color.rgba 0.08 0.08 0.1 0.95)
  fillPath (Afferent.Path.rectangleXYWH pX 0 panelW h)

  setFillColor VecColor.label
  fillTextXY "DOMAIN WARPING" (pX + 20 * screenScale) (36 * screenScale) fontMedium
  setFillColor (Color.gray 0.6)
  fillTextXY "warp2D / warp2DAdvanced" (pX + 20 * screenScale) (58 * screenScale) fontSmall

  let toggleA := domainWarpingToggleLayout w h screenScale 0
  let toggleB := domainWarpingToggleLayout w h screenScale 1
  let toggleC := domainWarpingToggleLayout w h screenScale 2
  renderToggle "Advanced" state.useAdvanced toggleA fontSmall
  renderToggle "Animate" state.animate toggleB fontSmall
  renderToggle "Show Vectors" state.showVectors toggleC fontSmall

  let sliders : Array WarpingSlider := #[.strength1, .strength2, .scale, .speed]
  for i in [:sliders.size] do
    let which := sliders.getD i .strength1
    let layout := domainWarpingSliderLayout w h screenScale i
    let active := match state.dragging with
      | .slider s => s == which
      | _ => false
    let t := domainWarpingSliderT state which
    renderSlider (sliderLabel which) (sliderValueLabel state which) t layout fontSmall active

  setFillColor (Color.gray 0.6)
  fillTextXY "R: reset" (pX + 20 * screenScale) (h - 30 * screenScale) fontSmall

/-- Create the domain warping widget. -/
def domainWarpingDemoWidget (env : DemoEnv) (state : DomainWarpingState)
    : Afferent.Arbor.WidgetBuilder := do
  Afferent.Arbor.custom (spec := {
    measure := fun _ _ => (0, 0)
    collect := fun _ => #[]
    draw := some (fun layout => do
      withContentRect layout fun w h => do
        resetTransform
        renderDomainWarpingDemo state w h env.screenScale env.fontMedium env.fontSmall
    )
  }) (style := { flexItem := some (Trellis.FlexItem.growing 1) })

end Demos.Linalg
