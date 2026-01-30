/-
  Spring Animation Playground - damped harmonic oscillator visualization.
-/
import Afferent
import Afferent.Widget
import Afferent.Arbor
import Demos.Core.Demo
import Demos.Linalg.Shared
import Trellis
import Linalg.Core

open Afferent CanvasM Linalg

namespace Demos.Linalg

/-- Drag state for spring playground. -/
inductive SpringDragMode where
  | none
  | sliderDamping
  | sliderFrequency
  deriving BEq, Inhabited

/-- Slider layout parameters. -/
structure SpringSliderLayout where
  x : Float
  y : Float
  width : Float
  height : Float

/-- Simple rectangle helper. -/
structure SpringRect where
  x : Float
  y : Float
  w : Float
  h : Float

/-- Compute slider geometry for spring sliders. -/
def springSliderLayout (w _h screenScale : Float) (idx : Nat) : SpringSliderLayout :=
  let startX := w - 260.0 * screenScale
  let startY := 95.0 * screenScale
  let width := 190.0 * screenScale
  let height := 8.0 * screenScale
  let spacing := 32.0 * screenScale
  { x := startX, y := startY + idx.toFloat * spacing, width := width, height := height }

/-- Map slider t to damping ratio. -/
def springDampingFrom (t : Float) : Float :=
  0.05 + Linalg.Float.clamp t 0.0 1.0 * 1.95

/-- Map damping ratio to slider t. -/
def springDampingTo (value : Float) : Float :=
  Linalg.Float.clamp ((value - 0.05) / 1.95) 0.0 1.0

/-- Map slider t to frequency (Hz). -/
def springFrequencyFrom (t : Float) : Float :=
  0.5 + Linalg.Float.clamp t 0.0 1.0 * 2.5

/-- Map frequency to slider t. -/
def springFrequencyTo (value : Float) : Float :=
  Linalg.Float.clamp ((value - 0.5) / 2.5) 0.0 1.0

/-- State for spring playground. -/
structure SpringAnimationPlaygroundState where
  time : Float := 0.0
  dampingRatio : Float := 0.35
  frequency : Float := 1.4
  animating : Bool := true
  dragging : SpringDragMode := .none
  energyHistory : Array Float := #[]
  deriving Inhabited


def springAnimationPlaygroundInitialState : SpringAnimationPlaygroundState := {}

private def clamp01 (t : Float) : Float :=
  Linalg.Float.clamp t 0.0 1.0

private def expf (x : Float) : Float :=
  Float.exp x

private def cosh (x : Float) : Float :=
  (expf x + expf (-x)) / 2.0

private def sinh (x : Float) : Float :=
  (expf x - expf (-x)) / 2.0

/-- Spring response for x(0)=1, v(0)=0. -/
def springResponse (t ζ ω : Float) : Float :=
  if ζ < 1.0 then
    let wd := ω * Float.sqrt (1.0 - ζ * ζ)
    let coeff := ζ / Float.sqrt (1.0 - ζ * ζ)
    expf (-ζ * ω * t) * (Float.cos (wd * t) + coeff * Float.sin (wd * t))
  else if ζ == 1.0 then
    expf (-ω * t) * (1.0 + ω * t)
  else
    let r := Float.sqrt (ζ * ζ - 1.0)
    expf (-ζ * ω * t) * (cosh (ω * r * t) + (ζ / r) * sinh (ω * r * t))

/-- Approximate velocity via finite difference. -/
def springVelocity (t ζ ω : Float) : Float :=
  let dt := 0.001
  let x1 := springResponse (t + dt) ζ ω
  let x0 := springResponse (t - dt) ζ ω
  (x1 - x0) / (2.0 * dt)

private def renderSlider (label : String) (value : Float) (layout : SpringSliderLayout)
    (fontSmall : Font) (active : Bool := false) : CanvasM Unit := do
  let t := clamp01 value
  let knobX := layout.x + t * layout.width
  let knobY := layout.y + layout.height / 2.0
  let knobRadius := layout.height * 0.75
  let trackHeight := layout.height * 0.5

  setFillColor (if active then Color.gray 0.7 else Color.gray 0.5)
  fillPath (Afferent.Path.rectangleXYWH layout.x (layout.y + layout.height / 2.0 - trackHeight / 2.0)
    layout.width trackHeight)

  setFillColor (if active then Color.yellow else Color.gray 0.8)
  fillPath (Afferent.Path.circle (Point.mk knobX knobY) knobRadius)

  setFillColor (Color.gray 0.8)
  fillTextXY label (layout.x - 74.0) (layout.y + 6.0) fontSmall

private def drawGraph (values : Array Float) (rect : SpringRect) (color : Color)
    (maxValue : Float) : CanvasM Unit := do
  if values.size < 2 then return
  let mut path := Afferent.Path.empty
  for i in [:values.size] do
    let t := i.toFloat / (values.size - 1).toFloat
    let v := values.getD i 0.0
    let y := rect.y + rect.h - (v / maxValue) * rect.h
    let x := rect.x + t * rect.w
    if i == 0 then
      path := path.moveTo (Point.mk x y)
    else
      path := path.lineTo (Point.mk x y)
  setStrokeColor color
  setLineWidth 1.6
  strokePath path

/-- Render spring playground. -/
def renderSpringAnimationPlayground (state : SpringAnimationPlaygroundState)
    (w h : Float) (screenScale : Float) (fontMedium fontSmall : Font) : CanvasM Unit := do
  let ω := 2.0 * Linalg.Float.pi * state.frequency
  let x := springResponse state.time state.dampingRatio ω
  let _v := springVelocity state.time state.dampingRatio ω
  let envelope := Float.exp (-state.dampingRatio * ω * state.time)

  -- Oscillator line
  let midY := h / 2 + 20 * screenScale
  let leftX := 80.0 * screenScale
  let rightX := w - 80.0 * screenScale
  setStrokeColor (Color.gray 0.4)
  setLineWidth 1.2
  strokePath (Afferent.Path.empty
    |>.moveTo (Point.mk leftX midY)
    |>.lineTo (Point.mk rightX midY))

  let amp := (rightX - leftX) / 3.5
  let px := (w / 2) + x * amp
  setFillColor (Color.rgba 0.2 0.9 1.0 0.9)
  fillPath (Afferent.Path.circle (Point.mk px midY) 10.0)

  -- Envelope
  let envY := 120.0 * screenScale
  let envRect : SpringRect := SpringRect.mk leftX envY (rightX - leftX) (90.0 * screenScale)
  setStrokeColor (Color.gray 0.35)
  setLineWidth 1.0
  strokePath (Afferent.Path.rectangleXYWH envRect.x envRect.y envRect.w envRect.h)

  let samples := 80
  let mut envPathTop := Afferent.Path.empty
  let mut envPathBot := Afferent.Path.empty
  for i in [:samples] do
    let t := i.toFloat / (samples - 1).toFloat
    let env := Float.exp (-state.dampingRatio * ω * t * 2.0)
    let yTop := envRect.y + (1.0 - env) * envRect.h
    let yBot := envRect.y + env * envRect.h
    let xPos := envRect.x + t * envRect.w
    if i == 0 then
      envPathTop := envPathTop.moveTo (Point.mk xPos yTop)
      envPathBot := envPathBot.moveTo (Point.mk xPos yBot)
    else
      envPathTop := envPathTop.lineTo (Point.mk xPos yTop)
      envPathBot := envPathBot.lineTo (Point.mk xPos yBot)
  setStrokeColor (Color.rgba 1.0 0.7 0.3 0.8)
  setLineWidth 1.2
  strokePath envPathTop
  strokePath envPathBot

  -- Energy graph
  let energyRect : SpringRect := SpringRect.mk (40.0 * screenScale) (h - 150.0 * screenScale)
    (w - 80.0 * screenScale) (90.0 * screenScale)
  setStrokeColor (Color.gray 0.35)
  setLineWidth 1.0
  strokePath (Afferent.Path.rectangleXYWH energyRect.x energyRect.y energyRect.w energyRect.h)
  let maxEnergy := 1.2
  drawGraph state.energyHistory energyRect (Color.rgba 0.4 0.9 0.4 0.9) maxEnergy

  -- Sliders
  let layoutDamp := springSliderLayout w h screenScale 0
  let layoutFreq := springSliderLayout w h screenScale 1
  let tDamp := springDampingTo state.dampingRatio
  let tFreq := springFrequencyTo state.frequency
  let activeDamp := match state.dragging with | .sliderDamping => true | _ => false
  let activeFreq := match state.dragging with | .sliderFrequency => true | _ => false
  renderSlider "damping" tDamp layoutDamp fontSmall activeDamp
  renderSlider "frequency" tFreq layoutFreq fontSmall activeFreq

  -- Labels
  setFillColor VecColor.label
  fillTextXY "SPRING ANIMATION PLAYGROUND" (20 * screenScale) (30 * screenScale) fontMedium
  setFillColor (Color.gray 0.6)
  fillTextXY "Space: pause | R: reset" (20 * screenScale) (55 * screenScale) fontSmall
  setFillColor VecColor.label
  fillTextXY s!"damping={formatFloat state.dampingRatio}  freq={formatFloat state.frequency}Hz  env={formatFloat envelope}"
    (20 * screenScale) (h - 40 * screenScale) fontSmall

/-- Create spring playground widget. -/
def springAnimationPlaygroundWidget (env : DemoEnv) (state : SpringAnimationPlaygroundState)
    : Afferent.Arbor.WidgetBuilder := do
  Afferent.Arbor.custom (spec := {
    measure := fun _ _ => (0, 0)
    collect := fun _ => #[]
    draw := some (fun layout => do
      withContentRect layout fun w h => do
        resetTransform
        renderSpringAnimationPlayground state w h env.screenScale env.fontMedium env.fontSmall
    )
  }) (style := { flexItem := some (Trellis.FlexItem.growing 1) })

end Demos.Linalg
