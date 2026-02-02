/-
  Particle Integration Comparison Demo - compare integration schemes and energy drift.
-/
import Afferent
import Afferent.Widget
import Afferent.Arbor
import Demos.Core.Demo
import Demos.Linalg.Shared
import Trellis
import Linalg.Core
import Linalg.Vec2
import Linalg.Vec3
import Linalg.Physics

open Afferent CanvasM Linalg
open Afferent.Widget

namespace Demos.Linalg

/-- Presets for particle integration comparison. -/
inductive IntegrationPreset where
  | harmonic
  | orbit
  | projectile
  deriving BEq, Inhabited

/-- Integration modes to compare. -/
inductive IntegrationMode where
  | euler
  | semiImplicit
  | verlet
  | rk4
  deriving BEq, Inhabited

/-- State for a single integrator. -/
structure IntegratorState where
  mode : IntegrationMode
  position : Vec3
  velocity : Vec3
  prevPosition : Vec3
  energyHistory : Array Float
  initialEnergy : Float

instance : Inhabited IntegratorState where
  default := {
    mode := .euler
    position := Vec3.zero
    velocity := Vec3.zero
    prevPosition := Vec3.zero
    energyHistory := #[]
    initialEnergy := 0.0
  }

/-- State for integration comparison demo. -/
structure ParticleIntegrationComparisonState where
  preset : IntegrationPreset := .harmonic
  states : Array IntegratorState := #[]
  time : Float := 0.0
  animating : Bool := true
  speed : Float := 1.0
  deriving Inhabited

private def presetName : IntegrationPreset → String
  | .harmonic => "Harmonic"
  | .orbit => "Orbit"
  | .projectile => "Projectile"

private def modeName : IntegrationMode → String
  | .euler => "Euler"
  | .semiImplicit => "Semi-Implicit"
  | .verlet => "Verlet"
  | .rk4 => "RK4"

private def modeColor : IntegrationMode → Color
  | .euler => Color.rgba 1.0 0.4 0.4 1.0
  | .semiImplicit => Color.rgba 0.3 0.9 0.5 1.0
  | .verlet => Color.rgba 0.3 0.6 1.0 1.0
  | .rk4 => Color.rgba 1.0 0.7 0.2 1.0

private def presetInitial (preset : IntegrationPreset) : Vec3 × Vec3 :=
  match preset with
  | .harmonic => (Vec3.mk 2.6 0.0 0.0, Vec3.mk 0.0 1.6 0.0)
  | .orbit => (Vec3.mk 3.0 0.0 0.0, Vec3.mk 0.0 1.25 0.0)
  | .projectile => (Vec3.mk (-3.5) (-2.0) 0.0, Vec3.mk 4.0 4.4 0.0)

private def presetAcceleration (preset : IntegrationPreset) (pos : Vec3) (_vel : Vec3) : Vec3 :=
  match preset with
  | .harmonic =>
      let k := 2.0
      pos.scale (-k)
  | .orbit =>
      let mu := 3.0
      let r := pos.length
      if r < 0.2 then Vec3.zero
      else
        let invR3 := 1.0 / (r * r * r)
        pos.scale (-mu * invR3)
  | .projectile =>
      Vec3.mk 0.0 (-3.0) 0.0

private def presetPotential (preset : IntegrationPreset) (pos : Vec3) : Float :=
  match preset with
  | .harmonic =>
      let k := 2.0
      0.5 * k * pos.lengthSquared
  | .orbit =>
      let mu := 3.0
      let r := Float.max (Vec3.length pos) 0.2
      let potential := (-mu) / r
      potential
  | .projectile =>
      let g := 3.0
      g * pos.y

private def energyFor (preset : IntegrationPreset) (pos vel : Vec3) : Float :=
  0.5 * vel.lengthSquared + presetPotential preset pos

private def baseDt : Float := 1.0 / 60.0

private def makeIntegrator (preset : IntegrationPreset) (mode : IntegrationMode) : IntegratorState :=
  let (pos, vel) := presetInitial preset
  let energy := energyFor preset pos vel
  let prev := pos.sub (vel.scale baseDt)
  { mode := mode
    position := pos
    velocity := vel
    prevPosition := prev
    energyHistory := #[0.0]
    initialEnergy := energy }

private def initialIntegratorStates (preset : IntegrationPreset) : Array IntegratorState :=
  #[makeIntegrator preset .euler,
    makeIntegrator preset .semiImplicit,
    makeIntegrator preset .verlet,
    makeIntegrator preset .rk4]

/-- Reset state for a given preset. -/
def particleIntegrationComparisonStateFor (preset : IntegrationPreset) : ParticleIntegrationComparisonState :=
  { preset := preset
    states := initialIntegratorStates preset
    time := 0.0
    animating := true
    speed := 1.0 }

/-- Initial state. -/
def particleIntegrationComparisonInitialState : ParticleIntegrationComparisonState :=
  particleIntegrationComparisonStateFor .harmonic

def particleIntegrationMathViewConfig (screenScale : Float) : MathView2D.Config := {
  style := { flexItem := some (Trellis.FlexItem.growing 1) }
  scale := 70.0 * screenScale
  minorStep := 1.0
  majorStep := 2.0
  gridMinorColor := Color.gray 0.2
  gridMajorColor := Color.gray 0.4
  axisColor := Color.gray 0.6
  labelColor := VecColor.label
  labelPrecision := 0
}

private def pushEnergyHistory (state : IntegratorState) (energy : Float) : Array Float :=
  let diff := energy - state.initialEnergy
  let history := state.energyHistory.push diff
  if history.size > 140 then history.eraseIdxIfInBounds 0 else history

private def stepIntegrator (preset : IntegrationPreset) (dt : Float) (state : IntegratorState) : IntegratorState :=
  if dt <= 0.0 then state
  else
    let accel := presetAcceleration preset state.position state.velocity
    match state.mode with
    | .euler =>
        let p := Particle.create state.position state.velocity accel 1.0
        let p' := Integration.eulerStep p dt
        let energy := energyFor preset p'.position p'.velocity
        { state with
          position := p'.position
          velocity := p'.velocity
          energyHistory := pushEnergyHistory state energy }
    | .semiImplicit =>
        let p := Particle.create state.position state.velocity accel 1.0
        let p' := Integration.semiImplicitEulerStep p dt
        let energy := energyFor preset p'.position p'.velocity
        { state with
          position := p'.position
          velocity := p'.velocity
          energyHistory := pushEnergyHistory state energy }
    | .verlet =>
        let newPos := Integration.verletStep state.position state.prevPosition accel dt
        let newVel := newPos.sub state.prevPosition |>.scale (0.5 / dt)
        let energy := energyFor preset newPos newVel
        { state with
          position := newPos
          velocity := newVel
          prevPosition := state.position
          energyHistory := pushEnergyHistory state energy }
    | .rk4 =>
        let (newPos, newVel) := Integration.rk4Step state.position state.velocity
          (fun p v => presetAcceleration preset p v) dt
        let energy := energyFor preset newPos newVel
        { state with
          position := newPos
          velocity := newVel
          energyHistory := pushEnergyHistory state energy }

/-- Step simulation for all integrators. -/
def stepParticleIntegrationComparison (state : ParticleIntegrationComparisonState) (dt : Float)
    : ParticleIntegrationComparisonState :=
  let dt' := Float.min dt 0.033
  let scaledDt := dt' * state.speed
  let newStates := state.states.map (stepIntegrator state.preset scaledDt)
  { state with states := newStates, time := state.time + dt' }

private def drawEnergyGraph (states : Array IntegratorState)
    (x y w h : Float) (font : Font) : CanvasM Unit := do
  setStrokeColor (Color.gray 0.4)
  setLineWidth 1.0
  strokePath (Afferent.Path.rectangleXYWH x y w h)

  let mut minVal := 0.0
  let mut maxVal := 0.0
  for s in states do
    for v in s.energyHistory do
      minVal := Float.min minVal v
      maxVal := Float.max maxVal v

  let range := Float.max (maxVal - minVal) 0.001
  let zeroY := y + h - ((0.0 - minVal) / range) * h
  setStrokeColor (Color.gray 0.5)
  setLineWidth 1.0
  let zeroPath := Afferent.Path.empty
    |>.moveTo (Point.mk x zeroY)
    |>.lineTo (Point.mk (x + w) zeroY)
  strokePath zeroPath

  for s in states do
    if s.energyHistory.size > 1 then
      let count := s.energyHistory.size
      let stepX := w / (count - 1).toFloat
      let mut path := Afferent.Path.empty
      for i in [:count] do
        let v := s.energyHistory[i]!
        let px := x + stepX * i.toFloat
        let py := y + h - ((v - minVal) / range) * h
        if i == 0 then
          path := path.moveTo (Point.mk px py)
        else
          path := path.lineTo (Point.mk px py)
      setStrokeColor (modeColor s.mode)
      setLineWidth 1.6
      strokePath path

  setFillColor (Color.gray 0.7)
  fillTextXY "Energy drift" (x + 8) (y + 16) font

/-- Render the integration comparison visualization. -/
def renderParticleIntegrationComparison (state : ParticleIntegrationComparisonState)
    (view : MathView2D.View) (screenScale : Float) (fontMedium fontSmall : Font) : CanvasM Unit := do
  let w := view.width
  let h := view.height
  let origin : Float × Float := (view.origin.x, view.origin.y)
  let scale := view.scale

  for s in state.states do
    let pos2 := Vec2.mk s.position.x s.position.y
    drawMarker pos2 origin scale (modeColor s.mode) 8.0
    let vel2 := Vec2.mk s.velocity.x s.velocity.y
    drawVectorArrow pos2 vel2 origin scale { color := modeColor s.mode, lineWidth := 2.0 }

  let graphW := 260.0 * screenScale
  let graphH := 120.0 * screenScale
  let graphX := w - graphW - 20.0 * screenScale
  let graphY := 80.0 * screenScale
  drawEnergyGraph state.states graphX graphY graphW graphH fontSmall

  let infoY := h - 130 * screenScale
  setFillColor VecColor.label
  fillTextXY s!"Preset: {presetName state.preset}" (20 * screenScale) infoY fontSmall
  fillTextXY s!"Speed: {formatFloat state.speed}" (20 * screenScale) (infoY + 20 * screenScale) fontSmall

  let mut legendY := infoY + 50 * screenScale
  for s in state.states do
    setFillColor (modeColor s.mode)
    fillTextXY (modeName s.mode) (20 * screenScale) legendY fontSmall
    legendY := legendY + 18 * screenScale

  fillTextXY "PARTICLE INTEGRATION COMPARISON" (20 * screenScale) (30 * screenScale) fontMedium
  setFillColor (Color.gray 0.6)
  let animText := if state.animating then "animating" else "paused"
  fillTextXY
    s!"Space: {animText} | 1: harmonic | 2: orbit | 3: projectile | +/- speed"
    (20 * screenScale) (55 * screenScale) fontSmall

/-- Create the integration comparison widget. -/
def particleIntegrationComparisonWidget (env : DemoEnv) (state : ParticleIntegrationComparisonState)
    : Afferent.Arbor.WidgetBuilder := do
  let config := particleIntegrationMathViewConfig env.screenScale
  MathView2D.mathView2D config env.fontSmall (fun view => do
    renderParticleIntegrationComparison state view env.screenScale env.fontMedium env.fontSmall
  )

end Demos.Linalg
