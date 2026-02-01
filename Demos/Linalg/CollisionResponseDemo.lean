/-
  Collision Response Demo - impulse-based particle collision with restitution and friction.
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

namespace Demos.Linalg

/-- Slider types for collision demo. -/
inductive CollisionSlider where
  | restitution
  | friction
  deriving BEq, Inhabited

/-- Collision snapshot for visualization. -/
structure CollisionSnapshot where
  point : Vec3
  normal : Vec3
  impulse : Vec3
  frictionImpulse : Vec3
  preVelA : Vec3
  preVelB : Vec3
  postVelA : Vec3
  postVelB : Vec3
  deriving Inhabited

/-- State for collision response demo. -/
structure CollisionResponseDemoState where
  particleA : Particle
  particleB : Particle
  radius : Float := 0.6
  restitution : Float := 0.6
  friction : Float := 0.25
  dragging : Option CollisionSlider := none
  animating : Bool := true
  time : Float := 0.0
  lastCollision : Option CollisionSnapshot := none
  deriving Inhabited

private def defaultParticles : Particle × Particle :=
  let a := Particle.create (Vec3.mk (-3.0) 0.4 0.0) (Vec3.mk 2.6 0.3 0.0) Vec3.zero 1.0
  let b := Particle.create (Vec3.mk (2.6) (-0.2) 0.0) (Vec3.mk (-2.0) (-0.1) 0.0) Vec3.zero 1.0
  (a, b)

/-- Initial state. -/
def collisionResponseDemoInitialState : CollisionResponseDemoState :=
  let (a, b) := defaultParticles
  { particleA := a, particleB := b }

private structure SliderLayout where
  x : Float
  y : Float
  width : Float
  height : Float

private def sliderLayout (w _h screenScale : Float) (idx : Nat) : SliderLayout :=
  let width := 220 * screenScale
  let height := 16 * screenScale
  let x := w - width - 30 * screenScale
  let y := 90 * screenScale + idx.toFloat * 36 * screenScale
  { x := x, y := y, width := width, height := height }

private def drawSlider (layout : SliderLayout) (value : Float) (label : String)
    (font : Font) : CanvasM Unit := do
  setStrokeColor (Color.gray 0.5)
  setLineWidth 1.5
  let path := Afferent.Path.empty
    |>.moveTo (Point.mk layout.x (layout.y + layout.height / 2))
    |>.lineTo (Point.mk (layout.x + layout.width) (layout.y + layout.height / 2))
  strokePath path

  let t := Float.clamp value 0.0 1.0
  let knobX := layout.x + t * layout.width
  setFillColor (Color.gray 0.9)
  fillPath (Afferent.Path.circle (Point.mk knobX (layout.y + layout.height / 2)) (6.0))

  setFillColor (Color.gray 0.7)
  fillTextXY s!"{label}: {formatFloat value}" layout.x (layout.y - 8.0) font

private def drawCircleWorld (center : Vec2) (radius : Float) (origin : Float × Float)
    (scale : Float) (stroke : Color) (fill : Option Color := none) (lineWidth : Float := 2.0) : CanvasM Unit := do
  let (sx, sy) := worldToScreen center origin scale
  let sr := radius * scale
  match fill with
  | some c =>
      setFillColor c
      fillPath (Afferent.Path.circle (Point.mk sx sy) sr)
  | none => pure ()
  setStrokeColor stroke
  setLineWidth lineWidth
  strokePath (Afferent.Path.circle (Point.mk sx sy) sr)

private def drawArrowWorld (start finish : Vec2) (origin : Float × Float) (scale : Float)
    (color : Color) (lineWidth : Float := 2.0) : CanvasM Unit := do
  let s := worldToScreen start origin scale
  let f := worldToScreen finish origin scale
  drawArrow2D s f { color := color, lineWidth := lineWidth }

private def applyFrictionImpulse (a b : Particle) (contact : Contact)
    (impulse : Vec3) (mu : Float) : Particle × Particle × Vec3 :=
  let relVel := CollisionResponse.relativeVelocity a b
  let velAlongNormal := relVel.dot contact.normal
  let tangent := relVel.sub (contact.normal.scale velAlongNormal)
  let tangentLen := tangent.length
  if tangentLen < Float.epsilon then
    (a, b, Vec3.zero)
  else
    let tDir := tangent.scale (1.0 / tangentLen)
    let invMassSum := a.inverseMass + b.inverseMass
    if invMassSum == 0.0 then
      (a, b, Vec3.zero)
    else
      let jt := -relVel.dot tDir / invMassSum
      let maxFriction := mu * impulse.length
      let frictionMag := Float.clamp jt (-maxFriction) maxFriction
      let frictionImpulse := tDir.scale frictionMag
      let newA := CollisionResponse.applyImpulse a frictionImpulse.neg
      let newB := CollisionResponse.applyImpulse b frictionImpulse
      (newA, newB, frictionImpulse)

def stepCollisionResponseDemo (state : CollisionResponseDemoState) (dt : Float)
    : CollisionResponseDemoState :=
  Id.run do
    let dt' := Float.min dt 0.033
    let mut a := state.particleA
    let mut b := state.particleB

    if state.animating then
      a := { a with position := a.position.add (a.velocity.scale dt') }
      b := { b with position := b.position.add (b.velocity.scale dt') }

    let dist := (b.position.sub a.position).length
    let combined := state.radius + state.radius
    let mut snapshot : Option CollisionSnapshot := state.lastCollision

    if dist < combined then
      let normal := if dist < Float.epsilon then Vec3.unitX else b.position.sub a.position |>.scale (1.0 / dist)
      let contactPoint := a.position.add (normal.scale state.radius)
      let contact := { point := contactPoint, normal := normal, penetration := combined - dist }

      let preVelA := a.velocity
      let preVelB := b.velocity

      let impulse := CollisionResponse.particleImpulse a b contact state.restitution
      let (a', b') := CollisionResponse.resolveParticleCollision a b contact state.restitution
      let (a'', b'', frictionImpulse) := applyFrictionImpulse a' b' contact impulse state.friction
      let (aFinal, bFinal) := CollisionResponse.positionalCorrection a'' b'' contact 0.8 0.01

      snapshot := some {
        point := contactPoint
        normal := normal
        impulse := impulse
        frictionImpulse := frictionImpulse
        preVelA := preVelA
        preVelB := preVelB
        postVelA := aFinal.velocity
        postVelB := bFinal.velocity
      }

      a := aFinal
      b := bFinal

    let boundary := 5.5
    if Float.abs a.position.x > boundary || Float.abs b.position.x > boundary then
      let (resetA, resetB) := defaultParticles
      return { state with particleA := resetA, particleB := resetB, lastCollision := none, time := 0.0 }
    else
      return { state with particleA := a, particleB := b, lastCollision := snapshot, time := state.time + dt' }

/-- Render collision response demo. -/
def renderCollisionResponseDemo (state : CollisionResponseDemoState)
    (w h : Float) (screenScale : Float) (fontMedium fontSmall : Font) : CanvasM Unit := do
  let origin : Float × Float := (w / 2, h / 2)
  let scale : Float := 70.0 * screenScale

  drawGrid2D {
    origin := origin
    scale := scale
    width := w
    height := h
    majorSpacing := 2.0
  } fontSmall

  let posA := Vec2.mk state.particleA.position.x state.particleA.position.y
  let posB := Vec2.mk state.particleB.position.x state.particleB.position.y
  drawCircleWorld posA state.radius origin scale (Color.rgba 0.3 0.8 1.0 0.9)
    (some (Color.rgba 0.3 0.8 1.0 0.15))
  drawCircleWorld posB state.radius origin scale (Color.rgba 1.0 0.6 0.3 0.9)
    (some (Color.rgba 1.0 0.6 0.3 0.15))

  let velA2 := Vec2.mk state.particleA.velocity.x state.particleA.velocity.y
  let velB2 := Vec2.mk state.particleB.velocity.x state.particleB.velocity.y
  drawVectorArrow posA velA2 origin scale { color := Color.rgba 0.2 0.8 1.0 1.0, lineWidth := 2.0 }
  drawVectorArrow posB velB2 origin scale { color := Color.rgba 1.0 0.6 0.3 1.0, lineWidth := 2.0 }

  match state.lastCollision with
  | some snap =>
      let contact2 := Vec2.mk snap.point.x snap.point.y
      let normal2 := Vec2.mk snap.normal.x snap.normal.y
      drawArrowWorld contact2 (contact2.add (normal2.scale 1.2)) origin scale (Color.rgba 0.9 0.9 0.2 1.0) 2.4
      let impulse2 := Vec2.mk snap.impulse.x snap.impulse.y
      drawArrowWorld contact2 (contact2.add (impulse2.scale 0.4)) origin scale (Color.rgba 0.9 0.3 0.9 1.0) 2.4
      let friction2 := Vec2.mk snap.frictionImpulse.x snap.frictionImpulse.y
      drawArrowWorld contact2 (contact2.add (friction2.scale 0.4)) origin scale (Color.rgba 0.4 0.9 0.8 1.0) 2.0
  | none => pure ()

  let restLayout := sliderLayout w h screenScale 0
  let fricLayout := sliderLayout w h screenScale 1
  drawSlider restLayout state.restitution "Restitution" fontSmall
  drawSlider fricLayout state.friction "Friction" fontSmall

  let infoY := h - 140 * screenScale
  setFillColor VecColor.label
  fillTextXY "Collision normal (yellow), impulse (magenta), friction (teal)" (20 * screenScale) infoY fontSmall
  match state.lastCollision with
  | some snap =>
      fillTextXY s!"pre A: {formatVec2 (Vec2.mk snap.preVelA.x snap.preVelA.y)}"
        (20 * screenScale) (infoY + 20 * screenScale) fontSmall
      fillTextXY s!"post A: {formatVec2 (Vec2.mk snap.postVelA.x snap.postVelA.y)}"
        (20 * screenScale) (infoY + 40 * screenScale) fontSmall
      fillTextXY s!"pre B: {formatVec2 (Vec2.mk snap.preVelB.x snap.preVelB.y)}"
        (20 * screenScale) (infoY + 60 * screenScale) fontSmall
      fillTextXY s!"post B: {formatVec2 (Vec2.mk snap.postVelB.x snap.postVelB.y)}"
        (20 * screenScale) (infoY + 80 * screenScale) fontSmall
  | none =>
      fillTextXY "Waiting for collision..." (20 * screenScale) (infoY + 20 * screenScale) fontSmall

  fillTextXY "COLLISION RESPONSE" (20 * screenScale) (30 * screenScale) fontMedium
  setFillColor (Color.gray 0.6)
  let animText := if state.animating then "animating" else "paused"
  fillTextXY s!"Space: {animText} | Drag sliders | R: reset" (20 * screenScale) (55 * screenScale) fontSmall

/-- Create collision response widget. -/
def collisionResponseDemoWidget (env : DemoEnv) (state : CollisionResponseDemoState)
    : Afferent.Arbor.WidgetBuilder := do
  Afferent.Arbor.custom (spec := {
    measure := fun _ _ => (0, 0)
    collect := fun _ => #[]
    draw := some (fun layout => do
      withContentRect layout fun w h => do
        resetTransform
        renderCollisionResponseDemo state w h env.screenScale env.fontMedium env.fontSmall
    )
  }) (style := { flexItem := some (Trellis.FlexItem.growing 1) })

end Demos.Linalg
