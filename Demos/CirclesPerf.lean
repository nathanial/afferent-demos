/-
  Circles Performance Test - Bouncing circles
-/
import Afferent

open Afferent CanvasM

namespace Demos

/-- Render bouncing circles using unified Dynamic module.
    CPU updates positions (physics), GPU does color + NDC conversion. -/
def renderCircleTestM (t : Float) (font : Font) (particles : Render.Dynamic.ParticleState)
    (radius : Float) (buffer : FFI.FloatBuffer) : CanvasM Unit := do
  setFillColor Color.white
  fillTextXY s!"Circles: {particles.count} dynamic circles (Space to advance)" 20 30 font
  let renderer ← getRenderer
  Render.Dynamic.drawCircles renderer particles buffer radius t

end Demos
