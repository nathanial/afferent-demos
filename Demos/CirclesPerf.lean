/-
  Circles Performance Test - Bouncing circles
-/
import Afferent

open Afferent CanvasM

namespace Demos

/-- Render bouncing circles using Canvas's integrated dynamic rendering.
    CPU updates positions (physics), GPU does color + NDC conversion. -/
def renderCircleTestM (t : Float) (font : Font) (particles : Render.Dynamic.ParticleState)
    (radius : Float) : CanvasM Unit := do
  setFillColor Color.white
  fillTextXY s!"Circles: {particles.count} dynamic circles (Space to advance)" 20 30 font
  fillDynamicCircles particles radius t

def stepCirclesPerfFrame (c : Canvas) (dt t : Float) (font : Font)
    (particles : Render.Dynamic.ParticleState) (radius : Float) (screenScale : Float)
    : IO (Canvas × Render.Dynamic.ParticleState) := do
  let nextParticles := particles.updateBouncing dt radius
  let c ← run' c do
    resetTransform
    setFillColor Color.white
    fillTextXY
      s!"Circles: {nextParticles.count} dynamic circles (Space to advance)"
      (20 * screenScale) (30 * screenScale) font
    fillDynamicCircles nextParticles radius t
  pure (c, nextParticles)

end Demos
