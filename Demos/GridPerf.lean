/-
  Grid Performance Test - Spinning squares in a grid
-/
import Afferent

open Afferent CanvasM

namespace Demos

/-- Render grid spinning squares using Canvas's integrated dynamic rendering.
    Static grid positions, GPU does color + NDC conversion. -/
def renderGridTestM (t : Float) (font : Font) (particles : Render.Dynamic.ParticleState)
    (halfSize : Float) : CanvasM Unit := do
  setFillColor Color.white
  fillTextXY s!"Grid: {particles.count} dynamic squares (Space to advance)" 20 30 font
  fillDynamicRectsAnimated particles halfSize t 3.0

end Demos
