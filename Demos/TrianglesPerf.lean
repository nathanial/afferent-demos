/-
  Triangles Performance Test - Spinning triangles in a grid
-/
import Afferent

open Afferent CanvasM

namespace Demos

/-- Render grid of spinning triangles using Canvas's integrated dynamic rendering. -/
def renderTriangleTestM (t : Float) (font : Font) (particles : Render.Dynamic.ParticleState)
    (halfSize : Float) : CanvasM Unit := do
  setFillColor Color.white
  fillTextXY s!"Triangles: {particles.count} dynamic triangles (Space to advance)" 20 30 font
  fillDynamicTrianglesAnimated particles halfSize t 2.0

end Demos
