/-
  Triangles Performance Test - Spinning triangles in a grid
-/
import Afferent

open Afferent CanvasM

namespace Demos

/-- Render grid of spinning triangles using unified Dynamic module. -/
def renderTriangleTestM (t : Float) (font : Font) (particles : Render.Dynamic.ParticleState)
    (halfSize : Float) (buffer : FFI.FloatBuffer) : CanvasM Unit := do
  setFillColor Color.white
  fillTextXY s!"Triangles: {particles.count} dynamic triangles (Space to advance)" 20 30 font
  let renderer ← getRenderer
  Render.Dynamic.drawTrianglesAnimated renderer particles buffer halfSize t 2.0

end Demos
