/-
  Triangles Performance Test - Spinning triangles in a grid
-/
import Afferent
import Afferent.Arbor
import Demos.Demo
import Trellis

open Afferent CanvasM

namespace Demos

/-- Render grid of spinning triangles using Canvas's integrated dynamic rendering. -/
def renderTriangleTestM (t : Float) (font : Font) (particles : Render.Dynamic.ParticleState)
    (halfSize : Float) : CanvasM Unit := do
  setFillColor Color.white
  fillTextXY s!"Triangles: {particles.count} dynamic triangles (Space to advance)" 20 30 font
  fillDynamicInstancedAnimated 1 particles halfSize t 2.0

def trianglesPerfWidget (t : Float) (font : Font)
    (particles : Render.Dynamic.ParticleState) (halfSize : Float) : Afferent.Arbor.WidgetBuilder := do
  Afferent.Arbor.custom (spec := {
    measure := fun _ _ => (0, 0)
    collect := fun _ => #[]
    draw := some (fun layout => do
      withContentRect layout fun _ _ => do
        resetTransform
        renderTriangleTestM t font particles halfSize
    )
  }) (style := { flexItem := some (Trellis.FlexItem.growing 1) })

end Demos
