/-
  SpinningCubes Widget for Overview Grid
  Shows a 5x5 grid of spinning cubes with a static camera (no FPS controls).
-/
import Afferent
import Afferent.Arbor
import Demos.SpinningCubes
import Trellis

open Afferent Afferent.Arbor CanvasM

namespace Demos

/-- SpinningCubes widget for Overview grid.
    Uses static camera (no FPS controls) and renders into its cell viewport. -/
def spinningCubesOverviewWidget (t : Float) (windowW windowH : Float) : WidgetBuilder := do
  custom (spec := {
    measure := fun _ _ => (0, 0)
    collect := fun _ => #[]
    draw := some (fun layout => do
      withContentRect layout fun w h => do
        let renderer ← getRenderer
        let rect := layout.contentRect
        renderSpinningCubesViewport renderer t w h rect.x rect.y windowW windowH
    )
  }) (style := {
    width := .percent 1.0
    height := .percent 1.0
    flexItem := some (Trellis.FlexItem.growing 1)
  })

end Demos
