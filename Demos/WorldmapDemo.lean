/-
  Worldmap Demo
  Handles input, updates tiles, and renders overlay labels.
-/
import Afferent
import Worldmap

open Afferent CanvasM

namespace Demos

def stepWorldmapDemoFrame (c : Canvas) (state : Worldmap.MapState) (screenScale : Float)
    (fontMedium fontSmall : Font) : IO (Canvas × Worldmap.MapState) := do
  let mut mapState := state
  let (w, h) ← FFI.Window.getSize c.ctx.window
  mapState := mapState.updateScreenSize w.toNat h.toNat
  mapState ← Worldmap.handleInput c.ctx.window mapState
  mapState := Worldmap.updateZoomAnimation mapState
  Worldmap.cancelStaleTasks mapState
  mapState ← Worldmap.updateTileCache mapState

  c.ctx.resetScissor
  Worldmap.render c.ctx.renderer mapState

  let c ← run' c do
    resetTransform
    setFillColor Color.white
    fillTextXY "Worldmap Demo - drag to pan, scroll to zoom (Space to advance)"
      (20 * screenScale) (30 * screenScale) fontMedium
    let lat := mapState.viewport.centerLat
    let lon := mapState.viewport.centerLon
    let zoom := mapState.displayZoom
    fillTextXY s!"lat={lat} lon={lon} zoom={zoom}"
      (20 * screenScale) (55 * screenScale) fontSmall
  pure (c, mapState)

end Demos
