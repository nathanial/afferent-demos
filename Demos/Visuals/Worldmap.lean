/-
  Worldmap Demo
  Handles input, updates tiles, and renders overlay labels.
-/
import Afferent
import Demos.Core.Demo
import Worldmap

open Afferent CanvasM

namespace Demos

def updateWorldmapDemo (env : DemoEnv) (state : Worldmap.MapState) : IO Worldmap.MapState := do
  let mut mapState := state
  let width := (max 1.0 env.physWidthF).toUInt32
  let height := (max 1.0 env.physHeightF).toUInt32
  mapState := mapState.updateScreenSize width.toNat height.toNat
  mapState ← Worldmap.handleInputAt env.window mapState env.contentOffsetX env.contentOffsetY
  mapState := Worldmap.updateZoomAnimation mapState
  Worldmap.cancelStaleTasks mapState
  mapState ← Worldmap.updateTileCache mapState
  pure mapState

def worldmapWidget (screenScale : Float) (fontMedium fontSmall : Font)
    (windowW windowH : Float) (state : Worldmap.MapState) : Afferent.Arbor.WidgetBuilder := do
  Afferent.Arbor.custom (spec := {
    measure := fun _ _ => (0, 0)
    collect := fun _ => #[]
    draw := some (fun layout => do
      withContentRect layout fun _ _ => do
        let rect := layout.contentRect
        let renderer ← getRenderer
        Worldmap.renderAt renderer state rect.x rect.y windowW windowH
        resetTransform
        setFillColor Color.white
        fillTextXY "Worldmap Demo - drag to pan, scroll to zoom (Space to advance)"
          (20 * screenScale) (30 * screenScale) fontMedium
        let lat := state.viewport.centerLat
        let lon := state.viewport.centerLon
        let zoom := state.displayZoom
        fillTextXY s!"lat={lat} lon={lon} zoom={zoom}"
          (20 * screenScale) (55 * screenScale) fontSmall
    )
  }) (style := { flexItem := some (Trellis.FlexItem.growing 1) })

end Demos
