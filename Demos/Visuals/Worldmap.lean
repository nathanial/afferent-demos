/-
  Worldmap Demo
  Handles input, updates tiles, and renders overlay labels.

  Note: This demo requires a TileManager for tile loading. The TileManager
  must be created and maintained in a SpiderM context by the demo runner.
-/
import Afferent
import Demos.Core.Demo
import Worldmap
import Tileset
import Reactive

open Afferent CanvasM
open Tileset (TileManager TileManagerConfig TileProvider)
open Reactive.Host (SpiderM)

namespace Demos

/-- Update worldmap state for one frame (IO portion only).
    This handles input, zoom animation, and frame ticking.
    Tile loading must be done separately in SpiderM via requestVisibleTiles. -/
def updateWorldmapDemo (env : DemoEnv) (state : Worldmap.MapState)
    (mgr : TileManager) : IO Worldmap.MapState := do
  let mut mapState := state
  let width := (max 1.0 env.physWidthF).toUInt32
  let height := (max 1.0 env.physHeightF).toUInt32
  mapState := mapState.updateScreenSize width.toNat height.toNat
  mapState ← Worldmap.handleInputAt env.window mapState env.contentOffsetX env.contentOffsetY
  mapState ← Worldmap.updateFrame mapState mgr
  pure mapState

/-- Request tiles for visible area (must be called from SpiderM context) -/
def requestWorldmapTiles (state : Worldmap.MapState) (mgr : TileManager) : SpiderM Worldmap.MapState := do
  if Worldmap.shouldFetchNewTiles state then
    Worldmap.requestVisibleTiles state mgr
  else
    pure state

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

/-- Create TileManager configuration for the demo -/
def worldmapTileConfig : TileManagerConfig := {
  provider := TileProvider.cartoDarkRetina
  diskCacheDir := ".tile-cache"
  diskCacheMaxSize := 500 * 1024 * 1024  -- 500 MB
}

end Demos
