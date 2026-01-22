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
  let s := screenScale
  let sidebarWidth := 320 * s
  let sidebarStyle : Afferent.Arbor.BoxStyle := {
    width := .length sidebarWidth
    height := .percent 1.0
    flexItem := some (Trellis.FlexItem.fixed sidebarWidth)
  }
  let mapStyle : Afferent.Arbor.BoxStyle := {
    width := .percent 1.0
    height := .percent 1.0
    flexItem := some (Trellis.FlexItem.growing 1)
  }
  Afferent.Arbor.flexRow (Trellis.FlexContainer.row 0) { width := .percent 1.0, height := .percent 1.0 } #[
    Afferent.Arbor.custom (spec := {
      measure := fun _ _ => (0, 0)
      collect := fun _ => #[]
      draw := some (fun layout => do
        let rect := layout.contentRect
        resetTransform
        setFillColor (Color.gray 0.1)
        fillRectXYWH rect.x rect.y rect.width rect.height
        setFillColor (Color.gray 0.9)
        let left := rect.x + (12 * s)
        let mut y := rect.y + (22 * s)
        let line := 18 * s
        let fmt1 := fun (v : Float) => s!"{(v * 10).toUInt32.toFloat / 10}"
        let (gpuCount, gpuBytes) ← state.textureCache.stats
        let gpuMb := (gpuBytes.toFloat / 1024.0 / 1024.0)
        let dynamics ← state.tileDynamics.get
        let mut ready := 0
        let mut pending := 0
        let mut failed := 0
        for (_, dyn) in dynamics.toList do
          let loadState ← dyn.sample
          match loadState with
          | .ready _ => ready := ready + 1
          | .loading => pending := pending + 1
          | .error _ => failed := failed + 1
        let minZoom := Tileset.intClamp (state.viewport.zoom - Tileset.natToInt state.fallbackParentDepth)
          state.tileProvider.minZoom state.tileProvider.maxZoom
        let maxZoom := Tileset.intClamp (state.viewport.zoom + Tileset.natToInt state.fallbackChildDepth)
          state.tileProvider.minZoom state.tileProvider.maxZoom
        let (reqMinZoom, reqMaxZoom) := Worldmap.requestedZoomRange state
        let countTilesInRange := fun (minZ maxZ : Int) (buffer : Int) => Id.run do
          let minZ := minZ.toNat
          let maxZ := maxZ.toNat
          let mut count := 0
          for z in [minZ:maxZ+1] do
            let zInt := Tileset.natToInt z
            let vp := { state.viewport with zoom := zInt }
            count := count + (vp.visibleTilesWithBuffer buffer).length
          return count
        let requestedCount := countTilesInRange reqMinZoom reqMaxZoom 1
        let candidateCount := countTilesInRange minZoom maxZoom 1
        let fallbackCount := candidateCount - requestedCount
        let visibleCount := state.viewport.visibleTiles.length
        fillTextXY "Worldmap Debug" left y fontMedium
        y := y + line
        fillTextXY s!"Zoom: {fmt1 state.displayZoom} (tile {state.viewport.zoom})" left y fontSmall
        y := y + line
        fillTextXY s!"Visible tiles: {visibleCount}" left y fontSmall
        y := y + line
        fillTextXY s!"Requested tiles: {requestedCount}" left y fontSmall
        y := y + line
        fillTextXY s!"Fallback tiles: {fallbackCount}" left y fontSmall
        y := y + line
        fillTextXY s!"Dynamics: {dynamics.size}" left y fontSmall
        y := y + line
        fillTextXY s!"Ready: {ready}  Pending: {pending}  Failed: {failed}" left y fontSmall
        y := y + line
        fillTextXY s!"GPU tiles: {gpuCount}  (~{fmt1 gpuMb} MB)" left y fontSmall
        y := y + line
        fillTextXY s!"Fallback z: {minZoom}..{maxZoom}" left y fontSmall
        y := y + line
        fillTextXY s!"Fade: {state.fadeFrames} frames" left y fontSmall
      )
    }) (style := sidebarStyle),
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
            (20 * s) (30 * s) fontMedium
          let lat := state.viewport.centerLat
          let lon := state.viewport.centerLon
          let zoom := state.displayZoom
          fillTextXY s!"lat={lat} lon={lon} zoom={zoom}"
            (20 * s) (55 * s) fontSmall
      )
    }) (style := mapStyle)
  ]

/-- Create TileManager configuration for the demo -/
def worldmapTileConfig : TileManagerConfig := {
  provider := TileProvider.cartoDarkRetina
  diskCacheDir := ".tile-cache"
  diskCacheMaxSize := 500 * 1024 * 1024  -- 500 MB
}

end Demos
