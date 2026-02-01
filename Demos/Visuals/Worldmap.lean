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

def worldmapWidgetNamed (screenScale : Float) (fontMedium fontSmall : Font)
    (windowW windowH : Float) (state : Worldmap.MapState)
    (mapName sidebarName : String) : Afferent.Arbor.WidgetBuilder := do
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
  let sidebarSpec := {
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
      let keepCount := countTilesInRange reqMinZoom reqMaxZoom 3
      let candidateCount := countTilesInRange minZoom maxZoom 1
      let visibleCount := state.viewport.visibleTiles.length
      let tileSetInRange := fun (minZ maxZ : Int) (buffer : Int) => Id.run do
        let minZ := minZ.toNat
        let maxZ := maxZ.toNat
        let mut set : Std.HashSet Tileset.TileCoord := {}
        for z in [minZ:maxZ+1] do
          let zInt := Tileset.natToInt z
          let vp := { state.viewport with zoom := zInt }
          let tiles := vp.visibleTilesWithBuffer buffer
          for coord in tiles do
            set := set.insert coord
        return set
      let mut ready := 0
      let mut pending := 0
      let mut failed := 0
      let mut missing := 0
      let requestedSet := tileSetInRange reqMinZoom reqMaxZoom 1
      for coord in requestedSet.toList do
        match dynamics[coord]? with
        | none => missing := missing + 1
        | some dyn =>
            let loadState ← dyn.sample
            match loadState with
            | .ready _ => ready := ready + 1
            | .loading => pending := pending + 1
            | .error _ => failed := failed + 1
      let textureCoords ← state.textureCache.coordinates
      let candidateSet := tileSetInRange minZoom maxZoom 1
      let mut fallbackCached := 0
      for coord in textureCoords do
        if candidateSet.contains coord && !requestedSet.contains coord then
          fallbackCached := fallbackCached + 1
      fillTextXY "Worldmap Debug" left y fontMedium
      y := y + line
      fillTextXY s!"Zoom: {fmt1 state.displayZoom} (tile {state.viewport.zoom})" left y fontSmall
      y := y + line
      fillTextXY s!"Visible tiles: {visibleCount}" left y fontSmall
      y := y + line
      fillTextXY s!"Requested tiles: {requestedCount}" left y fontSmall
      y := y + line
      fillTextXY s!"Keep tiles (buffer 3): {keepCount}" left y fontSmall
      y := y + line
      fillTextXY s!"Candidate tiles: {candidateCount}" left y fontSmall
      y := y + line
      fillTextXY s!"Fallback cached: {fallbackCached}" left y fontSmall
      y := y + line
      fillTextXY s!"Dynamics tracked: {dynamics.size}" left y fontSmall
      y := y + line
      fillTextXY s!"Ready (requested): {ready}" left y fontSmall
      y := y + line
      fillTextXY s!"Pending (requested): {pending}" left y fontSmall
      y := y + line
      fillTextXY s!"Failed (requested): {failed}" left y fontSmall
      y := y + line
      fillTextXY s!"Missing dynamics: {missing}" left y fontSmall
      y := y + line
      fillTextXY s!"GPU tiles: {gpuCount}" left y fontSmall
      y := y + line
      fillTextXY s!"GPU MB (est): {fmt1 gpuMb}" left y fontSmall
      y := y + line
      fillTextXY s!"Fallback z: {minZoom}..{maxZoom}" left y fontSmall
      y := y + line
      fillTextXY s!"Fade frames: {state.fadeFrames}" left y fontSmall
    )
  }
  let mapSpec := {
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
  }
  let sidebarWidget :=
    if sidebarName == "" then
      Afferent.Arbor.custom (spec := sidebarSpec) (style := sidebarStyle)
    else
      Afferent.Arbor.namedCustom sidebarName (spec := sidebarSpec) (style := sidebarStyle)
  let mapWidget :=
    if mapName == "" then
      Afferent.Arbor.custom (spec := mapSpec) (style := mapStyle)
    else
      Afferent.Arbor.namedCustom mapName (spec := mapSpec) (style := mapStyle)
  Afferent.Arbor.flexRow (Trellis.FlexContainer.row 0) { width := .percent 1.0, height := .percent 1.0 } #[
    sidebarWidget,
    mapWidget
  ]

def worldmapWidget (screenScale : Float) (fontMedium fontSmall : Font)
    (windowW windowH : Float) (state : Worldmap.MapState) : Afferent.Arbor.WidgetBuilder := do
  worldmapWidgetNamed screenScale fontMedium fontSmall windowW windowH state "" ""

/-- Create TileManager configuration for the demo -/
def worldmapTileConfig : TileManagerConfig := {
  provider := TileProvider.cartoDarkRetina
  diskCacheDir := ".tile-cache"
  diskCacheMaxSize := 500 * 1024 * 1024  -- 500 MB
}

end Demos
