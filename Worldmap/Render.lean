/-
  Tile Map Rendering
  Uses tileset library for tile loading and TextureCache for GPU textures
-/
import Worldmap.State
import Worldmap.Zoom
import Worldmap.Utils
import Tileset
import Reactive
import Raster
import Afferent.FFI.Texture
import Afferent.FFI.Renderer
import Afferent.FFI.FloatBuffer

namespace Worldmap

open Tileset (TileCoord TileManager TileLoadState MapViewport)
open Tileset (intToFloat natToInt clampLatitude clampZoom pi)
open Afferent.FFI (Texture Renderer)
open Reactive.Host (Dyn SpiderM)
open Worldmap.Zoom (centerForAnchor)
open Std (HashMap HashSet)

/-- Update zoom animation state.
    Lerps displayZoom toward targetZoom, keeping the anchor point fixed on screen. -/
def updateZoomAnimation (state : MapState) : MapState :=
  if !state.isAnimatingZoom then state
  else
    let config := state.zoomAnimationConfig
    let target := intToFloat state.targetZoom
    let diff := target - state.displayZoom
    if Float.abs diff < config.snapThreshold then
      -- Snap to target and stop animation
      let (newLat, newLon) := centerForAnchor
          state.zoomAnchorLat state.zoomAnchorLon
          state.zoomAnchorScreenX state.zoomAnchorScreenY
          state.viewport.screenWidth state.viewport.screenHeight
          state.viewport.tileSize target
      -- Clamp to bounds
      let clampedLat := state.mapBounds.clampLat (clampLatitude newLat)
      let clampedLon := state.mapBounds.clampLon newLon
      { state with
          displayZoom := target
          isAnimatingZoom := false
          viewport := { state.viewport with
            centerLat := clampedLat
            centerLon := clampedLon
            zoom := state.targetZoom
          }
      }
    else
      -- Lerp toward target with configured factor
      let newDisplayZoom := state.displayZoom + diff * config.lerpFactor
      -- Recompute center to keep anchor fixed
      let (newLat, newLon) := centerForAnchor
          state.zoomAnchorLat state.zoomAnchorLon
          state.zoomAnchorScreenX state.zoomAnchorScreenY
          state.viewport.screenWidth state.viewport.screenHeight
          state.viewport.tileSize newDisplayZoom
      -- Clamp to bounds
      let clampedLat := state.mapBounds.clampLat (clampLatitude newLat)
      let clampedLon := state.mapBounds.clampLon newLon
      -- Update viewport.zoom to floor of displayZoom for tile fetching
      let tileZoom := state.mapBounds.clampZoom (clampZoom (natToInt newDisplayZoom.floor.toUInt64.toNat))
      { state with
          displayZoom := newDisplayZoom
          viewport := { state.viewport with
            centerLat := clampedLat
            centerLon := clampedLon
            zoom := tileZoom
          }
      }

/-- Request tiles for the visible area. Call this once per frame.
    Returns the updated state with any new tile dynamics registered. -/
def requestVisibleTiles (state : MapState) (mgr : TileManager) : SpiderM MapState := do
  let visibleCoords := state.viewport.visibleTilesWithBuffer 1
  let dynamics ← SpiderM.liftIO state.tileDynamics.get

  -- Request tiles we don't have dynamics for yet
  let mut newDynamics := dynamics
  for coord in visibleCoords do
    unless newDynamics.contains coord do
      let dyn ← mgr.requestTile coord
      newDynamics := newDynamics.insert coord dyn

  -- Also request parent tiles for fallback
  if state.viewport.zoom > 0 then
    let parentSet : HashSet TileCoord := visibleCoords.foldl (fun s t => s.insert t.parentTile) {}
    for parentCoord in parentSet.toList do
      unless newDynamics.contains parentCoord do
        let dyn ← mgr.requestTile parentCoord
        newDynamics := newDynamics.insert parentCoord dyn

  SpiderM.liftIO <| state.tileDynamics.set newDynamics
  pure state

/-- Evict distant tiles from GPU texture cache and dynamics map -/
def evictDistantTiles (state : MapState) (mgr : TileManager) : IO MapState := do
  let buffer := 3  -- Keep tiles within 3 tiles of viewport
  let keepSet := state.viewport.visibleTileSet buffer

  -- Evict from GPU texture cache
  state.textureCache.evictDistant keepSet
  state.textureCache.evictOldest keepSet

  -- Evict from dynamics map
  let dynamics ← state.tileDynamics.get
  let toRemove := dynamics.toList.filter fun (coord, _) => !keepSet.contains coord
  let dynamics' := toRemove.foldl (fun d (coord, _) => d.erase coord) dynamics
  state.tileDynamics.set dynamics'

  -- Evict from TileManager
  mgr.evictDistant keepSet

  pure state

/-- Check if we should fetch new tiles (respects zoom debouncing) -/
def shouldFetchNewTiles (state : MapState) : Bool :=
  if state.isAnimatingZoom then
    state.frameCount - state.lastZoomChangeFrame >= state.zoomDebounceFrames
  else
    true

/-- Upload a limited number of ready tiles to GPU each frame. -/
def uploadReadyTiles (state : MapState) (maxUploads : Nat := 2) : IO MapState := do
  let dynamics ← state.tileDynamics.get
  let visible := state.viewport.visibleTiles
  let mut uploaded := 0
  for coord in visible do
    if uploaded < maxUploads then
      if !(← state.textureCache.has coord) then
        if let some dyn := dynamics[coord]? then
          let loadState ← dyn.sample
          match loadState with
          | .ready img =>
            let _ ← state.textureCache.getOrUploadImage coord img state.frameCount
            uploaded := uploaded + 1
          | _ => pure ()
  pure state

/-- Compute screen position for a tile with fractional zoom support -/
def tileScreenPosFrac (vp : MapViewport) (tile : TileCoord) (displayZoom : Float) : (Float × Float) :=
  let n := Float.pow 2.0 displayZoom
  let centerTileX := (vp.centerLon + 180.0) / 360.0 * n
  let latRad := vp.centerLat * pi / 180.0
  let centerTileY := (1.0 - Float.log (Float.tan latRad + 1.0 / Float.cos latRad) / pi) / 2.0 * n
  let scale := Float.pow 2.0 (displayZoom - intToFloat tile.z)
  let tileX := (intToFloat tile.x) * scale
  let tileY := (intToFloat tile.y) * scale
  let offsetX := (tileX - centerTileX) * (intToFloat vp.tileSize) + (intToFloat vp.screenWidth) / 2.0
  let offsetY := (tileY - centerTileY) * (intToFloat vp.tileSize) + (intToFloat vp.screenHeight) / 2.0
  (offsetX, offsetY)

/-- Try to get a texture for a tile. Returns None if not ready. -/
def getTileTexture (state : MapState) (coord : TileCoord) : IO (Option Texture) := do
  if let some tex ← state.textureCache.get? coord state.frameCount then
    pure (some tex)
  else
    pure none

/-- Find a loaded fallback texture from parent tiles -/
def findParentFallback (state : MapState) (coord : TileCoord) (maxLevels : Nat := 3)
    : IO (Option (TileCoord × Texture × Nat)) :=
  go coord 1 maxLevels
where
  go (c : TileCoord) (delta : Nat) (remaining : Nat) : IO (Option (TileCoord × Texture × Nat)) := do
    match remaining with
    | 0 => pure none
    | remaining' + 1 =>
      if c.z <= 0 then pure none
      else
        let parent := c.parentTile
        match ← getTileTexture state parent with
        | some tex => pure (some (parent, tex, delta))
        | none => go parent (delta + 1) remaining'

/-- Render all visible tiles with fractional zoom scaling -/
def renderTilesAt (renderer : Renderer) (state : MapState)
    (offsetX offsetY canvasWidth canvasHeight : Float) : IO Unit := do
  let visible := state.viewport.visibleTiles
  let spriteBuffer ← Afferent.FFI.FloatBuffer.create 5
  let drawSprite := fun (texture : Texture) (dstX dstY size : Float) => do
    let half := size / 2.0
    let cx := dstX + half
    let cy := dstY + half
    Afferent.FFI.FloatBuffer.setVec5 spriteBuffer 0 cx cy 0.0 half 1.0
    Renderer.drawSpritesInstanceBuffer renderer texture spriteBuffer 1 canvasWidth canvasHeight

  -- Compute scale factor for fractional zoom
  let tileZoom := state.viewport.zoom
  let scale := Float.pow 2.0 (state.displayZoom - intToFloat tileZoom)
  let scaledTileSize := (intToFloat state.viewport.tileSize) * scale

  -- PASS 1: Render parent tiles as background layer (scaled up 2x)
  if state.viewport.zoom > 0 then
    let parentSet : HashSet TileCoord := visible.foldl
      (fun s t => s.insert t.parentTile) {}
    let parentTileSize := scaledTileSize * 2.0
    for parentCoord in parentSet.toList do
      if let some tex ← getTileTexture state parentCoord then
        let (px, py) := tileScreenPosFrac state.viewport parentCoord state.displayZoom
        let dstX := px + offsetX
        let dstY := py + offsetY
        drawSprite tex dstX dstY parentTileSize

  -- PASS 2: Render visible tiles on top (higher resolution)
  for coord in visible do
    let (x, y) := tileScreenPosFrac state.viewport coord state.displayZoom
    let dstX := x + offsetX
    let dstY := y + offsetY
    if let some tex ← getTileTexture state coord then
      drawSprite tex dstX dstY scaledTileSize
    else
      -- Not loaded - try fallback from parent
      if let some (_, tex, _) ← findParentFallback state coord then
        drawSprite tex dstX dstY scaledTileSize

  Afferent.FFI.FloatBuffer.destroy spriteBuffer

/-- Render all visible tiles with fractional zoom scaling -/
def renderTiles (renderer : Renderer) (state : MapState) : IO Unit := do
  let canvasWidth := (intToFloat state.viewport.screenWidth)
  let canvasHeight := (intToFloat state.viewport.screenHeight)
  renderTilesAt renderer state 0.0 0.0 canvasWidth canvasHeight

/-- Main render function -/
def render (renderer : Renderer) (state : MapState) : IO Unit := do
  renderTiles renderer state

def renderAt (renderer : Renderer) (state : MapState)
    (offsetX offsetY canvasWidth canvasHeight : Float) : IO Unit := do
  renderTilesAt renderer state offsetX offsetY canvasWidth canvasHeight

/-- Combined update function: animation + eviction.
    Call this once per frame in IO context. -/
def updateFrame (state : MapState) (mgr : TileManager) : IO MapState := do
  let state := updateZoomAnimation state
  let state := state.tick
  let state ← uploadReadyTiles state
  let state ← evictDistantTiles state mgr
  mgr.tick
  pure state

end Worldmap
