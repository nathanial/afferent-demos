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
open Tileset (intToFloat natToInt clampLatitude clampZoom intClamp pi)
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

private def zoomRange (state : MapState) : (Int × Int) :=
  let parentDepth := natToInt state.fallbackParentDepth
  let childDepth := natToInt state.fallbackChildDepth
  let minZoom := intClamp (state.viewport.zoom - parentDepth)
    state.tileProvider.minZoom state.tileProvider.maxZoom
  let maxZoom := intClamp (state.viewport.zoom + childDepth)
    state.tileProvider.minZoom state.tileProvider.maxZoom
  (state.mapBounds.clampZoom minZoom, state.mapBounds.clampZoom maxZoom)

def requestedZoomRange (state : MapState) : (Int × Int) :=
  let zoom := intClamp state.viewport.zoom
    state.tileProvider.minZoom state.tileProvider.maxZoom
  let zoom := state.mapBounds.clampZoom zoom
  (zoom, zoom)

/-- Zoom priority order: target first, then alternating outward within bounds. -/
def zoomPriority (target minZoom maxZoom : Int) : List Int :=
  let minZoom := intClamp minZoom minZoom maxZoom
  let maxZoom := intClamp maxZoom minZoom maxZoom
  let maxErr := Nat.max (target - minZoom).toNat (maxZoom - target).toNat
  Id.run do
    let mut acc : List Int := []
    for err in [:maxErr.succ] do
      let zLow := target - natToInt err
      if zLow >= minZoom && zLow <= maxZoom then
        acc := acc ++ [zLow]
      let zHigh := target + natToInt err
      if zHigh != zLow && zHigh >= minZoom && zHigh <= maxZoom then
        acc := acc ++ [zHigh]
    return acc

private def visibleTilesAtZoom (state : MapState) (zoom : Int) (buffer : Int) : List TileCoord :=
  let vp := { state.viewport with zoom := zoom }
  vp.visibleTilesWithBuffer buffer

private def candidateTileSet (state : MapState) (buffer : Int) : HashSet TileCoord :=
  let (minZoom, maxZoom) := zoomRange state
  let minZ := minZoom.toNat
  let maxZ := maxZoom.toNat
  Id.run do
    let mut set : HashSet TileCoord := {}
    for z in [minZ:maxZ+1] do
      let zInt := natToInt z
      let tiles := visibleTilesAtZoom state zInt buffer
      for coord in tiles do
        set := set.insert coord
    return set

private def requestTileSet (state : MapState) (buffer : Int) : HashSet TileCoord :=
  let (minZoom, maxZoom) := requestedZoomRange state
  let minZ := minZoom.toNat
  let maxZ := maxZoom.toNat
  Id.run do
    let mut set : HashSet TileCoord := {}
    for z in [minZ:maxZ+1] do
      let zInt := natToInt z
      let tiles := visibleTilesAtZoom state zInt buffer
      for coord in tiles do
        set := set.insert coord
    return set

private def hasParentFallback (state : MapState) (coord : TileCoord) : IO Bool := do
  let mut c := coord
  let mut found := false
  for _ in [:state.fallbackParentDepth] do
    if !found && c.z > 0 then
      let parent := c.parentTile
      if (← state.textureCache.peek? parent).isSome then
        found := true
      c := parent
  pure found

private def hasChildFallback (state : MapState) (coord : TileCoord) : IO Bool := do
  let mut current : List TileCoord := [coord]
  let mut found := false
  for _ in [:state.fallbackChildDepth] do
    if !found then
      let mut next : List TileCoord := []
      for t in current do
        if t.z < state.tileProvider.maxZoom then
          for child in t.childTiles.toList do
            if !found then
              if (← state.textureCache.peek? child).isSome then
                found := true
            next := child :: next
      current := next
  pure found

private def shouldFadeTarget (state : MapState) (coord : TileCoord) : IO Bool := do
  if state.fadeFrames == 0 then
    pure false
  else if (← hasParentFallback state coord) then
    pure true
  else if (← hasChildFallback state coord) then
    pure true
  else
    pure false

/-- Request tiles for the visible area. Call this once per frame.
    Returns the updated state with any new tile dynamics registered. -/
def requestVisibleTiles (state : MapState) (mgr : TileManager) : SpiderM MapState := do
  let visibleCoords := requestTileSet state 1
  let loadKeepSet := requestTileSet state 3
  SpiderM.liftIO <| mgr.evictDistant loadKeepSet
  let dynamics ← SpiderM.liftIO state.tileDynamics.get
  let targetZoom := state.viewport.zoom
  let zoomPenalty : Int := 1000000
  let centerLat := state.viewport.centerLat
  let centerLon := state.viewport.centerLon
  let centerTile := Tileset.latLonToTile { lat := centerLat, lon := centerLon } targetZoom
  let (cursorStart, state) :=
    if centerTile != state.lastRequestCenter then
      let state := { state with
        requestGeneration := state.requestGeneration + 1
        requestCursor := 0
        lastRequestCenter := centerTile
      }
      (0, state)
    else
      (state.requestCursor, state)

  let mut entries : Array (TileCoord × Int) := #[]
  for coord in visibleCoords.toList do
    let center := Tileset.latLonToTile { lat := centerLat, lon := centerLon } coord.z
    let dx := (coord.x - center.x).natAbs
    let dy := (coord.y - center.y).natAbs
    let dist2 := dx * dx + dy * dy
    let zoomDelta := (coord.z - targetZoom).natAbs
    let priority := -((natToInt zoomDelta) * zoomPenalty + (natToInt dist2))
    entries := entries.push (coord, priority)

  -- Request tiles we don't have dynamics for yet
  let mut newDynamics := dynamics
  let sorted := entries.qsort (fun a b => a.2 > b.2)
  let total := sorted.size
  let budget := Nat.min state.requestBudget total
  let start := if total == 0 then 0 else cursorStart % total
  let mut scheduled := 0
  for i in [:budget] do
    let idx := (start + i) % total
    let (coord, priority) := sorted[idx]!
    let mut shouldRequest := true
    if let some dyn := dynamics[coord]? then
      let loadState ← dyn.sample
      match loadState with
      | .ready _ => shouldRequest := false
      | .loading => shouldRequest := true
      | .error _ => shouldRequest := true
    if shouldRequest then
      let dyn ← mgr.requestTileWithPriority coord priority
      newDynamics := newDynamics.insert coord dyn
      scheduled := scheduled + 1
  let nextCursor := if total == 0 then 0 else (start + budget) % total
  let state := { state with requestCursor := nextCursor }

  SpiderM.liftIO <| state.tileDynamics.set newDynamics
  pure state

/-- Evict distant tiles from GPU texture cache and dynamics map -/
def evictDistantTiles (state : MapState) (mgr : TileManager) : IO MapState := do
  let buffer := 3  -- Keep tiles within 3 tiles of viewport
  let keepSet := candidateTileSet state buffer
  let loadKeepSet := requestTileSet state buffer

  -- Evict from GPU texture cache
  state.textureCache.evictDistant keepSet
  state.textureCache.evictOldest keepSet

  -- Evict from dynamics map. Keep only tiles we are actively requesting.
  let dynamics ← state.tileDynamics.get
  let toRemove := dynamics.toList.filter fun (coord, _) => !loadKeepSet.contains coord
  let dynamics' := toRemove.foldl (fun d (coord, _) => d.erase coord) dynamics
  state.tileDynamics.set dynamics'

  -- Evict from TileManager
  mgr.evictDistant loadKeepSet

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
  let (minZoom, maxZoom) := zoomRange state
  let targetZoom := state.viewport.zoom
  let mut uploaded := 0
  let zooms := zoomPriority targetZoom minZoom maxZoom
  for z in zooms do
    let visible := visibleTilesAtZoom state z 1
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

private def tileRenderSize (vp : MapViewport) (tile : TileCoord) (displayZoom : Float) : Float :=
  let scale := Float.pow 2.0 (displayZoom - intToFloat tile.z)
  (intToFloat vp.tileSize) * scale

private def fadeAlpha (state : MapState) (createdFrame : Nat) : Float :=
  if state.fadeFrames == 0 then
    1.0
  else
    let age := state.frameCount - createdFrame
    if age >= state.fadeFrames then
      1.0
    else
      age.toFloat / state.fadeFrames.toFloat

/-- Render all visible tiles with fractional zoom scaling -/
def renderTilesAt (renderer : Renderer) (state : MapState)
    (offsetX offsetY canvasWidth canvasHeight : Float) : IO Unit := do
  let spriteBuffer ← Afferent.FFI.FloatBuffer.create 5
  let drawSprite := fun (texture : Texture) (dstX dstY size alpha : Float) => do
    let half := size / 2.0
    let cx := dstX + half
    let cy := dstY + half
    Afferent.FFI.FloatBuffer.setVec5 spriteBuffer 0 cx cy 0.0 half alpha
    Renderer.drawSpritesInstanceBuffer renderer texture spriteBuffer 1 canvasWidth canvasHeight

  let (minZoom, maxZoom) := zoomRange state
  let targetZoom := state.viewport.zoom
  let maxErr :=
    let errLow := (targetZoom - minZoom).toNat
    let errHigh := (maxZoom - targetZoom).toNat
    Nat.max errLow errHigh

  for e in [:maxErr.succ] do
    let err := maxErr - e
    let zLow := targetZoom - natToInt err
    if zLow >= minZoom && zLow <= maxZoom then
      let visible := visibleTilesAtZoom state zLow 1
      for coord in visible do
        if let some entry ← state.textureCache.getEntry? coord state.frameCount then
          let (x, y) := tileScreenPosFrac state.viewport coord state.displayZoom
          let dstX := x + offsetX
          let dstY := y + offsetY
          let size := tileRenderSize state.viewport coord state.displayZoom
          let alpha ←
            if zLow == targetZoom then
              if (← shouldFadeTarget state coord) then
                pure (fadeAlpha state entry.createdFrame)
              else
                pure 1.0
            else
              pure 1.0
          drawSprite entry.texture dstX dstY size alpha
    let zHigh := targetZoom + natToInt err
    if zHigh != zLow && zHigh >= minZoom && zHigh <= maxZoom then
      let visible := visibleTilesAtZoom state zHigh 1
      for coord in visible do
        if let some entry ← state.textureCache.getEntry? coord state.frameCount then
          let (x, y) := tileScreenPosFrac state.viewport coord state.displayZoom
          let dstX := x + offsetX
          let dstY := y + offsetY
          let size := tileRenderSize state.viewport coord state.displayZoom
          let alpha ←
            if zHigh == targetZoom then
              if (← shouldFadeTarget state coord) then
                pure (fadeAlpha state entry.createdFrame)
              else
                pure 1.0
            else
              pure 1.0
          drawSprite entry.texture dstX dstY size alpha

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
