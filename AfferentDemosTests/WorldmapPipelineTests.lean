import Crucible
import Worldmap
import Tileset
import Reactive

namespace AfferentDemosTests.WorldmapPipeline

open Crucible
open Std (HashSet)
open Reactive.Host (SpiderEnv)

private def makeProvider (tileSize : Int) : Tileset.TileProvider := {
  name := "Test Tiles"
  urlTemplate := "http://127.0.0.1:9/{z}/{x}/{y}.png"
  subdomains := #["a"]
  tileSize := tileSize
  maxZoom := 19
  minZoom := 0
  attribution := ""
}

private def freshCacheDir : IO String := do
  let stamp ← IO.monoMsNow
  pure s!"/tmp/tileset_worldmap_pipeline_{stamp}"

private def buildState (lat lon : Float) (zoom : Int)
    (provider : Tileset.TileProvider) (parentDepth childDepth : Nat) : IO Worldmap.MapState := do
  let config : Worldmap.MapStateConfig := {
    lat := lat
    lon := lon
    zoom := zoom
    width := 128
    height := 128
    provider := provider
    fallbackParentDepth := parentDepth
    fallbackChildDepth := childDepth
  }
  Worldmap.MapState.init config

private def requestedTileSet (state : Worldmap.MapState) (buffer : Int)
    : HashSet Tileset.TileCoord :=
  let (minZoom, maxZoom) := Worldmap.requestedZoomRange state
  let minZ := minZoom.toNat
  let maxZ := maxZoom.toNat
  Id.run do
    let mut set : HashSet Tileset.TileCoord := {}
    for z in [minZ:maxZ+1] do
      let zInt := Tileset.natToInt z
      let vp := { state.viewport with zoom := zInt }
      let tiles := vp.visibleTilesWithBuffer buffer
      for coord in tiles do
        set := set.insert coord
    return set

private def collectScenarioTiles (provider : Tileset.TileProvider)
    (steps : List (Float × Float × Int)) (parentDepth childDepth : Nat) : IO (HashSet Tileset.TileCoord) := do
  let mut set : HashSet Tileset.TileCoord := {}
  for (lat, lon, zoom) in steps do
    let state ← buildState lat lon zoom provider parentDepth childDepth
    let requested := requestedTileSet state 1
    for coord in requested.toList do
      set := set.insert coord
  pure set

private def tileTopLeftAtDisplayZoom (vp : Worldmap.MapViewport) (tile : Tileset.TileCoord)
    (displayZoom : Float) : (Float × Float) :=
  let n := Float.pow 2.0 displayZoom
  let centerTileX := (vp.centerLon + 180.0) / 360.0 * n
  let latRad := vp.centerLat * Tileset.pi / 180.0
  let centerTileY := (1.0 - Float.log (Float.tan latRad + 1.0 / Float.cos latRad) / Tileset.pi) / 2.0 * n
  let scale := Float.pow 2.0 (displayZoom - Tileset.intToFloat tile.z)
  let tileX := Tileset.intToFloat tile.x * scale
  let tileY := Tileset.intToFloat tile.y * scale
  let offsetX := (tileX - centerTileX) * (Tileset.intToFloat vp.tileSize) +
    (Tileset.intToFloat vp.screenWidth) / 2.0
  let offsetY := (tileY - centerTileY) * (Tileset.intToFloat vp.tileSize) +
    (Tileset.intToFloat vp.screenHeight) / 2.0
  (offsetX, offsetY)

private def tileIntersectsViewportAtDisplayZoom (vp : Worldmap.MapViewport) (displayZoom : Float)
    (buffer : Int) (tile : Tileset.TileCoord) : Bool :=
  let scale := Float.pow 2.0 (displayZoom - Tileset.intToFloat tile.z)
  let tileSize := Tileset.intToFloat vp.tileSize * scale
  let bufferPx := Tileset.intToFloat buffer * tileSize
  let viewLeft := -bufferPx
  let viewTop := -bufferPx
  let viewRight := (Tileset.intToFloat vp.screenWidth) + bufferPx
  let viewBottom := (Tileset.intToFloat vp.screenHeight) + bufferPx
  let (x, y) := tileTopLeftAtDisplayZoom vp tile displayZoom
  let tileRight := x + tileSize
  let tileBottom := y + tileSize
  tileRight > viewLeft && x < viewRight && tileBottom > viewTop && y < viewBottom

private def expectedVisibleTilesAtDisplayZoom (state : Worldmap.MapState) (buffer : Int)
    : HashSet Tileset.TileCoord :=
  let count := (Tileset.tilesAtZoom state.viewport.zoom).toNat
  Id.run do
    let mut set : HashSet Tileset.TileCoord := {}
    for y in [0:count] do
      for x in [0:count] do
        let tile : Tileset.TileCoord := {
          x := Tileset.natToInt x
          y := Tileset.natToInt y
          z := state.viewport.zoom
        }
        if tileIntersectsViewportAtDisplayZoom state.viewport state.displayZoom buffer tile then
          set := set.insert tile
    return set

private def firstStep (steps : List (Float × Float × Int)) : (Float × Float × Int) :=
  match steps with
  | (lat, lon, zoom) :: _ => (lat, lon, zoom)
  | [] => (0.0, 0.0, 4)

private def lastStep (steps : List (Float × Float × Int)) : (Float × Float × Int) :=
  let rec go : List (Float × Float × Int) → (Float × Float × Int)
    | [x] => x
    | _ :: xs => go xs
    | [] => (0.0, 0.0, 4)
  go steps

private def allReady (state : Worldmap.MapState) (coords : List Tileset.TileCoord) : IO Bool := do
  let dynamics ← state.tileDynamics.get
  for coord in coords do
    match dynamics[coord]? with
    | none => return false
    | some dyn =>
        let loadState ← dyn.sample
        match loadState with
        | .ready _ => pure ()
        | _ => return false
  pure true

private def allUploaded (state : Worldmap.MapState) (coords : List Tileset.TileCoord) : IO Bool := do
  for coord in coords do
    if !(← state.textureCache.has coord) then
      return false
  pure true

private def waitUntil (timeoutMs : Nat) (stepMs : Nat := 20)
    (check : IO Bool) : IO Bool := do
  let stepMs := if stepMs == 0 then 1 else stepMs
  let steps := timeoutMs / stepMs
  let rec loop : Nat → IO Bool
    | 0 => check
    | n + 1 => do
        if (← check) then
          pure true
        else
          IO.sleep stepMs.toUInt32
          loop n
  loop steps

testSuite "WorldmapPipeline"

test "rapid pan/zoom eventually uploads target tiles" (timeout := 20000) := do
  let tileSize := 32
  let provider := makeProvider tileSize
  let cacheDir ← freshCacheDir
  let diskConfig := Tileset.DiskCacheConfig.fromProvider provider cacheDir (200 * 1024 * 1024)
  let img := Raster.Image.create tileSize.toNat tileSize.toNat .rgba [50, 80, 120, 255]
  let bytes ← Raster.Image.encode img .png
  let steps : List (Float × Float × Int) := [
    (0.0, 0.0, 4),
    (10.0, 5.0, 4),
    (10.0, 5.0, 5),
    (-5.0, 15.0, 4),
    (0.0, 0.0, 4)
  ]
  let parentDepth : Nat := 1
  let childDepth : Nat := 0
  let coords ← collectScenarioTiles provider steps parentDepth childDepth
  for coord in coords.toList do
    Tileset.DiskCache.write diskConfig coord bytes
  let env ← SpiderEnv.new
  let config : Tileset.TileManagerConfig := {
    provider := provider
    diskCacheDir := cacheDir
    httpTimeout := 500
    retryConfig := { maxRetries := 0, baseDelayMs := 1 }
    workerCount := 2
  }
  let mgr ← (Tileset.TileManager.new config).run env
  let (initialLat, initialLon, initialZoom) := firstStep steps
  let (finalLat, finalLon, finalZoom) := lastStep steps
  let mut state ← buildState initialLat initialLon initialZoom provider parentDepth childDepth
  try
    for (lat, lon, zoom) in steps do
      state := state.setCenter lat lon
      state := state.setZoom zoom
      state ← (Worldmap.requestVisibleTiles state mgr).run env
      for _ in [:3] do
        state ← Worldmap.updateFrame state mgr
    state := state.setCenter finalLat finalLon
    state := state.setZoom finalZoom
    state ← (Worldmap.requestVisibleTiles state mgr).run env
    let targetTiles := state.viewport.visibleTilesWithBuffer 1
    let mut readyOk := false
    for _ in [:200] do
      state ← (Worldmap.requestVisibleTiles state mgr).run env
      state ← Worldmap.updateFrame state mgr
      if (← allReady state targetTiles) then
        readyOk := true
        break
      IO.sleep 20
    ensure readyOk "expected target tiles to reach ready state"
    let mut uploadedOk := false
    for _ in [:200] do
      state ← Worldmap.uploadReadyTiles state (maxUploads := 100)
      if (← allUploaded state targetTiles) then
        uploadedOk := true
        break
      IO.sleep 20
    ensure uploadedOk "expected all target tiles uploaded (no persistent fallback)"
  finally
    Tileset.TileManager.shutdown mgr
    env.currentScope.dispose

test "canceled requests recover on re-request" := do
  let tileSize := 32
  let provider := makeProvider tileSize
  let cacheDir ← freshCacheDir
  let diskConfig := Tileset.DiskCacheConfig.fromProvider provider cacheDir (200 * 1024 * 1024)
  let img := Raster.Image.create tileSize.toNat tileSize.toNat .rgba [120, 60, 40, 255]
  let bytes ← Raster.Image.encode img .png
  let env ← SpiderEnv.new
  let config : Tileset.TileManagerConfig := {
    provider := provider
    diskCacheDir := cacheDir
    httpTimeout := 500
    retryConfig := { maxRetries := 0, baseDelayMs := 1 }
    workerCount := 1
  }
  let mgr ← (Tileset.TileManager.new config).run env
  let mut state ← buildState 0.0 0.0 4 provider 0 0
  let requested := requestedTileSet state 1
  let visible := state.viewport.visibleTilesWithBuffer 1
  for coord in requested.toList do
    Tileset.DiskCache.write diskConfig coord bytes
  try
    state ← (Worldmap.requestVisibleTiles state mgr).run env
    -- Simulate cancellation while dynamics remain in the map.
    let keepSet : HashSet Tileset.TileCoord := {}
    Tileset.TileManager.evictDistant mgr keepSet
    -- Re-request visible tiles; dynamics already exist so no new request is issued.
    state ← (Worldmap.requestVisibleTiles state mgr).run env
    let mut readyOk := false
    for _ in [:200] do
      state ← (Worldmap.requestVisibleTiles state mgr).run env
      if (← allReady state visible) then
        readyOk := true
        break
      IO.sleep 10
    ensure readyOk "expected canceled tiles to recover when re-requested"
  finally
    Tileset.TileManager.shutdown mgr
    env.currentScope.dispose

test "requestVisibleTiles stays within viewport coverage at fractional zoom" := do
  let tileSize := 256
  let provider := makeProvider tileSize
  let cacheDir ← freshCacheDir
  let env ← SpiderEnv.new
  let config : Tileset.TileManagerConfig := {
    provider := provider
    diskCacheDir := cacheDir
    httpTimeout := 50
    retryConfig := { maxRetries := 0, baseDelayMs := 1 }
    workerCount := 1
  }
  let mgr ← (Tileset.TileManager.new config).run env
  let mut state ← buildState 0.0 0.0 3 provider 0 0
  state := state.updateScreenSize 512 512
  state := { state with
    displayZoom := 3.75
    targetZoom := 4
    requestBudget := 1000
  }
  try
    state ← (Worldmap.requestVisibleTiles state mgr).run env
    let dynamics ← state.tileDynamics.get
    let requested : HashSet Tileset.TileCoord :=
      dynamics.toList.foldl (fun s (coord, _) => s.insert coord) {}
    let expected := expectedVisibleTilesAtDisplayZoom state 1
    for coord in requested.toList do
      ensure (expected.contains coord)
        s!"requested tile {coord} outside viewport coverage"
    for coord in expected.toList do
      ensure (requested.contains coord)
        s!"missing requested tile {coord} for viewport coverage"
  finally
    Tileset.TileManager.shutdown mgr
    env.currentScope.dispose



end AfferentDemosTests.WorldmapPipeline
