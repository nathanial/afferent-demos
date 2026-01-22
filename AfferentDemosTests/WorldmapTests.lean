import Crucible
import Worldmap
import Tileset

namespace AfferentDemosTests.Worldmap

open Crucible
open Std (HashSet)

private def zoomRange (state : Worldmap.MapState) : (Int × Int) :=
  let parentDepth := Tileset.natToInt state.fallbackParentDepth
  let childDepth := Tileset.natToInt state.fallbackChildDepth
  let minZoom := Tileset.intClamp (state.viewport.zoom - parentDepth)
    state.tileProvider.minZoom state.tileProvider.maxZoom
  let maxZoom := Tileset.intClamp (state.viewport.zoom + childDepth)
    state.tileProvider.minZoom state.tileProvider.maxZoom
  (state.mapBounds.clampZoom minZoom, state.mapBounds.clampZoom maxZoom)

private def requestedTileSet (state : Worldmap.MapState) (buffer : Int) : HashSet Tileset.TileCoord :=
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

private def candidateTileSet (state : Worldmap.MapState) (buffer : Int) : HashSet Tileset.TileCoord :=
  let (minZoom, maxZoom) := zoomRange state
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


private def mkStateWithBounds (zoom : Int) (parentDepth childDepth : Nat)
    (bounds : Worldmap.MapBounds) : IO Worldmap.MapState := do
  let config : Worldmap.MapStateConfig := {
    lat := 0.0
    lon := 0.0
    zoom := zoom
    width := 800
    height := 600
    provider := Tileset.TileProvider.default
    bounds := bounds
    fallbackParentDepth := parentDepth
    fallbackChildDepth := childDepth
  }
  Worldmap.MapState.init config

private def mkState (zoom : Int) (parentDepth childDepth : Nat) : IO Worldmap.MapState := do
  mkStateWithBounds zoom parentDepth childDepth Worldmap.MapBounds.world

testSuite "Worldmap"

test "requested zoom range uses target zoom only" := do
  let state ← mkState 5 2 3
  let (minZoom, maxZoom) := Worldmap.requestedZoomRange state
  minZoom ≡ 5
  maxZoom ≡ 5

test "zoom priority starts at target" := do
  let order := Worldmap.zoomPriority 5 3 7
  order ≡ [5, 4, 6, 3, 7]

test "requested zoom range clamps to bounds" := do
  let state ← mkStateWithBounds 3 2 1 Worldmap.MapBounds.usa
  let (minZoom, maxZoom) := Worldmap.requestedZoomRange state
  minZoom ≡ 3
  maxZoom ≡ 3

test "zoom priority clamps at range edges" := do
  let orderLow := Worldmap.zoomPriority 3 3 5
  orderLow ≡ [3, 4, 5]
  let orderHigh := Worldmap.zoomPriority 5 3 5
  orderHigh ≡ [5, 4, 3]

test "zoom priority covers range without duplicates" := do
  let order := Worldmap.zoomPriority 4 2 6
  order.length ≡ 5
  let uniq := order.foldl (fun s z => s.insert z) ({} : Std.HashSet Int)
  uniq.size ≡ order.length
  ensure (order.all (fun z => z >= 2 && z <= 6)) "order should stay within bounds"

test "requested tiles are subset of keep tiles" := do
  let state ← mkState 5 2 1
  let requested := requestedTileSet state 1
  let keep := candidateTileSet state 3
  for coord in requested.toList do
    ensure (keep.contains coord) "keep set should include requested tiles"

test "candidate set includes parent fallback tiles" := do
  let state ← mkState 3 1 0
  let set := candidateTileSet state 0
  let center := Tileset.latLonToTile { lat := 0.0, lon := 0.0 } 3
  ensure (set.contains center) "candidate set should include center tile"
  ensure (set.contains center.parentTile) "candidate set should include parent tile"

test "candidate set includes child fallback tiles" := do
  let state ← mkState 3 0 1
  let set := candidateTileSet state 0
  let center := Tileset.latLonToTile { lat := 0.0, lon := 0.0 } 3
  let child := center.childTiles[0]!
  ensure (set.contains child) "candidate set should include child tile"

test "candidate set grows with fallback depth" := do
  let baseState ← mkState 3 0 0
  let withParents ← mkState 3 2 0
  let withChildren ← mkState 3 0 2
  let baseSet := candidateTileSet baseState 0
  let parentSet := candidateTileSet withParents 0
  let childSet := candidateTileSet withChildren 0
  ensure (parentSet.size >= baseSet.size) "parent fallback should not shrink set"
  ensure (childSet.size >= baseSet.size) "child fallback should not shrink set"

test "request set excludes child zooms" := do
  let state ← mkState 6 2 2
  let requested := requestedTileSet state 1
  for coord in requested.toList do
    ensure (coord.z <= state.viewport.zoom) "request set should not include child zooms"

test "zoom in adds higher zoom requests" := do
  let state ← mkState 4 2 1
  let before := requestedTileSet state 1
  let state' := state.setZoom 5
  let after := requestedTileSet state' 1
  let hasNew := after.toList.any (fun coord => coord.z == 5 && !before.contains coord)
  ensure hasNew "zoom-in should introduce target zoom tiles"

test "zoom out removes higher zoom requests" := do
  let state ← mkState 5 2 1
  let state' := state.setZoom 4
  let requested := requestedTileSet state' 1
  for coord in requested.toList do
    ensure (coord.z <= 4) "zoom-out should not request higher zoom tiles"

#generate_tests

end AfferentDemosTests.Worldmap
