import Crucible
import Worldmap
import Tileset

namespace AfferentDemosTests.Worldmap

open Crucible

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

test "requested zoom range excludes child zoom" := do
  let state ← mkState 5 2 3
  let (minZoom, maxZoom) := Worldmap.requestedZoomRange state
  minZoom ≡ 3
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

#generate_tests

end AfferentDemosTests.Worldmap
