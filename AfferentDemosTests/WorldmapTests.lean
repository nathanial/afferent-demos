import Crucible
import Worldmap
import Tileset

namespace AfferentDemosTests.Worldmap

open Crucible

private def mkState (zoom : Int) (parentDepth childDepth : Nat) : IO Worldmap.MapState := do
  let config : Worldmap.MapStateConfig := {
    lat := 0.0
    lon := 0.0
    zoom := zoom
    width := 800
    height := 600
    provider := Tileset.TileProvider.default
    fallbackParentDepth := parentDepth
    fallbackChildDepth := childDepth
  }
  Worldmap.MapState.init config

testSuite "Worldmap"

test "requested zoom range excludes child zoom" := do
  let state ← mkState 5 2 3
  let (minZoom, maxZoom) := Worldmap.requestedZoomRange state
  minZoom ≡ 3
  maxZoom ≡ 5

test "zoom priority starts at target" := do
  let order := Worldmap.zoomPriority 5 3 7
  order ≡ [5, 4, 6, 3, 7]

#generate_tests

end AfferentDemosTests.Worldmap
