/-
  Complete Map State
  Uses tileset library for tile management and TextureCache for GPU textures
-/
import Tileset
import Worldmap.TextureCache
import Worldmap.Utils
import Std.Data.HashMap

namespace Worldmap

open Tileset (TileProvider TileManager TileManagerConfig MapViewport TileCoord TileLoadState)
open Tileset (MapBounds intToFloat clampZoom clampLatitude intClamp)
open Worldmap (ZoomAnimationConfig defaultZoomAnimationConfig)
open Reactive.Host (Dyn SpiderM)
open Std (HashMap)

/-- Complete map state -/
structure MapState where
  viewport : MapViewport
  -- GPU texture management
  textureCache : TextureCache
  -- Reactive tile dynamics (coord → reactive tile state)
  tileDynamics : IO.Ref (HashMap TileCoord (Dyn TileLoadState))
  frameCount : Nat := 0
  -- Drag state
  isDragging : Bool := false
  dragStartX : Float := 0.0
  dragStartY : Float := 0.0
  dragStartLat : Float := 0.0
  dragStartLon : Float := 0.0
  -- Zoom animation state
  targetZoom : Int
  displayZoom : Float
  zoomAnchorScreenX : Float := 0.0
  zoomAnchorScreenY : Float := 0.0
  zoomAnchorLat : Float := 0.0
  zoomAnchorLon : Float := 0.0
  isAnimatingZoom : Bool := false
  -- Initial view for Home key reset
  initialLat : Float
  initialLon : Float
  initialZoom : Int
  -- Cursor position
  cursorLat : Float := 0.0
  cursorLon : Float := 0.0
  cursorScreenX : Float := 0.0
  cursorScreenY : Float := 0.0
  -- Tile provider configuration
  tileProvider : TileProvider := TileProvider.default
  -- Zoom animation configuration
  zoomAnimationConfig : ZoomAnimationConfig := defaultZoomAnimationConfig
  -- Map bounds constraints
  mapBounds : MapBounds := MapBounds.world
  -- Zoom debouncing
  lastZoomChangeFrame : Nat := 0
  zoomDebounceFrames : Nat := 6
  -- Fallback rendering configuration
  fallbackParentDepth : Nat := 2
  fallbackChildDepth : Nat := 1
  fadeFrames : Nat := 12

/-- Initialization configuration for MapState -/
structure MapStateConfig where
  /-- Initial latitude -/
  lat : Float
  /-- Initial longitude -/
  lon : Float
  /-- Initial zoom level -/
  zoom : Int
  /-- Screen width in pixels -/
  width : Int
  /-- Screen height in pixels -/
  height : Int
  /-- Tile provider -/
  provider : TileProvider := TileProvider.default
  /-- Zoom animation configuration -/
  zoomConfig : ZoomAnimationConfig := defaultZoomAnimationConfig
  /-- Map bounds -/
  bounds : MapBounds := MapBounds.world
  /-- Maximum GPU textures to cache -/
  maxGpuTextures : Nat := 256
  /-- How many parent zoom levels to keep/render as fallback -/
  fallbackParentDepth : Nat := 2
  /-- How many child zoom levels to keep/render as fallback -/
  fallbackChildDepth : Nat := 1
  /-- Frames to cross-fade when higher-detail tiles appear -/
  fadeFrames : Nat := 12
  deriving Inhabited

namespace MapState

/-- Initialize map state centered on a location -/
def init (config : MapStateConfig) : IO MapState := do
  let tileDynamics ← IO.mkRef {}
  let textureCache ← TextureCache.new config.maxGpuTextures
  -- Clamp zoom to provider limits and bounds
  let clampedZoom := config.bounds.clampZoom (intClamp (clampZoom config.zoom) config.provider.minZoom config.provider.maxZoom)
  -- Clamp lat/lon to bounds
  let clampedLat := config.bounds.clampLat (clampLatitude config.lat)
  let clampedLon := config.bounds.clampLon config.lon
  pure {
    viewport := {
      centerLat := clampedLat
      centerLon := clampedLon
      zoom := clampedZoom
      screenWidth := config.width
      screenHeight := config.height
      tileSize := config.provider.tileSize
    }
    textureCache
    tileDynamics
    targetZoom := clampedZoom
    displayZoom := intToFloat clampedZoom
    initialLat := clampedLat
    initialLon := clampedLon
    initialZoom := clampedZoom
    tileProvider := config.provider
    zoomAnimationConfig := config.zoomConfig
    mapBounds := config.bounds
    fallbackParentDepth := config.fallbackParentDepth
    fallbackChildDepth := config.fallbackChildDepth
    fadeFrames := config.fadeFrames
  }

/-- Change the tile provider (clears GPU texture cache since tiles are different) -/
def setProvider (state : MapState) (provider : TileProvider) : IO MapState := do
  let clampedZoom := state.mapBounds.clampZoom (intClamp state.viewport.zoom provider.minZoom provider.maxZoom)
  -- Clear GPU textures and dynamics when changing provider
  state.textureCache.clear
  state.tileDynamics.set {}
  pure { state with
    tileProvider := provider
    viewport := { state.viewport with
      zoom := clampedZoom
      tileSize := provider.tileSize
    }
    targetZoom := clampedZoom
    displayZoom := intToFloat clampedZoom
  }

/-- Change the zoom animation configuration -/
def setZoomAnimationConfig (state : MapState) (config : ZoomAnimationConfig) : MapState :=
  { state with zoomAnimationConfig := config }

/-- Change the map bounds (clamps current position if outside new bounds) -/
def setBounds (state : MapState) (bounds : MapBounds) : MapState :=
  let clampedLat := bounds.clampLat state.viewport.centerLat
  let clampedLon := bounds.clampLon state.viewport.centerLon
  let clampedZoom := bounds.clampZoom state.viewport.zoom
  { state with
    mapBounds := bounds
    viewport := { state.viewport with
      centerLat := clampedLat
      centerLon := clampedLon
      zoom := clampedZoom
    }
    targetZoom := clampedZoom
    displayZoom := intToFloat clampedZoom
  }

/-- Update viewport center (respects bounds) -/
def setCenter (state : MapState) (lat lon : Float) : MapState :=
  let clampedLat := state.mapBounds.clampLat (clampLatitude lat)
  let clampedLon := state.mapBounds.clampLon lon
  { state with viewport := { state.viewport with
      centerLat := clampedLat
      centerLon := clampedLon
    }
  }

/-- Update zoom level (respects bounds, also updates animation state) -/
def setZoom (state : MapState) (zoom : Int) : MapState :=
  let clamped := state.mapBounds.clampZoom (clampZoom zoom)
  { state with
      viewport := { state.viewport with zoom := clamped }
      targetZoom := clamped
      displayZoom := intToFloat clamped
      isAnimatingZoom := false
  }

/-- Update viewport screen dimensions (for window resize) -/
def updateScreenSize (state : MapState) (width height : Nat) : MapState :=
  { state with viewport := { state.viewport with
      screenWidth := width
      screenHeight := height
    }
  }

/-- Start dragging -/
def startDrag (state : MapState) (mouseX mouseY : Float) : MapState :=
  { state with
    isDragging := true
    dragStartX := mouseX
    dragStartY := mouseY
    dragStartLat := state.viewport.centerLat
    dragStartLon := state.viewport.centerLon
  }

/-- Stop dragging -/
def stopDrag (state : MapState) : MapState :=
  { state with isDragging := false }

/-- Reset to initial view (for Home key) -/
def resetToInitial (state : MapState) : MapState :=
  { state with
    viewport := { state.viewport with
      centerLat := state.initialLat
      centerLon := state.initialLon
      zoom := state.initialZoom
    }
    targetZoom := state.initialZoom
    displayZoom := intToFloat state.initialZoom
    isAnimatingZoom := false
  }

/-- Increment frame counter -/
def tick (state : MapState) : MapState :=
  { state with frameCount := state.frameCount + 1 }

end MapState

end Worldmap
