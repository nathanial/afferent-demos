/-
  Map Input Handling (Pan, Zoom, and Keyboard Navigation)
  Extracted from Afferent to Worldmap
  Uses Afferent.FFI.Window for input
-/
import Worldmap.State
import Worldmap.Zoom
import Worldmap.Utils
import Worldmap.KeyCode
import Tileset
import Afferent.FFI.Window

namespace Worldmap

open Afferent.FFI
open Worldmap.Zoom (screenToGeo)
open Tileset (clampZoom clampLatitude intToFloat)

/-- Pan speed in pixels per key press -/
def keyboardPanSpeed : Float := 100.0

/-- Check if left mouse button is down from button mask -/
def isLeftButtonDown (buttons : UInt8) : Bool :=
  (buttons &&& 1) != 0

def isInsideViewport (state : MapState) (x y : Float) : Bool :=
  x >= 0.0 &&
  y >= 0.0 &&
  x <= (intToFloat state.viewport.screenWidth) &&
  y <= (intToFloat state.viewport.screenHeight)

/-- Update cursor geographic position from screen position -/
def updateCursorPosition (window : Window) (state : MapState) : IO MapState := do
  let (mouseX, mouseY) ← Window.getMousePos window
  let (cursorLat, cursorLon) := screenToGeo state.viewport mouseX mouseY
  pure { state with
    cursorLat := cursorLat
    cursorLon := cursorLon
    cursorScreenX := mouseX
    cursorScreenY := mouseY
  }

def updateCursorPositionAt (window : Window) (state : MapState)
    (offsetX offsetY : Float) : IO MapState := do
  let (rawX, rawY) ← Window.getMousePos window
  let mouseX := rawX - offsetX
  let mouseY := rawY - offsetY
  if isInsideViewport state mouseX mouseY then
    let (cursorLat, cursorLon) := screenToGeo state.viewport mouseX mouseY
    pure { state with
      cursorLat := cursorLat
      cursorLon := cursorLon
      cursorScreenX := mouseX
      cursorScreenY := mouseY
    }
  else
    pure state

/-- Handle mouse input for panning (respects map bounds) -/
def handlePanInput (window : Window) (state : MapState) : IO MapState := do
  let (mouseX, mouseY) ← Window.getMousePos window
  let buttons ← Window.getMouseButtons window
  let leftDown := isLeftButtonDown buttons

  if leftDown then
    if state.isDragging then
      -- Continue dragging - update center based on delta
      let dx := state.dragStartX - mouseX
      let dy := state.dragStartY - mouseY
      let (dLon, dLat) := state.viewport.pixelsToDegrees dx dy
      -- Apply global and bounds constraints
      let newLat := state.mapBounds.clampLat (clampLatitude (state.dragStartLat - dLat))
      let newLon := state.mapBounds.clampLon (state.dragStartLon + dLon)
      pure { state with
        viewport := { state.viewport with centerLat := newLat, centerLon := newLon }
      }
    else
      -- Start dragging
      pure (state.startDrag mouseX mouseY)
  else
    -- Not dragging
    pure state.stopDrag

def handlePanInputAt (window : Window) (state : MapState)
    (offsetX offsetY : Float) : IO MapState := do
  let (rawX, rawY) ← Window.getMousePos window
  let mouseX := rawX - offsetX
  let mouseY := rawY - offsetY
  let inside := isInsideViewport state mouseX mouseY
  let buttons ← Window.getMouseButtons window
  let leftDown := isLeftButtonDown buttons

  if leftDown then
    if state.isDragging then
      let dx := state.dragStartX - mouseX
      let dy := state.dragStartY - mouseY
      let (dLon, dLat) := state.viewport.pixelsToDegrees dx dy
      let newLat := state.mapBounds.clampLat (clampLatitude (state.dragStartLat - dLat))
      let newLon := state.mapBounds.clampLon (state.dragStartLon + dLon)
      pure { state with
        viewport := { state.viewport with centerLat := newLat, centerLon := newLon }
      }
    else if inside then
      pure (state.startDrag mouseX mouseY)
    else
      pure state
  else
    pure state.stopDrag

/-- Handle mouse wheel for zooming at cursor position.
    Starts zoom animation - the geographic point under the cursor stays fixed. -/
def handleZoomInput (window : Window) (state : MapState) : IO MapState := do
  let (_, wheelY) ← Window.getScrollDelta window
  if wheelY != 0.0 then
    -- `getScrollDelta` reports accumulated scroll since last clear; consume it exactly once.
    Window.clearScroll window
    let (mouseX, mouseY) ← Window.getMousePos window
    let delta := if wheelY > 0.0 then 1 else -1
    -- Accumulate: add delta to current target (not viewport.zoom)
    -- Respect both global zoom limits and map bounds
    let newTarget := state.mapBounds.clampZoom (clampZoom (state.targetZoom + delta))

    if state.isAnimatingZoom then
      -- Already animating: just update target, keep existing anchor, reset debounce
      pure { state with
        targetZoom := newTarget
        lastZoomChangeFrame := state.frameCount
      }
    else
      -- Not animating: capture anchor point and start animation
      -- Get geographic coordinates of cursor position
      let (anchorLat, anchorLon) := screenToGeo state.viewport mouseX mouseY
      pure { state with
        targetZoom := newTarget
        isAnimatingZoom := true
        zoomAnchorScreenX := mouseX
        zoomAnchorScreenY := mouseY
        zoomAnchorLat := anchorLat
        zoomAnchorLon := anchorLon
        lastZoomChangeFrame := state.frameCount
      }
  else
    pure state

def handleZoomInputAt (window : Window) (state : MapState)
    (offsetX offsetY : Float) : IO MapState := do
  let (_, wheelY) ← Window.getScrollDelta window
  if wheelY != 0.0 then
    Window.clearScroll window
    let (rawX, rawY) ← Window.getMousePos window
    let mouseX := rawX - offsetX
    let mouseY := rawY - offsetY
    if !isInsideViewport state mouseX mouseY then
      pure state
    else
      let delta := if wheelY > 0.0 then 1 else -1
      let newTarget := state.mapBounds.clampZoom (clampZoom (state.targetZoom + delta))
      if state.isAnimatingZoom then
        pure { state with
          targetZoom := newTarget
          lastZoomChangeFrame := state.frameCount
        }
      else
        let (anchorLat, anchorLon) := screenToGeo state.viewport mouseX mouseY
        pure { state with
          targetZoom := newTarget
          isAnimatingZoom := true
          zoomAnchorScreenX := mouseX
          zoomAnchorScreenY := mouseY
          zoomAnchorLat := anchorLat
          zoomAnchorLon := anchorLon
          lastZoomChangeFrame := state.frameCount
        }
  else
    pure state

/-- Handle keyboard input for navigation.
    - Arrow keys: pan the map
    - +/=: zoom in (centered)
    - -: zoom out (centered)
    - Home: reset to initial view
    - 0-9: jump to zoom level -/
def handleKeyboardInput (window : Window) (state : MapState) : IO MapState := do
  let keyCode ← Window.getKeyCode window
  if keyCode == 0 then
    -- No key pressed
    pure state
  else
    -- Consume the key press
    Window.clearKey window
    -- Arrow key panning
    if keyCode == KeyCode.arrowUp then
      let (_, dLat) := state.viewport.pixelsToDegrees 0.0 (-keyboardPanSpeed)
      let newLat := state.mapBounds.clampLat (clampLatitude (state.viewport.centerLat - dLat))
      pure { state with viewport := { state.viewport with centerLat := newLat } }
    else if keyCode == KeyCode.arrowDown then
      let (_, dLat) := state.viewport.pixelsToDegrees 0.0 keyboardPanSpeed
      let newLat := state.mapBounds.clampLat (clampLatitude (state.viewport.centerLat - dLat))
      pure { state with viewport := { state.viewport with centerLat := newLat } }
    else if keyCode == KeyCode.arrowLeft then
      let (dLon, _) := state.viewport.pixelsToDegrees (-keyboardPanSpeed) 0.0
      let newLon := state.mapBounds.clampLon (state.viewport.centerLon + dLon)
      pure { state with viewport := { state.viewport with centerLon := newLon } }
    else if keyCode == KeyCode.arrowRight then
      let (dLon, _) := state.viewport.pixelsToDegrees keyboardPanSpeed 0.0
      let newLon := state.mapBounds.clampLon (state.viewport.centerLon + dLon)
      pure { state with viewport := { state.viewport with centerLon := newLon } }
    -- Zoom in (+/=)
    else if keyCode == KeyCode.equal then
      let newZoom := state.mapBounds.clampZoom (clampZoom (state.viewport.zoom + 1))
      pure (state.setZoom newZoom)
    -- Zoom out (-)
    else if keyCode == KeyCode.minus then
      let newZoom := state.mapBounds.clampZoom (clampZoom (state.viewport.zoom - 1))
      pure (state.setZoom newZoom)
    -- Home key: reset to initial view
    else if keyCode == KeyCode.home then
      pure state.resetToInitial
    -- Number keys: jump to zoom level
    else
      match KeyCode.toZoomLevel keyCode with
      | some zoom =>
        let clampedZoom := state.mapBounds.clampZoom (clampZoom zoom)
        pure (state.setZoom clampedZoom)
      | none => pure state

/-- Combined input handler -/
def handleInput (window : Window) (state : MapState) : IO MapState := do
  let state ← handlePanInput window state
  let state ← handleZoomInput window state
  let state ← handleKeyboardInput window state
  updateCursorPosition window state

def handleInputAt (window : Window) (state : MapState)
    (offsetX offsetY : Float) : IO MapState := do
  let state ← handlePanInputAt window state offsetX offsetY
  let state ← handleZoomInputAt window state offsetX offsetY
  let state ← handleKeyboardInput window state
  updateCursorPositionAt window state offsetX offsetY

end Worldmap
