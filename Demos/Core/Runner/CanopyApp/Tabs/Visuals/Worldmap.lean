/-
  Demo Runner - Canopy app visuals Worldmap tab content.
-/
import Reactive
import Afferent
import Afferent.Canopy
import Afferent.Canopy.Reactive
import Demos.Core.Demo
import Demos.Visuals.Worldmap
import Tileset
import Worldmap
import Trellis

open Reactive Reactive.Host
open Afferent
open Afferent.Arbor
open Afferent.Canopy
open Afferent.Canopy.Reactive
open Trellis

namespace Demos

structure WorldmapFullState where
  mapState : Worldmap.MapState
  layout : Option Trellis.ComputedLayout := none

namespace WorldmapFullState

def updateScreenSize (s : WorldmapFullState) (layout : Trellis.ComputedLayout) : WorldmapFullState :=
  let w := max 1.0 layout.contentRect.width
  let h := max 1.0 layout.contentRect.height
  { s with
    mapState := s.mapState.updateScreenSize w.toUInt32.toNat h.toUInt32.toNat
    layout := some layout
  }

def updateCursor (s : WorldmapFullState) (x y : Float) : WorldmapFullState :=
  if Worldmap.isInsideViewport s.mapState x y then
    let (cursorLat, cursorLon) := Worldmap.Zoom.screenToGeo s.mapState.viewport x y
    { s with mapState := { s.mapState with
        cursorLat := cursorLat
        cursorLon := cursorLon
        cursorScreenX := x
        cursorScreenY := y
      }
    }
  else
    s

def updateDrag (s : WorldmapFullState) (x y : Float) : WorldmapFullState :=
  if s.mapState.isDragging then
    let dx := s.mapState.dragStartX - x
    let dy := s.mapState.dragStartY - y
    let (dLon, dLat) := s.mapState.viewport.pixelsToDegrees dx dy
    let newLat := s.mapState.mapBounds.clampLat (Tileset.clampLatitude (s.mapState.dragStartLat - dLat))
    let newLon := s.mapState.mapBounds.clampLon (s.mapState.dragStartLon + dLon)
    { s with mapState := { s.mapState with viewport := { s.mapState.viewport with centerLat := newLat, centerLon := newLon } } }
  else
    s

end WorldmapFullState

def worldmapTabContent (env : DemoEnv) (manager : Tileset.TileManager) : WidgetM Unit := do
  let elapsedTime ← useElapsedTime
  let config : Worldmap.MapStateConfig := {
    lat := 37.7749
    lon := -122.4194
    zoom := 12
    width := env.physWidth.toNat
    height := env.physHeight.toNat
    provider := Tileset.TileProvider.cartoDarkRetina
    fallbackParentDepth := 3
    fallbackChildDepth := 2
    fadeFrames := 12
    persistentFallbackZoom := some 1
  }
  let initialMapState ← SpiderM.liftIO (Worldmap.MapState.init config)
  let mapName ← registerComponentW "worldmap-map"
  let sidebarName ← registerComponentW "worldmap-sidebar" (isInteractive := false)

  let clickEvents ← useClickData mapName
  let mouseUpEvents ← useAllMouseUp
  let hoverEvents ← useAllHovers
  let scrollEvents ← useScroll mapName
  let keyEvents ← useKeyboard

  -- Helper to extract layout from name map and layouts
  let getLayout := fun (nameMap : Std.HashMap String Afferent.Arbor.WidgetId)
                       (layouts : Trellis.LayoutResult) =>
    match nameMap.get? mapName with
    | some wid => layouts.get wid
    | none => none

  -- Map click events to state update functions (start drag)
  let clickUpdates ← Event.mapM (fun data =>
    fun (s : WorldmapFullState) =>
      match getLayout data.nameMap data.layouts with
      | some layout =>
          let x := data.click.x - layout.contentRect.x
          let y := data.click.y - layout.contentRect.y
          let s := s.updateScreenSize layout
          if data.click.button == 0 && Worldmap.isInsideViewport s.mapState x y then
            { s with mapState := s.mapState.startDrag x y }
          else
            s
      | none => s
    ) clickEvents

  -- Map mouse up events to state update functions (stop drag)
  let mouseUpUpdates ← Event.mapM (fun data =>
    fun (s : WorldmapFullState) =>
      if data.button == 0 then
        { s with mapState := s.mapState.stopDrag }
      else
        s
    ) mouseUpEvents

  -- Map hover events to state update functions (cursor + drag)
  let hoverUpdates ← Event.mapM (fun data =>
    fun (s : WorldmapFullState) =>
      match getLayout data.nameMap data.layouts with
      | some layout =>
          let x := data.x - layout.contentRect.x
          let y := data.y - layout.contentRect.y
          let s := s.updateScreenSize layout
          let s := s.updateCursor x y
          s.updateDrag x y
      | none => s
    ) hoverEvents

  -- Map scroll events to state update functions (zoom)
  let scrollUpdates ← Event.mapM (fun data =>
    fun (s : WorldmapFullState) =>
      match getLayout data.nameMap data.layouts with
      | some layout =>
          let wheelY := data.scroll.deltaY
          if wheelY != 0.0 then
            let x := data.scroll.x - layout.contentRect.x
            let y := data.scroll.y - layout.contentRect.y
            let s := s.updateScreenSize layout
            if Worldmap.isInsideViewport s.mapState x y then
              let delta := if wheelY > 0.0 then 1 else -1
              let newTarget := s.mapState.mapBounds.clampZoom (Tileset.clampZoom (s.mapState.targetZoom + delta))
              if s.mapState.isAnimatingZoom then
                { s with mapState := { s.mapState with
                    targetZoom := newTarget
                    lastZoomChangeFrame := s.mapState.frameCount
                  }
                }
              else
                let (anchorLat, anchorLon) := Worldmap.Zoom.screenToGeo s.mapState.viewport x y
                { s with mapState := { s.mapState with
                    targetZoom := newTarget
                    isAnimatingZoom := true
                    zoomAnchorScreenX := x
                    zoomAnchorScreenY := y
                    zoomAnchorLat := anchorLat
                    zoomAnchorLon := anchorLon
                    lastZoomChangeFrame := s.mapState.frameCount
                  }
                }
            else
              s
          else
            s
      | none => s
    ) scrollEvents

  -- Map keyboard events to state update functions (pan/zoom via keys)
  let keyUpdates ← Event.mapM (fun data =>
    let key := data.event.key
    fun (s : WorldmapFullState) =>
      match key with
      | .up =>
          let (_, dLat) := s.mapState.viewport.pixelsToDegrees 0.0 (-Worldmap.keyboardPanSpeed)
          let newLat := s.mapState.mapBounds.clampLat (Tileset.clampLatitude (s.mapState.viewport.centerLat - dLat))
          { s with mapState := { s.mapState with viewport := { s.mapState.viewport with centerLat := newLat } } }
      | .down =>
          let (_, dLat) := s.mapState.viewport.pixelsToDegrees 0.0 Worldmap.keyboardPanSpeed
          let newLat := s.mapState.mapBounds.clampLat (Tileset.clampLatitude (s.mapState.viewport.centerLat - dLat))
          { s with mapState := { s.mapState with viewport := { s.mapState.viewport with centerLat := newLat } } }
      | .left =>
          let (dLon, _) := s.mapState.viewport.pixelsToDegrees (-Worldmap.keyboardPanSpeed) 0.0
          let newLon := s.mapState.mapBounds.clampLon (s.mapState.viewport.centerLon + dLon)
          { s with mapState := { s.mapState with viewport := { s.mapState.viewport with centerLon := newLon } } }
      | .right =>
          let (dLon, _) := s.mapState.viewport.pixelsToDegrees Worldmap.keyboardPanSpeed 0.0
          let newLon := s.mapState.mapBounds.clampLon (s.mapState.viewport.centerLon + dLon)
          { s with mapState := { s.mapState with viewport := { s.mapState.viewport with centerLon := newLon } } }
      | .home =>
          { s with mapState := s.mapState.resetToInitial }
      | .char '=' =>
          let newZoom := s.mapState.mapBounds.clampZoom (Tileset.clampZoom (s.mapState.viewport.zoom + 1))
          { s with mapState := s.mapState.setZoom newZoom }
      | .char '-' =>
          let newZoom := s.mapState.mapBounds.clampZoom (Tileset.clampZoom (s.mapState.viewport.zoom - 1))
          { s with mapState := s.mapState.setZoom newZoom }
      | .char c =>
          if c.isDigit then
            let zoom := (c.toNat - '0'.toNat)
            let newZoom := s.mapState.mapBounds.clampZoom (Tileset.clampZoom (Int.ofNat zoom))
            { s with mapState := s.mapState.setZoom newZoom }
          else
            s
      | _ => s
    ) keyEvents

  -- Map time updates to identity function (just triggers re-render)
  let timeUpdates ← Event.mapM (fun _ =>
    fun (s : WorldmapFullState) => s
    ) elapsedTime.updated

  -- Merge all updates and fold into state
  let allUpdates ← Event.mergeAllListM [clickUpdates, mouseUpUpdates, hoverUpdates, scrollUpdates, keyUpdates, timeUpdates]
  let initialState : WorldmapFullState := { mapState := initialMapState }
  let state ← foldDyn (fun f s => f s) initialState allUpdates

  let _ ← dynWidget state fun s => do
    -- Apply layout update if available
    let mut mapState := s.mapState
    if let some layout := s.layout then
      mapState := s.updateScreenSize layout |>.mapState
    -- Request tiles and update frame (IO operations)
    mapState ← requestWorldmapTiles mapState manager
    mapState ← SpiderM.liftIO (Worldmap.updateFrame mapState manager)
    let (windowW, windowH) ← SpiderM.liftIO do
      let (w, h) ← FFI.Window.getSize env.window
      pure (w.toFloat, h.toFloat)
    emit (pure (worldmapWidgetNamed env.screenScale env.fontMedium env.fontSmall windowW windowH mapState
      mapName sidebarName))
  pure ()

end Demos
