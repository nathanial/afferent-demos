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
  let stateRef ← SpiderM.liftIO do
    let state ← Worldmap.MapState.init config
    IO.mkRef state
  let mapName ← registerComponentW "worldmap-map"
  let sidebarName ← registerComponentW "worldmap-sidebar" (isInteractive := false)
  let layoutRef ← SpiderM.liftIO (IO.mkRef (none : Option Trellis.ComputedLayout))

  let updateLayout := fun (nameMap : Std.HashMap String Afferent.Arbor.WidgetId)
      (layouts : Trellis.LayoutResult) => do
        match nameMap.get? mapName with
        | some wid =>
            match layouts.get wid with
            | some layout => layoutRef.set (some layout)
            | none => pure ()
        | none => pure ()

  let updateScreenSize := fun (state : Worldmap.MapState) (layout : Trellis.ComputedLayout) =>
    let w := max 1.0 layout.contentRect.width
    let h := max 1.0 layout.contentRect.height
    state.updateScreenSize w.toUInt32.toNat h.toUInt32.toNat

  let updateCursor := fun (state : Worldmap.MapState) (x y : Float) =>
    if Worldmap.isInsideViewport state x y then
      let (cursorLat, cursorLon) := Worldmap.Zoom.screenToGeo state.viewport x y
      { state with
        cursorLat := cursorLat
        cursorLon := cursorLon
        cursorScreenX := x
        cursorScreenY := y
      }
    else
      state

  let updateDrag := fun (state : Worldmap.MapState) (x y : Float) =>
    if state.isDragging then
      let dx := state.dragStartX - x
      let dy := state.dragStartY - y
      let (dLon, dLat) := state.viewport.pixelsToDegrees dx dy
      let newLat := state.mapBounds.clampLat (Tileset.clampLatitude (state.dragStartLat - dLat))
      let newLon := state.mapBounds.clampLon (state.dragStartLon + dLon)
      { state with viewport := { state.viewport with centerLat := newLat, centerLon := newLon } }
    else
      state

  let clickEvents ← useClickData mapName
  let clickAction ← Event.mapM (fun data => do
    updateLayout data.nameMap data.layouts
    match data.nameMap.get? mapName with
    | some wid =>
        match data.layouts.get wid with
        | some layout =>
            let x := data.click.x - layout.contentRect.x
            let y := data.click.y - layout.contentRect.y
            let mut state ← stateRef.get
            state := updateScreenSize state layout
            if data.click.button == 0 && Worldmap.isInsideViewport state x y then
              state := state.startDrag x y
            stateRef.set state
        | none => pure ()
    | none => pure ()
    ) clickEvents
  performEvent_ clickAction

  let mouseUpEvents ← useAllMouseUp
  let mouseUpAction ← Event.mapM (fun data => do
    if data.button == 0 then
      stateRef.modify (·.stopDrag)
    ) mouseUpEvents
  performEvent_ mouseUpAction

  let hoverEvents ← useAllHovers
  let hoverAction ← Event.mapM (fun data => do
    updateLayout data.nameMap data.layouts
    match data.nameMap.get? mapName with
    | some wid =>
        match data.layouts.get wid with
        | some layout =>
            let x := data.x - layout.contentRect.x
            let y := data.y - layout.contentRect.y
            let mut state ← stateRef.get
            state := updateScreenSize state layout
            state := updateCursor state x y
            state := updateDrag state x y
            stateRef.set state
        | none => pure ()
    | none => pure ()
    ) hoverEvents
  performEvent_ hoverAction

  let scrollEvents ← useScroll mapName
  let scrollAction ← Event.mapM (fun data => do
    updateLayout data.nameMap data.layouts
    match data.nameMap.get? mapName with
    | some wid =>
        match data.layouts.get wid with
        | some layout =>
            let wheelY := data.scroll.deltaY
            if wheelY != 0.0 then
              let x := data.scroll.x - layout.contentRect.x
              let y := data.scroll.y - layout.contentRect.y
              let mut state ← stateRef.get
              state := updateScreenSize state layout
              if Worldmap.isInsideViewport state x y then
                let delta := if wheelY > 0.0 then 1 else -1
                let newTarget := state.mapBounds.clampZoom (Tileset.clampZoom (state.targetZoom + delta))
                if state.isAnimatingZoom then
                  state := { state with
                    targetZoom := newTarget
                    lastZoomChangeFrame := state.frameCount
                  }
                else
                  let (anchorLat, anchorLon) := Worldmap.Zoom.screenToGeo state.viewport x y
                  state := { state with
                    targetZoom := newTarget
                    isAnimatingZoom := true
                    zoomAnchorScreenX := x
                    zoomAnchorScreenY := y
                    zoomAnchorLat := anchorLat
                    zoomAnchorLon := anchorLon
                    lastZoomChangeFrame := state.frameCount
                  }
              stateRef.set state
        | none => pure ()
    | none => pure ()
    ) scrollEvents
  performEvent_ scrollAction

  let keyEvents ← useKeyboard
  let keyAction ← Event.mapM (fun data => do
    let key := data.event.key
    let mut state ← stateRef.get
    match key with
    | .up =>
        let (_, dLat) := state.viewport.pixelsToDegrees 0.0 (-Worldmap.keyboardPanSpeed)
        let newLat := state.mapBounds.clampLat (Tileset.clampLatitude (state.viewport.centerLat - dLat))
        state := { state with viewport := { state.viewport with centerLat := newLat } }
    | .down =>
        let (_, dLat) := state.viewport.pixelsToDegrees 0.0 Worldmap.keyboardPanSpeed
        let newLat := state.mapBounds.clampLat (Tileset.clampLatitude (state.viewport.centerLat - dLat))
        state := { state with viewport := { state.viewport with centerLat := newLat } }
    | .left =>
        let (dLon, _) := state.viewport.pixelsToDegrees (-Worldmap.keyboardPanSpeed) 0.0
        let newLon := state.mapBounds.clampLon (state.viewport.centerLon + dLon)
        state := { state with viewport := { state.viewport with centerLon := newLon } }
    | .right =>
        let (dLon, _) := state.viewport.pixelsToDegrees Worldmap.keyboardPanSpeed 0.0
        let newLon := state.mapBounds.clampLon (state.viewport.centerLon + dLon)
        state := { state with viewport := { state.viewport with centerLon := newLon } }
    | .home =>
        state := state.resetToInitial
    | .char '=' =>
        let newZoom := state.mapBounds.clampZoom (Tileset.clampZoom (state.viewport.zoom + 1))
        state := state.setZoom newZoom
    | .char '-' =>
        let newZoom := state.mapBounds.clampZoom (Tileset.clampZoom (state.viewport.zoom - 1))
        state := state.setZoom newZoom
    | .char c =>
        if c.isDigit then
          let zoom := (c.toNat - '0'.toNat)
          let newZoom := state.mapBounds.clampZoom (Tileset.clampZoom (Int.ofNat zoom))
          state := state.setZoom newZoom
    | _ => pure ()
    stateRef.set state
    ) keyEvents
  performEvent_ keyAction

  let _ ← dynWidget elapsedTime fun _ => do
    let mut state ← SpiderM.liftIO stateRef.get
    if let some layout ← SpiderM.liftIO layoutRef.get then
      state := updateScreenSize state layout
    state ← requestWorldmapTiles state manager
    state ← SpiderM.liftIO (Worldmap.updateFrame state manager)
    SpiderM.liftIO (stateRef.set state)
    let (windowW, windowH) ← SpiderM.liftIO do
      let (w, h) ← FFI.Window.getSize env.window
      pure (w.toFloat, h.toFloat)
    emit (pure (worldmapWidgetNamed env.screenScale env.fontMedium env.fontSmall windowW windowH state
      mapName sidebarName))
  pure ()

end Demos
