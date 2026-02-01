/-
  Demo Runner - Canopy app visuals tab content.
-/
import Reactive
import Afferent
import Afferent.Canopy
import Afferent.Canopy.Reactive
import Demos.Core.Demo
import Demos.Visuals.Seascape
import Demos.Visuals.ShapeGallery
import Demos.Visuals.DashedLines
import Demos.Visuals.LineCaps
import Demos.Visuals.TextureMatrix
import Demos.Visuals.Orbital
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

structure SeascapeInputState where
  w : Bool := false
  a : Bool := false
  s : Bool := false
  d : Bool := false
  q : Bool := false
  e : Bool := false
  deriving Inhabited

def textureMatrixTabContent (env : DemoEnv) (elapsedTime : Dynamic Spider Float) : WidgetM Unit := do
  let _ ← dynWidget elapsedTime fun t => do
    emit (pure (textureMatrixWidget t env.screenScale env.windowWidthF env.windowHeightF
      env.fontMedium env.fontSmall env.spriteTexture))
  pure ()

def orbitalInstancedTabContent (env : DemoEnv) (elapsedTime : Dynamic Spider Float) : WidgetM Unit := do
  let _ ← dynWidget elapsedTime fun t => do
    emit (pure (orbitalInstancedWidget t env.screenScale env.windowWidthF env.windowHeightF
      env.fontMedium env.orbitalCount env.orbitalParams env.orbitalBuffer))
  pure ()

def seascapeTabContent (env : DemoEnv) (elapsedTime : Dynamic Spider Float)
    (stateRef : IO.Ref SeascapeState) (lastTimeRef : IO.Ref Float)
    (keysRef : IO.Ref SeascapeInputState) (lockRef : IO.Ref Bool)
    (deltaRef : IO.Ref Afferent.Canopy.Reactive.MouseDeltaData) : WidgetM Unit := do
  let seascapeName ← registerComponentW "seascape"
  let clickEvents ← useClick seascapeName
  let clickAction ← Event.mapM (fun _ => do
    let locked ← lockRef.get
    if !locked then
      FFI.Window.setPointerLock env.window true
      lockRef.set true
    ) clickEvents
  performEvent_ clickAction

  let keyEvents ← useKeyboardAll
  let keyAction ← Event.mapM (fun data => do
    let key := data.event.key
    let isPress := data.event.isPress
    if key == .escape && isPress then
      let locked ← lockRef.get
      FFI.Window.setPointerLock env.window (!locked)
      lockRef.set (!locked)
    match key with
    | .char 'w' => keysRef.modify fun s => { s with w := isPress }
    | .char 'a' => keysRef.modify fun s => { s with a := isPress }
    | .char 's' => keysRef.modify fun s => { s with s := isPress }
    | .char 'd' => keysRef.modify fun s => { s with d := isPress }
    | .char 'q' => keysRef.modify fun s => { s with q := isPress }
    | .char 'e' => keysRef.modify fun s => { s with e := isPress }
    | _ => pure ()
    ) keyEvents
  performEvent_ keyAction

  let mouseDeltas ← useMouseDelta
  let deltaAction ← Event.mapM (fun delta => deltaRef.set delta) mouseDeltas
  performEvent_ deltaAction

  let _ ← dynWidget elapsedTime fun t => do
    let state ← SpiderM.liftIO do
      let lastT ← lastTimeRef.get
      let dt := if lastT == 0.0 then 0.0 else max 0.0 (t - lastT)
      let current ← stateRef.get
      let locked ← lockRef.get
      let keys ← keysRef.get
      let delta ← deltaRef.get
      let dx := if locked then delta.dx else 0.0
      let dy := if locked then delta.dy else 0.0
      let camera := current.camera.update dt keys.w keys.s keys.a keys.d keys.e keys.q dx dy
      let next := { current with camera := camera, locked := locked }
      stateRef.set next
      lastTimeRef.set t
      pure next
    let (windowW, windowH) ← SpiderM.liftIO do
      let (w, h) ← FFI.Window.getSize env.window
      pure (w.toFloat, h.toFloat)
    let containerStyle : BoxStyle := {
      flexItem := some (FlexItem.growing 1)
      width := .percent 1.0
      height := .percent 1.0
    }
    emit (pure (namedColumn seascapeName 0 containerStyle #[
      seascapeWidget t env.screenScale windowW windowH env.fontMedium env.fontSmall state
    ]))
  pure ()

def shapeGalleryTabContent (env : DemoEnv) (elapsedTime : Dynamic Spider Float)
    (indexRef : IO.Ref Nat) : WidgetM Unit := do
  let keyEvents ← useKeyboard
  let keyAction ← Event.mapM (fun data => do
    let total := shapeGalleryCount
    if total == 0 then
      pure ()
    else
      match data.event.key with
      | .right | .space =>
          indexRef.modify fun idx => (idx + 1) % total
      | .left =>
          indexRef.modify fun idx => if idx == 0 then total - 1 else idx - 1
      | _ => pure ()
    ) keyEvents
  performEvent_ keyAction

  let _ ← dynWidget elapsedTime fun _ => do
    let idx ← SpiderM.liftIO indexRef.get
    emit (pure (shapeGalleryWidget idx env.screenScale env.fontLarge env.fontSmall env.fontMedium))
  pure ()

def worldmapTabContent (env : DemoEnv) (elapsedTime : Dynamic Spider Float)
    (stateRef : IO.Ref Worldmap.MapState) (tileManager : Tileset.TileManager) : WidgetM Unit := do
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
    state ← requestWorldmapTiles state tileManager
    state ← SpiderM.liftIO (Worldmap.updateFrame state tileManager)
    SpiderM.liftIO (stateRef.set state)
    let (windowW, windowH) ← SpiderM.liftIO do
      let (w, h) ← FFI.Window.getSize env.window
      pure (w.toFloat, h.toFloat)
    emit (pure (worldmapWidgetNamed env.screenScale env.fontMedium env.fontSmall windowW windowH state
      mapName sidebarName))
  pure ()

def lineCapsTabContent (env : DemoEnv) (elapsedTime : Dynamic Spider Float) : WidgetM Unit := do
  let _ ← dynWidget elapsedTime fun _ => do
    emit (pure (lineCapsWidget env.screenScale env.fontSmall env.fontMedium))
  pure ()

def dashedLinesTabContent (env : DemoEnv) (elapsedTime : Dynamic Spider Float) : WidgetM Unit := do
  let _ ← dynWidget elapsedTime fun _ => do
    emit (pure (dashedLinesWidget env.screenScale env.fontSmall env.fontMedium))
  pure ()

end Demos
