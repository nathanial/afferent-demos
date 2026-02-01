/-
  Demo Runner - Canopy app shell for demo tabs.
-/
import Reactive
import Afferent
import Afferent.Canopy
import Afferent.Canopy.Reactive
import Demos.Core.Demo
import Demos.Core.DemoRegistry
import Demos.Overview.Card
import Demos.Overview.DemoGrid
import Demos.Overview.SpinningCubes
import Demos.Layout.Flexbox
import Demos.Layout.CssGrid
import Demos.Reactive.Showcase.App
import Demos.Perf.Circles
import Demos.Perf.Lines
import Demos.Perf.Sprites
import Demos.Perf.Widget.App
import Demos.Visuals.Seascape
import Demos.Visuals.ShapeGallery
import Demos.Visuals.DashedLines
import Demos.Visuals.LineCaps
import Demos.Visuals.TextureMatrix
import Demos.Visuals.Orbital
import Demos.Overview.Fonts
import Demos.Chat.App
import Demos.Visuals.Worldmap
import Demos.Linalg.VectorInterpolation
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

structure CanopyAppState where
  render : ComponentRender

private structure SeascapeInputState where
  w : Bool := false
  a : Bool := false
  s : Bool := false
  d : Bool := false
  q : Bool := false
  e : Bool := false
  deriving Inhabited

private def roundTo (v : Float) (places : Nat) : Float :=
  let factor := (10 : Float) ^ places.toFloat
  (v * factor).round / factor

private def formatFloat (v : Float) (places : Nat := 2) : String :=
  let s := toString (roundTo v places)
  if s.any (· == '.') then
    let s := s.dropRightWhile (· == '0')
    if s.endsWith "." then s.dropRight 1 else s
  else
    s

private def formatStatsLines (stats : RunnerStats) : Array String :=
  #[
    s!"layout {formatFloat stats.layoutMs}ms • collect {formatFloat stats.collectMs}ms • exec {formatFloat stats.executeMs}ms",
    s!"cmds {stats.commandCount} • widgets {stats.widgetCount} • layouts {stats.layoutCount}",
    s!"draws {stats.drawCalls} • batched {stats.batchedCalls} • single {stats.individualCalls}",
    s!"cache hits {stats.cacheHits} • misses {stats.cacheMisses}",
    s!"frame {formatFloat stats.frameMs}ms • {formatFloat stats.fps 1} fps"
  ]

private def demoFontsFromEnv (env : DemoEnv) : DemoFonts := {
  label := env.fontSmallId
  small := env.fontSmallId
  medium := env.fontMediumId
  large := env.fontLargeId
  huge := env.fontHugeId
}

private def overviewTabContent (env : DemoEnv) (elapsedTime : Dynamic Spider Float) : WidgetM Unit := do
  let demoFonts := demoFontsFromEnv env
  let cubes := spinningCubesInitialState
  let _ ← dynWidget elapsedTime fun t => do
    emit (pure (demoGridWidget env.screenScale t demoFonts cubes env.windowWidthF env.windowHeightF))
  pure ()

private def circlesTabContent (env : DemoEnv) (elapsedTime : Dynamic Spider Float)
    (particlesRef : IO.Ref Render.Dynamic.ParticleState)
    (lastTimeRef : IO.Ref Float) : WidgetM Unit := do
  let _ ← dynWidget elapsedTime fun t => do
    let particles ← SpiderM.liftIO do
      let lastT ← lastTimeRef.get
      let dt := if lastT == 0.0 then 0.0 else max 0.0 (t - lastT)
      let current ← particlesRef.get
      let next := current.updateBouncing dt env.circleRadius
      particlesRef.set next
      lastTimeRef.set t
      pure next
    emit (pure (circlesPerfWidget t env.fontMedium particles env.circleRadius))
  pure ()

private def spritesTabContent (env : DemoEnv) (elapsedTime : Dynamic Spider Float)
    (particlesRef : IO.Ref Render.Dynamic.ParticleState)
    (lastTimeRef : IO.Ref Float) : WidgetM Unit := do
  let _ ← dynWidget elapsedTime fun t => do
    let particles ← SpiderM.liftIO do
      let lastT ← lastTimeRef.get
      let dt := if lastT == 0.0 then 0.0 else max 0.0 (t - lastT)
      let current ← particlesRef.get
      let next := current.updateBouncing dt env.spriteHalfSize
      particlesRef.set next
      lastTimeRef.set t
      pure next
    emit (pure (spritesPerfWidget env.screenScale env.fontMedium env.spriteTexture particles env.spriteHalfSize))
  pure ()

private def linesPerfTabContent (env : DemoEnv) (elapsedTime : Dynamic Spider Float) : WidgetM Unit := do
  let _ ← dynWidget elapsedTime fun t => do
    emit (pure (linesPerfWidget t env.lineBuffer env.lineCount env.lineWidth
      env.fontMedium env.windowWidthF env.windowHeightF))
  pure ()

private def layoutTabContent (env : DemoEnv) (elapsedTime : Dynamic Spider Float) : WidgetM Unit := do
  let _ ← dynWidget elapsedTime fun _ => do
    emit (pure (layoutWidgetFlex env.fontMediumId env.fontSmallId env.screenScale))
  pure ()

private def cssGridTabContent (env : DemoEnv) (elapsedTime : Dynamic Spider Float) : WidgetM Unit := do
  let _ ← dynWidget elapsedTime fun _ => do
    emit (pure (cssGridWidget env.fontMediumId env.fontSmallId env.screenScale))
  pure ()

private def reactiveShowcaseTabContent (appState : ReactiveShowcase.AppState) : WidgetM Unit := do
  emit appState.render

private def widgetPerfTabContent (appState : WidgetPerf.AppState) : WidgetM Unit := do
  emit appState.render

private def fontShowcaseTabContent (env : DemoEnv) (elapsedTime : Dynamic Spider Float) : WidgetM Unit := do
  let _ ← dynWidget elapsedTime fun _ => do
    emit (pure (fontShowcaseWidget env.showcaseFonts env.fontMediumId env.screenScale))
  pure ()

private def chatDemoTabContent (appState : ChatDemo.AppState) : WidgetM Unit := do
  emit appState.render

private def textureMatrixTabContent (env : DemoEnv) (elapsedTime : Dynamic Spider Float) : WidgetM Unit := do
  let _ ← dynWidget elapsedTime fun t => do
    emit (pure (textureMatrixWidget t env.screenScale env.windowWidthF env.windowHeightF
      env.fontMedium env.fontSmall env.spriteTexture))
  pure ()

private def orbitalInstancedTabContent (env : DemoEnv) (elapsedTime : Dynamic Spider Float) : WidgetM Unit := do
  let _ ← dynWidget elapsedTime fun t => do
    emit (pure (orbitalInstancedWidget t env.screenScale env.windowWidthF env.windowHeightF
      env.fontMedium env.orbitalCount env.orbitalParams env.orbitalBuffer))
  pure ()

private def vectorInterpolationTabContent (env : DemoEnv) (elapsedTime : Dynamic Spider Float)
    (stateRef : IO.Ref Demos.Linalg.VectorInterpolationState)
    (lastTimeRef : IO.Ref Float) : WidgetM Unit := do
  let interpName ← registerComponentW "vector-interpolation"

  let clickEvents ← useClickData interpName
  let clickAction ← Event.mapM (fun data => do
    if data.click.button != 0 then
      pure ()
    else
      match data.nameMap.get? interpName with
      | some wid =>
          match data.layouts.get wid with
          | some layout =>
              let rect := layout.contentRect
              let localX := data.click.x - rect.x
              let localY := data.click.y - rect.y
              let origin := (rect.width / 2, rect.height / 2)
              let scale := 50.0 * env.screenScale
              let worldPos := Demos.Linalg.screenToWorld (localX, localY) origin scale
              let state ← stateRef.get
              if Demos.Linalg.nearPoint worldPos state.vectorA 0.5 then
                stateRef.set { state with dragging := some .vectorA }
              else if Demos.Linalg.nearPoint worldPos state.vectorB 0.5 then
                stateRef.set { state with dragging := some .vectorB }
              else
                pure ()
          | none => pure ()
      | none => pure ()
    ) clickEvents
  performEvent_ clickAction

  let hoverEvents ← useAllHovers
  let hoverAction ← Event.mapM (fun data => do
    let state ← stateRef.get
    match state.dragging with
    | some target =>
        match data.nameMap.get? interpName with
        | some wid =>
            match data.layouts.get wid with
            | some layout =>
                let rect := layout.contentRect
                let localX := data.x - rect.x
                let localY := data.y - rect.y
                let origin := (rect.width / 2, rect.height / 2)
                let scale := 50.0 * env.screenScale
                let worldPos := Demos.Linalg.screenToWorld (localX, localY) origin scale
                let next := match target with
                  | .vectorA => { state with vectorA := worldPos }
                  | .vectorB => { state with vectorB := worldPos }
                stateRef.set next
            | none => pure ()
        | none => pure ()
    | none => pure ()
    ) hoverEvents
  performEvent_ hoverAction

  let mouseUpEvents ← useAllMouseUp
  let mouseUpAction ← Event.mapM (fun data => do
    if data.button == 0 then
      stateRef.modify fun s => { s with dragging := none }
    ) mouseUpEvents
  performEvent_ mouseUpAction

  let keyEvents ← useKeyboard
  let keyAction ← Event.mapM (fun data => do
    if data.event.key == .space && data.event.isPress then
      stateRef.modify fun s => { s with animating := !s.animating }
    ) keyEvents
  performEvent_ keyAction

  let _ ← dynWidget elapsedTime fun t => do
    let state ← SpiderM.liftIO do
      let lastT ← lastTimeRef.get
      let dt := if lastT == 0.0 then 0.0 else max 0.0 (t - lastT)
      let mut current ← stateRef.get
      if current.animating then
        let nextT := current.t + dt * 0.5
        let wrapped := if nextT >= 1.0 then nextT - 1.0 else nextT
        current := { current with t := wrapped }
        stateRef.set current
      lastTimeRef.set t
      pure current
    let containerStyle : BoxStyle := {
      flexItem := some (FlexItem.growing 1)
      width := .percent 1.0
      height := .percent 1.0
    }
    emit (pure (namedColumn interpName 0 containerStyle #[
      Demos.Linalg.vectorInterpolationWidget env state
    ]))
  pure ()

private def seascapeTabContent (env : DemoEnv) (elapsedTime : Dynamic Spider Float)
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

private def shapeGalleryTabContent (env : DemoEnv) (elapsedTime : Dynamic Spider Float)
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

private def worldmapTabContent (env : DemoEnv) (elapsedTime : Dynamic Spider Float)
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

private def lineCapsTabContent (env : DemoEnv) (elapsedTime : Dynamic Spider Float) : WidgetM Unit := do
  let _ ← dynWidget elapsedTime fun _ => do
    emit (pure (lineCapsWidget env.screenScale env.fontSmall env.fontMedium))
  pure ()

private def dashedLinesTabContent (env : DemoEnv) (elapsedTime : Dynamic Spider Float) : WidgetM Unit := do
  let _ ← dynWidget elapsedTime fun _ => do
    emit (pure (dashedLinesWidget env.screenScale env.fontSmall env.fontMedium))
  pure ()

private def statsFooter (env : DemoEnv) (elapsedTime : Dynamic Spider Float) : WidgetM Unit := do
  let footerHeight := 110.0 * env.screenScale
  let footerStyle : BoxStyle := {
    backgroundColor := some (Color.gray 0.08)
    padding := EdgeInsets.symmetric (6.0 * env.screenScale) (4.0 * env.screenScale)
    width := .percent 1.0
    height := .length footerHeight
    flexItem := some (FlexItem.fixed footerHeight)
  }
  column' (gap := 2.0 * env.screenScale) (style := footerStyle) do
    let _ ← dynWidget elapsedTime fun _ => do
      let stats ← SpiderM.liftIO env.statsRef.get
      let lines := formatStatsLines stats
      for line in lines do
        caption' line
      pure ()
    pure ()

private def demoStubContent (id : DemoId) : WidgetM Unit := do
  let inst := demoInstance id
  filledPanel' 24 do
    heading2' inst.name
    caption' s!"{inst.shortName} • stub"
    spacer' 0 8
    bodyText' "This demo has not been migrated to Canopy yet."
    bodyText' "Replace this panel with the real demo widget."

/-- Create the demo shell as a single Canopy widget tree. -/
def createCanopyApp (env : DemoEnv) : ReactiveM CanopyAppState := do
  let elapsedTime ← useElapsedTime
  let reactiveShowcaseApp ← ReactiveShowcase.createApp env
  let widgetPerfApp ← WidgetPerf.createApp env
  let chatDemoApp ← ChatDemo.createApp env
  let circlesRef ← SpiderM.liftIO do
    let particles := Render.Dynamic.ParticleState.create 1000000 env.physWidthF env.physHeightF 42
    IO.mkRef particles
  let circlesTimeRef ← SpiderM.liftIO (IO.mkRef 0.0)
  let spritesRef ← SpiderM.liftIO do
    let particles := Render.Dynamic.ParticleState.create 1000000 env.physWidthF env.physHeightF 123
    IO.mkRef particles
  let spritesTimeRef ← SpiderM.liftIO (IO.mkRef 0.0)
  let seascapeRef ← SpiderM.liftIO (IO.mkRef { camera := seascapeCamera })
  let seascapeTimeRef ← SpiderM.liftIO (IO.mkRef 0.0)
  let seascapeKeysRef ← SpiderM.liftIO (IO.mkRef ({} : SeascapeInputState))
  let seascapeLockRef ← SpiderM.liftIO (IO.mkRef false)
  let seascapeDeltaRef ← SpiderM.liftIO (IO.mkRef { dx := 0.0, dy := 0.0 })
  let shapeGalleryIndexRef ← SpiderM.liftIO (IO.mkRef 0)
  let vectorInterpolationRef ← SpiderM.liftIO (IO.mkRef Demos.Linalg.vectorInterpolationInitialState)
  let vectorInterpolationTimeRef ← SpiderM.liftIO (IO.mkRef 0.0)
  let worldmapStateRef ← SpiderM.liftIO do
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
    let state ← Worldmap.MapState.init config
    IO.mkRef state
  let worldmapTileConfig : Tileset.TileManagerConfig := {
    provider := Tileset.TileProvider.cartoDarkRetina
    diskCacheDir := "./tile_cache"
    diskCacheMaxSize := 500 * 1024 * 1024
  }
  let worldmapManager ← Tileset.TileManager.new worldmapTileConfig
  let tabs : Array TabDef := demoIds.map fun id => {
    label := (demoInstance id).shortName
    content := match id with
      | .demoGrid => overviewTabContent env elapsedTime
      | .circlesPerf => circlesTabContent env elapsedTime circlesRef circlesTimeRef
      | .spritesPerf => spritesTabContent env elapsedTime spritesRef spritesTimeRef
      | .linesPerf => linesPerfTabContent env elapsedTime
      | .layout => layoutTabContent env elapsedTime
      | .cssGrid => cssGridTabContent env elapsedTime
      | .reactiveShowcase => reactiveShowcaseTabContent reactiveShowcaseApp
      | .widgetPerf => widgetPerfTabContent widgetPerfApp
      | .seascape => seascapeTabContent env elapsedTime seascapeRef seascapeTimeRef
          seascapeKeysRef seascapeLockRef seascapeDeltaRef
      | .shapeGallery => shapeGalleryTabContent env elapsedTime shapeGalleryIndexRef
      | .worldmap => worldmapTabContent env elapsedTime worldmapStateRef worldmapManager
      | .lineCaps => lineCapsTabContent env elapsedTime
      | .dashedLines => dashedLinesTabContent env elapsedTime
      | .textureMatrix => textureMatrixTabContent env elapsedTime
      | .orbitalInstanced => orbitalInstancedTabContent env elapsedTime
      | .fontShowcase => fontShowcaseTabContent env elapsedTime
      | .chatDemo => chatDemoTabContent chatDemoApp
      | .vectorInterpolation =>
          vectorInterpolationTabContent env elapsedTime vectorInterpolationRef vectorInterpolationTimeRef
      | _ => demoStubContent id
  }

  let (_, render) ← runWidget do
    let rootStyle : BoxStyle := {
      backgroundColor := some (Color.gray 0.08)
      padding := EdgeInsets.uniform 16
      width := .percent 1.0
      height := .percent 1.0
      flexItem := some (FlexItem.growing 1)
    }

    column' (gap := 16) (style := rootStyle) do
      heading1' "Afferent Demos"

      let contentStyle : BoxStyle := {
        flexItem := some (FlexItem.growing 1)
        width := .percent 1.0
        height := .percent 1.0
      }

      column' (gap := 0) (style := contentStyle) do
        let _ ← tabView tabs 0
        pure ()
      statsFooter env elapsedTime

  pure { render := render }

end Demos
