/-
  Demo Runner - Main orchestration for all demos
-/
import Afferent
import Afferent.Arbor
import Afferent.Widget
import Demos.Overview.Text
import Demos.Demo
import Demos.DemoRegistry
import Demos.TabBar
import Wisp
import Init.Data.FloatArray

set_option maxRecDepth 1024

open Afferent CanvasM

namespace Demos

private structure FontPack where
  registry : FontRegistry
  smallId : Afferent.Arbor.FontId
  mediumId : Afferent.Arbor.FontId
  largeId : Afferent.Arbor.FontId
  hugeId : Afferent.Arbor.FontId

private structure LoadingState where
  fontSmall : Option Font := none
  fontMedium : Option Font := none
  fontLarge : Option Font := none
  fontHuge : Option Font := none
  layoutFont : Option Font := none
  fontPack : Option FontPack := none
  spriteTexture : Option FFI.Texture := none
  lineSegments : Option (Array Float × Nat) := none
  lineBuffer : Option FFI.Buffer := none
  gridParticles : Option Render.Dynamic.ParticleState := none
  orbitalParams : Option FloatArray := none
  orbitalBuffer : Option FFI.FloatBuffer := none

private structure LoadedAssets where
  screenScale : Float
  fontSmall : Font
  fontMedium : Font
  fontLarge : Font
  fontHuge : Font
  layoutFont : Font
  fontPack : FontPack
  spriteTexture : FFI.Texture
  halfSize : Float
  circleRadius : Float
  spriteHalfSize : Float
  gridParticles : Render.Dynamic.ParticleState
  lineBuffer : FFI.Buffer
  lineCount : Nat
  lineWidth : Float
  orbitalCount : Nat
  orbitalParams : FloatArray
  orbitalBuffer : FFI.FloatBuffer
  physWidthF : Float
  physHeightF : Float
  physWidth : UInt32
  physHeight : UInt32
  layoutOffsetX : Float
  layoutOffsetY : Float
  layoutScale : Float

/-- Tabbar UI state. -/
private structure TabBarState where
  selectedTab : Nat
  tabIds : Array Afferent.Arbor.WidgetId
  cachedWidget : Option Afferent.Arbor.Widget
  cachedLayouts : Option Trellis.LayoutResult
deriving Inhabited

/-- Fixed tabbar height in logical pixels. -/
def tabBarHeight : Float := ({} : TabBarStyle).height

private structure RunningState where
  assets : LoadedAssets
  demos : Array AnyDemo
  displayMode : Nat
  msaaEnabled : Bool
  frameCount : Nat
  fpsAccumulator : Float
  displayFps : Float
  framesLeft : Nat
  tabBar : TabBarState

private inductive AppState where
  | loading (state : LoadingState)
  | running (state : RunningState)

private def loadingStepsTotal : Nat := 12

private def loadingStepsDone (s : LoadingState) : Nat :=
  (if s.fontSmall.isSome then 1 else 0) +
  (if s.fontMedium.isSome then 1 else 0) +
  (if s.fontLarge.isSome then 1 else 0) +
  (if s.fontHuge.isSome then 1 else 0) +
  (if s.layoutFont.isSome then 1 else 0) +
  (if s.fontPack.isSome then 1 else 0) +
  (if s.spriteTexture.isSome then 1 else 0) +
  (if s.lineSegments.isSome then 1 else 0) +
  (if s.lineBuffer.isSome then 1 else 0) +
  (if s.gridParticles.isSome then 1 else 0) +
  (if s.orbitalParams.isSome then 1 else 0) +
  (if s.orbitalBuffer.isSome then 1 else 0)

private def loadingProgress (s : LoadingState) : Float :=
  (loadingStepsDone s).toFloat / (loadingStepsTotal.toFloat)

private def loadingStatus (s : LoadingState) : String :=
  if s.fontSmall.isNone || s.fontMedium.isNone || s.fontLarge.isNone || s.fontHuge.isNone then
    "Loading fonts..."
  else if s.layoutFont.isNone then
    "Preparing layout font..."
  else if s.fontPack.isNone then
    "Registering fonts..."
  else if s.spriteTexture.isNone then
    "Loading sprites..."
  else if s.lineSegments.isNone then
    "Generating lines..."
  else if s.lineBuffer.isNone then
    "Uploading line buffer..."
  else if s.gridParticles.isNone then
    "Generating particles..."
  else if s.orbitalParams.isNone then
    "Preparing orbitals..."
  else if s.orbitalBuffer.isNone then
    "Uploading orbitals..."
  else
    "Finalizing..."

private def buildOrbitalParams (orbitalCount : Nat)
    (minRadius maxRadius speedMin speedMax sizeMin sizeMax : Float) : FloatArray := Id.run do
  let twoPi : Float := 6.283185307
  let mut arr := FloatArray.emptyWithCapacity (orbitalCount * 5)
  let mut s := 4242
  for i in [:orbitalCount] do
    s := (s * 1103515245 + 12345) % (2^31)
    let phase := (s.toFloat / 2147483648.0) * twoPi
    s := (s * 1103515245 + 12345) % (2^31)
    let radius := minRadius + (s.toFloat / 2147483648.0) * (maxRadius - minRadius)
    s := (s * 1103515245 + 12345) % (2^31)
    let baseSpeed := speedMin + (s.toFloat / 2147483648.0) * (speedMax - speedMin)
    s := (s * 1103515245 + 12345) % (2^31)
    let dir : Float := if s % 2 == 0 then 1.0 else -1.0
    let speed := baseSpeed * dir
    s := (s * 1103515245 + 12345) % (2^31)
    let size := sizeMin + (s.toFloat / 2147483648.0) * (sizeMax - sizeMin)
    let hue := i.toFloat / orbitalCount.toFloat
    arr := arr.push phase
    arr := arr.push radius
    arr := arr.push speed
    arr := arr.push hue
    arr := arr.push size
  arr

private def spriteHalfSizeFromTexture (texture : FFI.Texture) : IO Float := do
  let (w, h) ← FFI.Texture.getSize texture
  let side : UInt32 := if w ≤ h then w else h
  pure (side.toFloat / 2.0)

private def renderLoading (c : Canvas) (t : Float) (screenScale : Float)
    (progress : Float) (label : String) (font? : Option Font) : IO Canvas := do
  let c' ← run' c do
    resetTransform
    let (w, h) ← getCurrentSize
    setFillColor (Color.gray 0.12)
    fillRectXYWH 0 0 w h
    let barW := min (w * 0.6) (420.0 * screenScale)
    let barH := max (h * 0.02) (12.0 * screenScale)
    let x := (w - barW) / 2.0
    let y := (h - barH) / 2.0
    setFillColor (Color.gray 0.22)
    fillRectXYWH x y barW barH
    let hue := (t * 0.08) - (t * 0.08).floor
    setFillColor (Color.hsva hue 0.55 0.9 1.0)
    fillRectXYWH x y (barW * progress) barH
    let radius := min w h * 0.08
    let angle := t * 2.2
    let dotSize := max (6.0 * screenScale) (barH * 0.6)
    let dotX := w * 0.5 + Float.cos angle * radius
    let dotY := h * 0.5 - Float.sin angle * radius
    setFillColor (Color.gray 0.8)
    fillRectXYWH (dotX - dotSize / 2) (dotY - dotSize / 2) dotSize dotSize
    if let some font := font? then
      let (textW, _) ← measureText label font
      setFillColor (Color.gray 0.75)
      fillTextXY label ((w - textW) / 2.0) (y - 12.0 * screenScale) font
  pure c'

private def advanceLoading (s0 : LoadingState) (screenScale : Float) (canvas : Canvas)
    (lineRef : IO.Ref (Option (Array Float × Nat)))
    (gridRef : IO.Ref (Option Render.Dynamic.ParticleState))
    (orbitalRef : IO.Ref (Option FloatArray))
    (orbitalCount : Nat) : IO LoadingState := do
  let mut s := s0
  if s.lineSegments.isNone then
    if let some segs ← lineRef.get then
      s := { s with lineSegments := some segs }
  if s.gridParticles.isNone then
    if let some grid ← gridRef.get then
      s := { s with gridParticles := some grid }
  if s.orbitalParams.isNone then
    if let some params ← orbitalRef.get then
      s := { s with orbitalParams := some params }

  if s.fontSmall.isNone then
    let fontSmall ← Font.load "/System/Library/Fonts/Monaco.ttf" (16 * screenScale).toUInt32
    return { s with fontSmall := some fontSmall }
  if s.fontMedium.isNone then
    let fontMedium ← Font.load "/System/Library/Fonts/Monaco.ttf" (24 * screenScale).toUInt32
    return { s with fontMedium := some fontMedium }
  if s.fontLarge.isNone then
    let fontLarge ← Font.load "/System/Library/Fonts/Monaco.ttf" (36 * screenScale).toUInt32
    return { s with fontLarge := some fontLarge }
  if s.fontHuge.isNone then
    let fontHuge ← Font.load "/System/Library/Fonts/Monaco.ttf" (48 * screenScale).toUInt32
    return { s with fontHuge := some fontHuge }
  if s.layoutFont.isNone then
    let layoutLabelPt : Float := 12.0
    let layoutFontPx : UInt32 := (max 8.0 (layoutLabelPt * screenScale)).toUInt32
    let layoutFont ← Font.load "/System/Library/Fonts/Monaco.ttf" layoutFontPx
    return { s with layoutFont := some layoutFont }
  if s.fontPack.isNone then
    match s.fontSmall, s.fontMedium, s.fontLarge, s.fontHuge with
    | some fontSmall, some fontMedium, some fontLarge, some fontHuge =>
        let (reg1, fontSmallId) := FontRegistry.empty.register fontSmall "small"
        let (reg2, fontMediumId) := reg1.register fontMedium "medium"
        let (reg3, fontLargeId) := reg2.register fontLarge "large"
        let (reg4, fontHugeId) := reg3.register fontHuge "huge"
        let registry := reg4.setDefault fontMedium
        return { s with fontPack := some {
          registry := registry
          smallId := fontSmallId
          mediumId := fontMediumId
          largeId := fontLargeId
          hugeId := fontHugeId
        } }
    | _, _, _, _ => return s
  if s.spriteTexture.isNone then
    let spriteTexture ← FFI.Texture.load "nibble-32.png"
    return { s with spriteTexture := some spriteTexture }
  if s.lineBuffer.isNone then
    match s.lineSegments with
    | some (segments, _) =>
        let lineBuffer ← FFI.Buffer.createStrokeSegmentPersistent canvas.ctx.renderer segments
        return { s with lineBuffer := some lineBuffer }
    | none => pure ()
  if s.orbitalBuffer.isNone then
    match s.orbitalParams with
    | some _ =>
        let orbitalBuffer ← FFI.FloatBuffer.create (orbitalCount.toUSize * 8)
        return { s with orbitalBuffer := some orbitalBuffer }
    | none => pure ()
  return s

private def toLoadedAssets (s : LoadingState)
    (screenScale halfSize circleRadius : Float)
    (lineWidth : Float)
    (orbitalCount : Nat)
    (physWidthF physHeightF : Float)
    (physWidth physHeight : UInt32)
    (layoutOffsetX layoutOffsetY layoutScale : Float)
    : IO (Option LoadedAssets) := do
  match s.fontSmall, s.fontMedium, s.fontLarge, s.fontHuge, s.layoutFont,
        s.fontPack, s.spriteTexture, s.gridParticles, s.lineSegments,
        s.lineBuffer, s.orbitalParams, s.orbitalBuffer with
  | some fontSmall, some fontMedium, some fontLarge, some fontHuge, some layoutFont,
    some fontPack, some spriteTexture, some gridParticles, some (_, lineCount),
    some lineBuffer, some orbitalParams, some orbitalBuffer =>
      let spriteHalfSize ← spriteHalfSizeFromTexture spriteTexture
      pure (some {
        screenScale
        fontSmall
        fontMedium
        fontLarge
        fontHuge
        layoutFont
        fontPack
        spriteTexture
        halfSize
        circleRadius
        spriteHalfSize
        gridParticles
        lineBuffer
        lineCount
        lineWidth
        orbitalCount
        orbitalParams
        orbitalBuffer
        physWidthF
        physHeightF
        physWidth
        physHeight
        layoutOffsetX
        layoutOffsetY
        layoutScale
      })
  | _, _, _, _, _, _, _, _, _, _, _, _ => pure none

private def cleanupLoading (s : LoadingState) : IO Unit := do
  if let some font := s.fontSmall then font.destroy
  if let some font := s.fontMedium then font.destroy
  if let some font := s.fontLarge then font.destroy
  if let some font := s.fontHuge then font.destroy
  if let some font := s.layoutFont then font.destroy
  if let some tex := s.spriteTexture then FFI.Texture.destroy tex
  if let some buf := s.lineBuffer then FFI.Buffer.destroy buf
  if let some buf := s.orbitalBuffer then FFI.FloatBuffer.destroy buf

private def cleanupAssets (a : LoadedAssets) : IO Unit := do
  a.fontSmall.destroy
  a.fontMedium.destroy
  a.fontLarge.destroy
  a.fontHuge.destroy
  a.layoutFont.destroy
  FFI.Texture.destroy a.spriteTexture
  FFI.Buffer.destroy a.lineBuffer
  FFI.FloatBuffer.destroy a.orbitalBuffer

private def mkEnvFromAssets (a : LoadedAssets) (t dt : Float) (keyCode : UInt16) : DemoEnv := {
  screenScale := a.screenScale
  t := t
  dt := dt
  keyCode := keyCode
  fontSmall := a.fontSmall
  fontMedium := a.fontMedium
  fontLarge := a.fontLarge
  fontHuge := a.fontHuge
  layoutFont := a.layoutFont
  fontRegistry := a.fontPack.registry
  fontMediumId := a.fontPack.mediumId
  fontSmallId := a.fontPack.smallId
  fontLargeId := a.fontPack.largeId
  fontHugeId := a.fontPack.hugeId
  spriteTexture := a.spriteTexture
  halfSize := a.halfSize
  circleRadius := a.circleRadius
  spriteHalfSize := a.spriteHalfSize
  gridParticles := a.gridParticles
  lineBuffer := a.lineBuffer
  lineCount := a.lineCount
  lineWidth := a.lineWidth
  orbitalCount := a.orbitalCount
  orbitalParams := a.orbitalParams
  orbitalBuffer := a.orbitalBuffer
  physWidthF := a.physWidthF
  physHeightF := a.physHeightF
  physWidth := a.physWidth
  physHeight := a.physHeight
  contentOffsetX := 0.0
  contentOffsetY := 0.0
  layoutOffsetX := a.layoutOffsetX
  layoutOffsetY := a.layoutOffsetY
  layoutScale := a.layoutScale
}

/-- Build tab configurations from the demo list. -/
private def buildTabConfigs (demos : Array AnyDemo) (selectedIdx : Nat) : Array TabConfig :=
  demos.mapIdx fun idx demo => {
    id := idx
    label := AnyDemo.shortName demo
    isActive := idx == selectedIdx
  }

/-- Build or rebuild the tabbar widget. -/
private def rebuildTabBar (demos : Array AnyDemo) (selectedIdx : Nat)
    (fontId : Afferent.Arbor.FontId) (screenScale : Float) : TabBarState :=
  let configs := buildTabConfigs demos selectedIdx
  let result := buildTabBar configs fontId {} screenScale
  { selectedTab := selectedIdx
    tabIds := result.tabIds
    cachedWidget := some result.widget
    cachedLayouts := none  -- Will be computed during render
  }

/-- Render the tabbar widget using CanvasM. Returns updated canvas and layouts for hit testing. -/
private def renderTabBarWidgetM (tabBarState : TabBarState)
    (fontRegistry : FontRegistry) (physWidth : Float) (screenScale : Float)
    : CanvasM (Option Trellis.LayoutResult) := do
  match tabBarState.cachedWidget with
  | none => pure none
  | some widget =>
    -- Render the tabbar using the standard Arbor widget renderer
    let availWidth := physWidth
    let availHeight := tabBarHeight * screenScale
    Afferent.Widget.renderArborWidget fontRegistry widget availWidth availHeight
    -- Compute layouts separately for hit testing
    let measureResult ← runWithFonts fontRegistry (Afferent.Arbor.measureWidget widget availWidth availHeight)
    let layouts := Trellis.layout measureResult.node availWidth availHeight
    pure (some layouts)

/-- Handle click events for the tabbar. Returns Some newTabIndex if a tab was clicked. -/
private def handleTabBarClick (tabBarState : TabBarState) (clickX clickY : Float)
    : Option Nat := do
  let widget ← tabBarState.cachedWidget
  let layouts ← tabBarState.cachedLayouts
  -- Check if click is in the tabbar area; map any hit child to its tab container.
  let hitPath := Afferent.Arbor.hitTestPath widget layouts clickX clickY
  tabBarState.tabIds.findIdx? (fun tabId => hitPath.any (· == tabId))

/-- Unified visual demo - runs all demos in a grid layout -/
def unifiedDemo : IO Unit := do
  IO.println "Unified Visual Demo (with Animations!)"
  IO.println "--------------------------------------"
  -- Wisp requires explicit global init before any HTTP requests (worldmap tiles).
  Wisp.FFI.globalInit

  -- Query the actual screen scale factor (e.g., 1.5 for 150%, 2.0 for Retina)
  let screenScale ← FFI.getScreenScale
  IO.println s!"Screen scale factor: {screenScale}"

  -- Base logical dimensions
  let baseWidth : Float := 1920.0
  let baseHeight : Float := 1080.0

  -- Physical pixel dimensions
  let physWidth := (baseWidth * screenScale).toUInt32
  let physHeight := (baseHeight * screenScale).toUInt32
  IO.println s!"Physical resolution: {physWidth}x{physHeight}"

  -- Create canvas at physical resolution
  let canvas ← Canvas.create physWidth physHeight "Afferent - Visual Demos (LSD Disco Party Edition)"

  -- Sizes scaled for physical resolution
  let halfSize := 1.5 * screenScale
  let circleRadius := 2.0 * screenScale

  -- Physical dimensions as floats
  let physWidthF := baseWidth * screenScale
  let physHeightF := baseHeight * screenScale

  -- Precompute layout-demo fit so mode 5 can scale uniformly and keep text readable.
  let layoutW : Float := 1000.0
  let layoutH : Float := 800.0
  let layoutPadTop : Float := 60.0 * screenScale
  let calcLayout := fun (availW availH : Float) =>
    let layoutAvailW : Float := availW
    let layoutAvailH : Float := max 1.0 (availH - layoutPadTop)
    let layoutScale : Float := min (layoutAvailW / layoutW) (layoutAvailH / layoutH)
    let layoutOffsetX : Float := (layoutAvailW - layoutW * layoutScale) / 2.0
    let layoutOffsetY : Float := layoutPadTop + (layoutAvailH - layoutH * layoutScale) / 2.0
    (layoutOffsetX, layoutOffsetY, layoutScale)
  let (layoutOffsetX, layoutOffsetY, layoutScale) := calcLayout physWidthF physHeightF

  -- Grid particles (316x316 ≈ 100k grid of spinning squares/triangles)
  let gridCols := 316 * 3
  let gridRows := 316 * 2
  let gridSpacing := 2.0 * screenScale
  let gridStartX := (physWidthF - (gridCols.toFloat - 1) * gridSpacing) / 2.0
  let gridStartY := (physHeightF - (gridRows.toFloat - 1) * gridSpacing) / 2.0

  let lineWidth := 1.0 * screenScale

  -- Orbital instanced demo parameters (CPU orbit, GPU instancing)
  let orbitalCount : Nat := 50000
  let minRadius : Float := 20.0 * screenScale
  let maxRadius : Float := (min physWidthF physHeightF) * 0.45
  let speedMin : Float := 0.3
  let speedMax : Float := 1.6
  let sizeMin : Float := 1.0 * screenScale
  let sizeMax : Float := 3.5 * screenScale

  -- Background tasks for heavy CPU work.
  let gridRef ← IO.mkRef (none : Option Render.Dynamic.ParticleState)
  let _ ← IO.asTask (prio := .dedicated) do
    let gridParticles := Render.Dynamic.ParticleState.createGrid
      gridCols gridRows gridStartX gridStartY gridSpacing physWidthF physHeightF
    gridRef.set (some gridParticles)

  let lineRef ← IO.mkRef (none : Option (Array Float × Nat))
  let _ ← IO.asTask (prio := .dedicated) do
    let lineSegments := Demos.buildLineSegments physWidthF physHeightF
    lineRef.set (some lineSegments)

  let orbitalRef ← IO.mkRef (none : Option FloatArray)
  let _ ← IO.asTask (prio := .dedicated) do
    let params := buildOrbitalParams orbitalCount minRadius maxRadius speedMin speedMax sizeMin sizeMax
    orbitalRef.set (some params)

  -- Display modes: 0 = demo, 1 = grid squares, 2 = triangles, 3 = circles, 4 = sprites, 16 = lines, 17 = texture matrix, 18 = orbital instanced
  let startMode :=
    match (← IO.getEnv "AFFERENT_START_MODE") with
    | some s =>
        match s.toNat? with
        | some n => n
        | none => 0
    | none => 0
  let exitAfterFrames :=
    match (← IO.getEnv "AFFERENT_EXIT_AFTER_FRAMES") with
    | some s =>
        match s.toNat? with
        | some n => n
        | none => 0
    | none => 0

  let startTime ← IO.monoMsNow

  let renderLoop : IO (Canvas × AppState) := do
    let mut c := canvas
    let mut state : AppState := .loading {}
    let mut lastTime := startTime
    while !(← c.shouldClose) do
      let ok ← c.beginFrame Color.darkGray
      if ok then
        let now ← IO.monoMsNow
        let t := (now - startTime).toFloat / 1000.0  -- Elapsed seconds
        let dt := (now - lastTime).toFloat / 1000.0  -- Delta time
        lastTime := now
        match state with
        | .loading ls =>
            let ls ← advanceLoading ls screenScale c lineRef gridRef orbitalRef orbitalCount
            let progress := loadingProgress ls
            let label := loadingStatus ls
            c ← renderLoading c t screenScale progress label ls.fontSmall
            let assetsOpt ← toLoadedAssets ls screenScale halfSize circleRadius lineWidth orbitalCount
              physWidthF physHeightF physWidth physHeight layoutOffsetX layoutOffsetY layoutScale
            match assetsOpt with
            | some assets =>
                let tabBarHeightPx := tabBarHeight * screenScale
                let contentHeightF := max 1.0 (assets.physHeightF - tabBarHeightPx)
                let contentHeight := contentHeightF.toUInt32
                let (contentLayoutOffsetX, contentLayoutOffsetY, contentLayoutScale) :=
                  calcLayout assets.physWidthF contentHeightF
                let initEnv := mkEnvFromAssets assets 0.0 0.0 0
                let initEnv := {
                  initEnv with
                  physHeightF := contentHeightF
                  physHeight := contentHeight
                  contentOffsetX := 0.0
                  contentOffsetY := tabBarHeightPx
                  layoutOffsetX := contentLayoutOffsetX
                  layoutOffsetY := contentLayoutOffsetY
                  layoutScale := contentLayoutScale
                }
                let mut demos ← buildDemoList initEnv
                let displayMode : Nat := startMode % demos.size
                let msaaEnabled : Bool := AnyDemo.msaaEnabled (demos[displayMode]!)
                FFI.Renderer.setMSAAEnabled c.ctx.renderer msaaEnabled
                -- Build initial tabbar state
                let tabBarState := rebuildTabBar demos displayMode assets.fontPack.smallId screenScale
                IO.println "Rendering animated demo... (close window to exit)"
                IO.println "Click tabs to switch demos"
                state := .running {
                  assets := assets
                  demos := demos
                  displayMode := displayMode
                  msaaEnabled := msaaEnabled
                  frameCount := 0
                  fpsAccumulator := 0.0
                  displayFps := 0.0
                  framesLeft := exitAfterFrames
                  tabBar := tabBarState
                }
            | none =>
                state := .loading ls
            c ← c.endFrame
        | .running rs =>
            let mut rs := rs
            let keyCode ← c.getKeyCode
            let s := rs.assets.screenScale
            let tabBarHeightPx := tabBarHeight * s

            -- Check for click events on tabbar
            let mut clickOutsideTabBar := false
            let click ← FFI.Window.getClick c.ctx.window
            match click with
            | some ce =>
              if ce.y <= tabBarHeightPx then
                FFI.Window.clearClick c.ctx.window
                -- Check if click is on a tab
                match handleTabBarClick rs.tabBar ce.x ce.y with
                | some newTabIdx =>
                  if newTabIdx != rs.displayMode then
                    -- Switch to new tab
                    let exitEnv := mkEnvFromAssets rs.assets 0.0 0.0 keyCode
                    let contentHeightF := max 1.0 (rs.assets.physHeightF - tabBarHeightPx)
                    let contentHeight := contentHeightF.toUInt32
                    let (contentLayoutOffsetX, contentLayoutOffsetY, contentLayoutScale) :=
                      calcLayout rs.assets.physWidthF contentHeightF
                    let exitEnv := {
                      exitEnv with
                      physHeightF := contentHeightF
                      physHeight := contentHeight
                      contentOffsetX := 0.0
                      contentOffsetY := tabBarHeightPx
                      layoutOffsetX := contentLayoutOffsetX
                      layoutOffsetY := contentLayoutOffsetY
                      layoutScale := contentLayoutScale
                    }
                    let currentDemo ← AnyDemo.onExit (rs.demos[rs.displayMode]!) c exitEnv
                    rs := { rs with demos := rs.demos.set! rs.displayMode currentDemo }
                    rs := { rs with displayMode := newTabIdx }
                    c := c.resetState
                    c.ctx.resetScissor
                    rs := { rs with msaaEnabled := AnyDemo.msaaEnabled (rs.demos[rs.displayMode]!) }
                    FFI.Renderer.setMSAAEnabled c.ctx.renderer rs.msaaEnabled
                    -- Rebuild tabbar with new selection
                    let newTabBar := rebuildTabBar rs.demos newTabIdx rs.assets.fontPack.smallId s
                    rs := { rs with tabBar := newTabBar }
                    IO.println s!"Switched to {AnyDemo.name (rs.demos[rs.displayMode]!)}"
                | none => pure ()
              else
                clickOutsideTabBar := true
            | none => pure ()

            -- Set up scissor to clip demo content to area below tabbar
            let tabBarHeightPxU32 := tabBarHeightPx.toUInt32
            let contentHeightF := max 1.0 (rs.assets.physHeightF - tabBarHeightPx)
            let contentHeight := contentHeightF.toUInt32
            c.setScissor 0 tabBarHeightPxU32 rs.assets.physWidth contentHeight

            -- Use a base transform so resetTransform keeps the tabbar offset.
            let contentTransform := Transform.translate 0 tabBarHeightPx
            c := c.modifyState (CanvasState.setBaseTransform contentTransform)
            c := c.resetTransform

            -- Create adjusted environment with reduced height for demo area
            let (contentLayoutOffsetX, contentLayoutOffsetY, contentLayoutScale) :=
              calcLayout rs.assets.physWidthF contentHeightF
            let mut env := mkEnvFromAssets rs.assets t dt keyCode
            env := {
              env with
              physHeightF := contentHeightF
              physHeight := contentHeight
              contentOffsetX := 0.0
              contentOffsetY := tabBarHeightPx
              layoutOffsetX := contentLayoutOffsetX
              layoutOffsetY := contentLayoutOffsetY
              layoutScale := contentLayoutScale
            }

            -- Render the current demo (clipped to content area)
            let demo := rs.demos[rs.displayMode]!
            let (c', nextDemo) ← AnyDemo.step demo c env
            c := c'
            rs := { rs with demos := rs.demos.set! rs.displayMode nextDemo }
            if clickOutsideTabBar then
              FFI.Window.clearClick c.ctx.window

            -- Reset base transform and scissor, then render tabbar on top
            c := c.modifyState (CanvasState.setBaseTransform Transform.identity)
            c := c.resetTransform
            c.resetScissor
            let (layouts, c') ← run c (renderTabBarWidgetM rs.tabBar rs.assets.fontPack.registry rs.assets.physWidthF s)
            c := c'
            -- Cache the layouts for hit testing
            rs := { rs with tabBar := { rs.tabBar with cachedLayouts := layouts } }

            -- Update FPS counter (update display every 10 frames for stability)
            rs := { rs with frameCount := rs.frameCount + 1 }
            if dt > 0.0 then
              rs := { rs with fpsAccumulator := rs.fpsAccumulator + (1.0 / dt) }
            if rs.frameCount >= 10 then
              let displayFps := rs.fpsAccumulator / rs.frameCount.toFloat
              rs := {
                rs with
                displayFps := displayFps
                fpsAccumulator := 0.0
                frameCount := 0
              }

            -- Render FPS counter in top-right corner (after all other rendering)
            -- Reset transform to draw in screen coordinates
            let fpsText := s!"{rs.displayFps.toUInt32} FPS"
            let (textWidth, _) ← rs.assets.fontSmall.measureText fpsText
            c ← run' c do
              resetTransform
              let (fpsW, _) ← getCurrentSize
              setFillColor (Color.hsva 0.0 0.0 0.0 0.6)
              fillRectXYWH (fpsW - textWidth - 20 * s) (tabBarHeightPx + 5 * s) (textWidth + 15 * s) (25 * s)
              setFillColor Color.white
              fillTextXY fpsText (fpsW - textWidth - 12 * s) (tabBarHeightPx + 22 * s) rs.assets.fontSmall

            c ← c.endFrame

            if rs.framesLeft != 0 then
              let framesLeft := rs.framesLeft - 1
              rs := { rs with framesLeft := framesLeft }
              if framesLeft == 0 then
                state := .running rs
                break
            state := .running rs
    pure (c, state)

  let renderTask ← IO.asTask (prio := .dedicated) renderLoop
  canvas.ctx.window.runEventLoop
  let (c, state) ← match renderTask.get with
    | .ok result => pure result
    | .error err => throw err

  IO.println "Cleaning up..."
  match state with
  | .loading ls => cleanupLoading ls
  | .running rs => cleanupAssets rs.assets
  c.destroy
  Wisp.FFI.globalCleanup
  Wisp.HTTP.Client.shutdown

/-- Main entry point - runs all demos -/
def main : IO Unit := do
  IO.println "Afferent - 2D Vector Graphics Library"
  IO.println "======================================"
  IO.println ""

  -- Run unified visual demo (single window with all demos)
  unifiedDemo

  IO.println ""
  IO.println "Done!"

end Demos
