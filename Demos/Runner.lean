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
import Std.Internal.Async.Process
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
  canopyId : Afferent.Arbor.FontId
  canopySmallId : Afferent.Arbor.FontId

private structure LoadingState where
  fontSmall : Option Font := none
  fontMedium : Option Font := none
  fontLarge : Option Font := none
  fontHuge : Option Font := none
  fontCanopy : Option Font := none
  fontCanopySmall : Option Font := none
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
  fontCanopy : Font
  fontCanopySmall : Font
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

/-- Fixed tabbar height in logical pixels. -/
def tabBarHeight : Float := ({} : TabBarStyle).height
def footerBarHeight : Float := 60

private def tabBarStartId : Nat := 1

private structure RootBuild where
  widget : Afferent.Arbor.Widget
  tabBar : TabBarResult
  contentId : Afferent.Arbor.WidgetId

private structure RunningState where
  assets : LoadedAssets
  demos : Array AnyDemo
  demoRefs : Array (IO.Ref AnyDemo)
  displayMode : Nat
  frameCount : Nat
  fpsAccumulator : Float
  displayFps : Float
  renderCommandCount : Nat
  widgetCount : Nat
  cacheHits : Nat
  cacheMisses : Nat
  cacheSize : Nat
  batchedCalls : Nat
  individualCalls : Nat
  rectsBatched : Nat
  circlesBatched : Nat
  strokeRectsBatched : Nat
  peakRssKb : UInt64
  minorFaults : UInt64
  majorFaults : UInt64
  framesLeft : Nat
  tabBar : TabBarResult
  lastHoverPath : Array Afferent.Arbor.WidgetId := #[]
  lastMouseX : Float := 0.0
  lastMouseY : Float := 0.0
  prevLeftDown : Bool := false
  -- Timing stats (in milliseconds)
  timeUpdateMs : Float := 0.0
  timeBuildMs : Float := 0.0
  timeLayoutMs : Float := 0.0
  timeCollectMs : Float := 0.0
  timeGpuMs : Float := 0.0

private inductive AppState where
  | loading (state : LoadingState)
  | running (state : RunningState)

private def loadingStepsTotal : Nat := 14

private def loadingStepsDone (s : LoadingState) : Nat :=
  (if s.fontSmall.isSome then 1 else 0) +
  (if s.fontMedium.isSome then 1 else 0) +
  (if s.fontLarge.isSome then 1 else 0) +
  (if s.fontHuge.isSome then 1 else 0) +
  (if s.fontCanopy.isSome then 1 else 0) +
  (if s.fontCanopySmall.isSome then 1 else 0) +
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
  if s.fontSmall.isNone || s.fontMedium.isNone || s.fontLarge.isNone || s.fontHuge.isNone ||
     s.fontCanopy.isNone || s.fontCanopySmall.isNone then
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
  if s.fontCanopy.isNone then
    let fontCanopy ← Font.load "/System/Library/Fonts/Monaco.ttf" (14 * screenScale).toUInt32
    return { s with fontCanopy := some fontCanopy }
  if s.fontCanopySmall.isNone then
    let fontCanopySmall ← Font.load "/System/Library/Fonts/Monaco.ttf" (10 * screenScale).toUInt32
    return { s with fontCanopySmall := some fontCanopySmall }
  if s.layoutFont.isNone then
    let layoutLabelPt : Float := 12.0
    let layoutFontPx : UInt32 := (max 8.0 (layoutLabelPt * screenScale)).toUInt32
    let layoutFont ← Font.load "/System/Library/Fonts/Monaco.ttf" layoutFontPx
    return { s with layoutFont := some layoutFont }
  if s.fontPack.isNone then
    match s.fontSmall, s.fontMedium, s.fontLarge, s.fontHuge, s.fontCanopy, s.fontCanopySmall with
    | some fontSmall, some fontMedium, some fontLarge, some fontHuge, some fontCanopy, some fontCanopySmall =>
        let (reg1, fontSmallId) := FontRegistry.empty.register fontSmall "small"
        let (reg2, fontMediumId) := reg1.register fontMedium "medium"
        let (reg3, fontLargeId) := reg2.register fontLarge "large"
        let (reg4, fontHugeId) := reg3.register fontHuge "huge"
        let (reg5, fontCanopyId) := reg4.register fontCanopy "canopy"
        let (reg6, fontCanopySmallId) := reg5.register fontCanopySmall "canopySmall"
        let registry := reg6.setDefault fontMedium
        return { s with fontPack := some {
          registry := registry
          smallId := fontSmallId
          mediumId := fontMediumId
          largeId := fontLargeId
          hugeId := fontHugeId
          canopyId := fontCanopyId
          canopySmallId := fontCanopySmallId
        } }
    | _, _, _, _, _, _ => return s
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
  match s.fontSmall, s.fontMedium, s.fontLarge, s.fontHuge, s.fontCanopy, s.fontCanopySmall,
        s.layoutFont, s.fontPack, s.spriteTexture, s.gridParticles, s.lineSegments,
        s.lineBuffer, s.orbitalParams, s.orbitalBuffer with
  | some fontSmall, some fontMedium, some fontLarge, some fontHuge, some fontCanopy, some fontCanopySmall,
    some layoutFont, some fontPack, some spriteTexture, some gridParticles, some (_, lineCount),
    some lineBuffer, some orbitalParams, some orbitalBuffer =>
      let spriteHalfSize ← spriteHalfSizeFromTexture spriteTexture
      pure (some {
        screenScale
        fontSmall
        fontMedium
        fontLarge
        fontHuge
        fontCanopy
        fontCanopySmall
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
  | _, _, _, _, _, _, _, _, _, _, _, _, _, _ => pure none

private def cleanupLoading (s : LoadingState) : IO Unit := do
  if let some font := s.fontSmall then font.destroy
  if let some font := s.fontMedium then font.destroy
  if let some font := s.fontLarge then font.destroy
  if let some font := s.fontHuge then font.destroy
  if let some font := s.fontCanopy then font.destroy
  if let some font := s.fontCanopySmall then font.destroy
  if let some font := s.layoutFont then font.destroy
  if let some tex := s.spriteTexture then FFI.Texture.destroy tex
  if let some buf := s.lineBuffer then FFI.Buffer.destroy buf
  if let some buf := s.orbitalBuffer then FFI.FloatBuffer.destroy buf

private def cleanupAssets (a : LoadedAssets) : IO Unit := do
  a.fontSmall.destroy
  a.fontMedium.destroy
  a.fontLarge.destroy
  a.fontHuge.destroy
  a.fontCanopy.destroy
  a.fontCanopySmall.destroy
  a.layoutFont.destroy
  FFI.Texture.destroy a.spriteTexture
  FFI.Buffer.destroy a.lineBuffer
  FFI.FloatBuffer.destroy a.orbitalBuffer

private def mkEnvFromAssets (a : LoadedAssets) (t dt : Float)
    (keyCode : UInt16) (clearKey : IO Unit) (window : FFI.Window) : DemoEnv := {
  screenScale := a.screenScale
  t := t
  dt := dt
  keyCode := keyCode
  clearKey := clearKey
  window := window
  fontSmall := a.fontSmall
  fontMedium := a.fontMedium
  fontLarge := a.fontLarge
  fontHuge := a.fontHuge
  fontCanopy := a.fontCanopy
  fontCanopySmall := a.fontCanopySmall
  layoutFont := a.layoutFont
  fontRegistry := a.fontPack.registry
  fontMediumId := a.fontPack.mediumId
  fontSmallId := a.fontPack.smallId
  fontLargeId := a.fontPack.largeId
  fontHugeId := a.fontPack.hugeId
  fontCanopyId := a.fontPack.canopyId
  fontCanopySmallId := a.fontPack.canopySmallId
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
  windowWidthF := a.physWidthF
  windowHeightF := a.physHeightF
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
    (fontId : Afferent.Arbor.FontId) (screenScale : Float) : TabBarResult :=
  let configs := buildTabConfigs demos selectedIdx
  buildTabBar configs fontId {} screenScale tabBarStartId

private def nextWidgetId (w : Afferent.Arbor.Widget) : Nat :=
  (Afferent.Arbor.Widget.allIds w).foldl (fun acc wid => max acc wid) 0 + 1

private def buildFooterWidget (startId : Nat) (fontId : Afferent.Arbor.FontId)
    (screenScale : Float) (line1 line2 : String) : Afferent.Arbor.Widget :=
  Afferent.Arbor.buildFrom startId do
    let s := fun (v : Float) => v * screenScale
    let outerStyle : Afferent.Arbor.BoxStyle := {
      backgroundColor := some (Color.gray 0.08)
      padding := Trellis.EdgeInsets.symmetric (s 8) (s 4)
      height := .length (s footerBarHeight)
      flexItem := some (Trellis.FlexItem.fixed (s footerBarHeight))
    }
    let rowStyle : Afferent.Arbor.BoxStyle := {
      width := .percent 1.0
    }
    Afferent.Arbor.flexColumn (Trellis.FlexContainer.column (s 2)) outerStyle #[
      Afferent.Arbor.flexRow { Trellis.FlexContainer.row 0 with alignItems := .center } rowStyle #[
        Afferent.Arbor.text' line1 fontId (Color.gray 0.7) .left none
      ],
      Afferent.Arbor.flexRow { Trellis.FlexContainer.row 0 with alignItems := .center } rowStyle #[
        Afferent.Arbor.text' line2 fontId (Color.gray 0.6) .left none
      ]
    ]

private def buildRootWidget (tabBar : TabBarResult) (content : Afferent.Arbor.Widget)
    (footerLine1 footerLine2 : String) (fontId : Afferent.Arbor.FontId) (screenScale : Float) : RootBuild :=
  let footerStartId := nextWidgetId content
  let footer := buildFooterWidget footerStartId fontId screenScale footerLine1 footerLine2
  let root : Afferent.Arbor.Widget :=
    .flex 0 none (Trellis.FlexContainer.column 0)
      { width := .percent 1.0, height := .percent 1.0 }
      #[tabBar.widget, content, footer]
  { widget := root, tabBar := tabBar, contentId := Afferent.Arbor.Widget.id content }

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
                let footerHeightPx := footerBarHeight * screenScale
                let contentHeightF := max 1.0 (assets.physHeightF - tabBarHeightPx - footerHeightPx)
                let contentHeight := contentHeightF.toUInt32
                let (contentLayoutOffsetX, contentLayoutOffsetY, contentLayoutScale) :=
                  calcLayout assets.physWidthF contentHeightF
                let initEnv := mkEnvFromAssets assets 0.0 0.0 0 (pure ()) c.ctx.window
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
                let demos ← buildDemoList initEnv
                let demoRefs ← demos.mapM (fun demo => IO.mkRef demo)
                let displayMode : Nat := startMode % demos.size
                -- Build initial tabbar state
                let tabBar := rebuildTabBar demos displayMode assets.fontPack.smallId screenScale
                IO.println "Rendering animated demo... (close window to exit)"
                IO.println "Click tabs to switch demos"
                state := .running {
                  assets := assets
                  demos := demos
                  demoRefs := demoRefs
                  displayMode := displayMode
                  frameCount := 0
                  fpsAccumulator := 0.0
                  displayFps := 0.0
                  renderCommandCount := 0
                  widgetCount := 0
                  cacheHits := 0
                  cacheMisses := 0
                  cacheSize := 0
                  batchedCalls := 0
                  individualCalls := 0
                  rectsBatched := 0
                  circlesBatched := 0
                  strokeRectsBatched := 0
                  peakRssKb := 0
                  minorFaults := 0
                  majorFaults := 0
                  framesLeft := exitAfterFrames
                  tabBar := tabBar
                }
            | none =>
                state := .loading ls
            c ← c.endFrame
        | .running rs =>
            let mut rs := rs
            let keyCode ← c.getKeyCode
            let click ← FFI.Window.getClick c.ctx.window
            let s := rs.assets.screenScale
            let (screenW, screenH) ← c.ctx.getCurrentSize
            if screenW != rs.assets.physWidthF || screenH != rs.assets.physHeightF then
              let (layoutOffsetX, layoutOffsetY, layoutScale) := calcLayout screenW screenH
              rs := { rs with
                assets := {
                  rs.assets with
                  physWidthF := screenW
                  physHeightF := screenH
                  physWidth := screenW.toUInt32
                  physHeight := screenH.toUInt32
                  layoutOffsetX := layoutOffsetX
                  layoutOffsetY := layoutOffsetY
                  layoutScale := layoutScale
                }
              }
            let tabBarHeightPx := tabBarHeight * s
            let footerHeightPx := footerBarHeight * s

            let memMb : UInt64 := rs.peakRssKb / 1024
            let cacheTotal := rs.cacheHits + rs.cacheMisses
            let cacheRate := if cacheTotal > 0 then (rs.cacheHits * 100) / cacheTotal else 0
            let totalDrawCalls := rs.batchedCalls + rs.individualCalls

            -- Line 1: Performance, Commands, Draw Calls
            let footerLine1 :=
              s!"FPS: {rs.displayFps.toUInt32}  |  Render Commands: {rs.renderCommandCount}  |  Draw Calls: {totalDrawCalls} (Batched: {rs.batchedCalls}, Individual: {rs.individualCalls})  |  Widgets: {rs.widgetCount}"

            -- Line 2: Timing breakdown and memory
            let fmt := fun (v : Float) => s!"{(v * 10).toUInt32.toFloat / 10}"  -- 1 decimal place
            let footerLine2 :=
              s!"Timing: Update {fmt rs.timeUpdateMs}ms, Build {fmt rs.timeBuildMs}ms, Layout {fmt rs.timeLayoutMs}ms, Collect {fmt rs.timeCollectMs}ms, GPU {fmt rs.timeGpuMs}ms  |  Cache: {cacheRate}%  |  Mem: {memMb}MB"

            let buildDemoWidget := fun (tabBar : TabBarResult) (demo : AnyDemo)
                (envForView : DemoEnv) =>
              match AnyDemo.view? demo envForView with
              | some builder => Afferent.Arbor.buildFrom tabBar.finalId builder
              | none => Afferent.Arbor.buildFrom tabBar.finalId (Afferent.Arbor.spacer 0 0)

            let buildRoot := fun (tabBar : TabBarResult) (demo : AnyDemo)
                (envForView : DemoEnv) =>
              buildRootWidget tabBar (buildDemoWidget tabBar demo envForView)
                footerLine1 footerLine2 rs.assets.fontPack.smallId s

            let measureRoot := fun (root : Afferent.Arbor.Widget) => do
              let measureResult ← runWithFonts rs.assets.fontPack.registry
                (Afferent.Arbor.measureWidget root screenW screenH)
              let layouts := Trellis.layout measureResult.node screenW screenH
              -- Apply content scale transforms to layouts
              let layoutsWithScale ← runWithFonts rs.assets.fontPack.registry
                (Afferent.Arbor.applyContentScale measureResult.widget layouts)
              pure (measureResult.widget, layoutsWithScale)

            let envFromLayout := fun (layout : Trellis.ComputedLayout) (t dt : Float) (keyCode : UInt16) (clearKey : IO Unit) =>
              let rect := layout.contentRect
              let contentW := max 1.0 rect.width
              let contentH := max 1.0 rect.height
              let (contentLayoutOffsetX, contentLayoutOffsetY, contentLayoutScale) :=
                calcLayout contentW contentH
              let base := mkEnvFromAssets rs.assets t dt keyCode clearKey c.ctx.window
              {
                base with
                physWidthF := contentW
                physHeightF := contentH
                physWidth := contentW.toUInt32
                physHeight := contentH.toUInt32
                contentOffsetX := rect.x
                contentOffsetY := rect.y
                layoutOffsetX := contentLayoutOffsetX
                layoutOffsetY := contentLayoutOffsetY
                layoutScale := contentLayoutScale
              }

            let fallbackContentH := max 1.0 (screenH - tabBarHeightPx - footerHeightPx)
            let defaultContentRect : Trellis.LayoutRect :=
              { x := 0, y := tabBarHeightPx, width := screenW, height := fallbackContentH }
            let defaultLayout := Trellis.ComputedLayout.simple 0 defaultContentRect
            let envForView := envFromLayout defaultLayout t dt keyCode c.clearKey
            -- Timing: Update phase (FRP propagation for Canopy demos)
            let tUpdate0 ← IO.monoMsNow
            match rs.demoRefs[rs.displayMode]? with
            | some demoRef => do
                let demo ← demoRef.get
                let demo' ← AnyDemo.update demo envForView
                demoRef.set demo'
            | none => pure ()
            let tUpdate1 ← IO.monoMsNow
            -- Timing: Build phase (widget tree construction)
            let tBuild0 ← IO.monoMsNow
            let buildRootBuild : IO RootBuild := do
              match rs.demoRefs[rs.displayMode]? with
              | some demoRef => do
                  let demo ← demoRef.get
                  pure (buildRoot rs.tabBar demo envForView)
              | none =>
                  pure (buildRootWidget rs.tabBar (.spacer rs.tabBar.finalId none 0 0)
                    footerLine1 footerLine2 rs.assets.fontPack.smallId s)

            let initRootBuild ← buildRootBuild
            let tBuild1 ← IO.monoMsNow
            -- Timing: Layout phase (measurement and layout calculation)
            let tLayout0 ← IO.monoMsNow
            let ((measuredWidget, layouts, clickedTab, demoClickPath), rootBuild) ←
              (do
                let rootBuild : RootBuild ← get
                let (measuredWidget, layouts) ← StateT.lift (measureRoot rootBuild.widget)
                let mut clickedTab : Option Nat := none
                let mut demoClickPath : Option (Array Afferent.Arbor.WidgetId) := none
                match click with
                | some ce =>
                    let hitPath := Afferent.Arbor.hitTestPath measuredWidget layouts ce.x ce.y
                    let inTabBar := hitPath.any (· == rootBuild.tabBar.rowId)
                    if inTabBar then
                      clickedTab := rootBuild.tabBar.tabIds.findIdx? (fun tabId => hitPath.any (· == tabId))
                    else
                      demoClickPath := some hitPath
                | none => pure ()
                pure (measuredWidget, layouts, clickedTab, demoClickPath)
              ) |>.run initRootBuild
            let tLayout1 ← IO.monoMsNow

            let mut measuredWidget := measuredWidget
            let mut layouts := layouts
            let mut rootBuild := rootBuild

            let mut tabSwitched := false
            match clickedTab with
            | some newTabIdx =>
                if newTabIdx != rs.displayMode then
                  let contentLayout :=
                    (layouts.get rootBuild.contentId).getD (Trellis.ComputedLayout.simple rootBuild.contentId defaultContentRect)
                  let exitEnv := envFromLayout contentLayout 0.0 0.0 keyCode c.clearKey
                  match rs.demoRefs[rs.displayMode]? with
                  | some demoRef =>
                      let currentDemo ← demoRef.get
                      let nextDemo ← AnyDemo.onExit currentDemo c exitEnv
                      demoRef.set nextDemo
                  | none => pure ()
                  rs := { rs with displayMode := newTabIdx }
                  c := c.resetState
                  c.ctx.resetScissor
                  let newTabBar := rebuildTabBar rs.demos newTabIdx rs.assets.fontPack.smallId s
                  rs := { rs with tabBar := newTabBar }
                  IO.println s!"Switched to {AnyDemo.name (rs.demos[rs.displayMode]!)}"
                  tabSwitched := true

                  let ((measuredWidget', layouts'), rootBuild') ←
                    (do
                      let rootBuild : RootBuild ← StateT.lift buildRootBuild
                      set rootBuild
                      let (measuredWidget, layouts) ← StateT.lift (measureRoot rootBuild.widget)
                      pure (measuredWidget, layouts)
                    ) |>.run rootBuild
                  measuredWidget := measuredWidget'
                  layouts := layouts'
                  rootBuild := rootBuild'
            | none => pure ()

            if !tabSwitched then
              match demoClickPath with
              | some hitPath =>
                  match click with
                  | some ce =>
                      let contentLayout :=
                        (layouts.get rootBuild.contentId).getD (Trellis.ComputedLayout.simple rootBuild.contentId defaultContentRect)
                      let clickEnv := envFromLayout contentLayout t dt keyCode c.clearKey
                      match rs.demoRefs[rs.displayMode]? with
                      | some demoRef =>
                          let demo ← demoRef.get
                          let demo' ← AnyDemo.handleClickWithLayouts demo clickEnv rootBuild.contentId hitPath ce layouts measuredWidget
                          demoRef.set demo'
                          let ((measuredWidget', layouts'), rootBuild') ←
                            (do
                              let rootBuild : RootBuild ← StateT.lift buildRootBuild
                              set rootBuild
                              let (measuredWidget, layouts) ← StateT.lift (measureRoot rootBuild.widget)
                              pure (measuredWidget, layouts)
                            ) |>.run rootBuild
                          measuredWidget := measuredWidget'
                          layouts := layouts'
                          rootBuild := rootBuild'
                      | none => pure ()
                  | none => pure ()
              | none => pure ()

            -- Handle hover events (mouse movement)
            let (mouseX, mouseY) ← FFI.Window.getMousePos c.ctx.window
            let hoverPath := Afferent.Arbor.hitTestPath measuredWidget layouts mouseX mouseY
            let inTabBar := hoverPath.any (· == rootBuild.tabBar.rowId)
            -- Only dispatch hover if not in tab bar and path changed
            if !inTabBar && (hoverPath != rs.lastHoverPath || mouseX != rs.lastMouseX || mouseY != rs.lastMouseY) then
              let contentLayout :=
                (layouts.get rootBuild.contentId).getD (Trellis.ComputedLayout.simple rootBuild.contentId defaultContentRect)
              let hoverEnv := envFromLayout contentLayout t dt keyCode c.clearKey
              match rs.demoRefs[rs.displayMode]? with
              | some demoRef =>
                  let demo ← demoRef.get
                  let demo' ← AnyDemo.handleHoverWithLayouts demo hoverEnv rootBuild.contentId hoverPath mouseX mouseY layouts measuredWidget
                  demoRef.set demo'
                  -- Rebuild widget to reflect hover state changes
                  let ((measuredWidget', layouts'), rootBuild') ←
                    (do
                      let rootBuild : RootBuild ← StateT.lift buildRootBuild
                      set rootBuild
                      let (measuredWidget, layouts) ← StateT.lift (measureRoot rootBuild.widget)
                      pure (measuredWidget, layouts)
                    ) |>.run rootBuild
                  measuredWidget := measuredWidget'
                  layouts := layouts'
                  rootBuild := rootBuild'
              | none => pure ()
              rs := { rs with lastHoverPath := hoverPath, lastMouseX := mouseX, lastMouseY := mouseY }

            -- Handle keyboard events (use hasKeyPressed to distinguish key code 0 from "no key")
            let hasKey ← FFI.Window.hasKeyPressed c.ctx.window
            if hasKey then
              let modifiers ← FFI.Window.getModifiers c.ctx.window
              let keyEvent : Afferent.Arbor.KeyEvent := {
                key := Afferent.Arbor.Key.fromKeyCode keyCode
                modifiers := Afferent.Arbor.Modifiers.fromBitmask modifiers
              }
              let contentLayout :=
                (layouts.get rootBuild.contentId).getD (Trellis.ComputedLayout.simple rootBuild.contentId defaultContentRect)
              let keyEnv := envFromLayout contentLayout t dt keyCode c.clearKey
              match rs.demoRefs[rs.displayMode]? with
              | some demoRef =>
                  let demo ← demoRef.get
                  let demo' ← AnyDemo.handleKey demo keyEnv keyEvent
                  demoRef.set demo'
                  c.clearKey
                  -- Rebuild widget to reflect keyboard input changes
                  let ((measuredWidget', layouts'), rootBuild') ←
                    (do
                      let rootBuild : RootBuild ← StateT.lift buildRootBuild
                      set rootBuild
                      let (measuredWidget, layouts) ← StateT.lift (measureRoot rootBuild.widget)
                      pure (measuredWidget, layouts)
                    ) |>.run rootBuild
                  measuredWidget := measuredWidget'
                  layouts := layouts'
                  rootBuild := rootBuild'
              | none => pure ()

            -- Handle scroll events
            let (scrollX, scrollY) ← FFI.Window.getScrollDelta c.ctx.window
            if scrollX != 0.0 || scrollY != 0.0 then
              let modifiers ← FFI.Window.getModifiers c.ctx.window
              let scrollEvent : Afferent.Arbor.ScrollEvent := {
                x := mouseX
                y := mouseY
                deltaX := scrollX
                deltaY := scrollY
                modifiers := Afferent.Arbor.Modifiers.fromBitmask modifiers
              }
              let contentLayout :=
                (layouts.get rootBuild.contentId).getD (Trellis.ComputedLayout.simple rootBuild.contentId defaultContentRect)
              let scrollEnv := envFromLayout contentLayout t dt keyCode c.clearKey
              match rs.demoRefs[rs.displayMode]? with
              | some demoRef =>
                  let demo ← demoRef.get
                  -- Calculate fresh hit path at scroll position (not stale hoverPath)
                  let scrollPath := Afferent.Arbor.hitTestPath measuredWidget layouts mouseX mouseY
                  let demo' ← AnyDemo.handleScrollWithLayouts demo scrollEnv scrollPath scrollEvent layouts measuredWidget
                  demoRef.set demo'
                  -- Rebuild widget to reflect scroll changes
                  let ((measuredWidget', layouts'), rootBuild') ←
                    (do
                      let rootBuild : RootBuild ← StateT.lift buildRootBuild
                      set rootBuild
                      let (measuredWidget, layouts) ← StateT.lift (measureRoot rootBuild.widget)
                      pure (measuredWidget, layouts)
                    ) |>.run rootBuild
                  measuredWidget := measuredWidget'
                  layouts := layouts'
                  rootBuild := rootBuild'
              | none => pure ()
              FFI.Window.clearScroll c.ctx.window

            -- Handle mouse button release (for ending drag interactions)
            let buttons ← FFI.Window.getMouseButtons c.ctx.window
            let leftDown := (buttons &&& (1 : UInt8)) != (0 : UInt8)
            if !leftDown && rs.prevLeftDown then
              let contentLayout :=
                (layouts.get rootBuild.contentId).getD (Trellis.ComputedLayout.simple rootBuild.contentId defaultContentRect)
              let mouseUpEnv := envFromLayout contentLayout t dt keyCode c.clearKey
              match rs.demoRefs[rs.displayMode]? with
              | some demoRef =>
                  let demo ← demoRef.get
                  let mouseUpPath := Afferent.Arbor.hitTestPath measuredWidget layouts mouseX mouseY
                  let demo' ← AnyDemo.handleMouseUpWithLayouts demo mouseUpEnv mouseX mouseY mouseUpPath layouts measuredWidget
                  demoRef.set demo'
                  -- Rebuild widget to reflect state changes from mouse up
                  let ((measuredWidget', layouts'), rootBuild') ←
                    (do
                      let rootBuild : RootBuild ← StateT.lift buildRootBuild
                      set rootBuild
                      let (measuredWidget, layouts) ← StateT.lift (measureRoot rootBuild.widget)
                      pure (measuredWidget, layouts)
                    ) |>.run rootBuild
                  measuredWidget := measuredWidget'
                  layouts := layouts'
                  rootBuild := rootBuild'
              | none => pure ()
            rs := { rs with prevLeftDown := leftDown }

            -- Timing: Collect phase (render command generation)
            let tCollect0 ← IO.monoMsNow
            let (commands, cacheHits, cacheMisses) ← Afferent.Arbor.collectCommandsCachedWithStats c.renderCache measuredWidget layouts
            let tCollect1 ← IO.monoMsNow
            let cacheSize := (← c.renderCache.get).size
            let widgetCount := Afferent.Arbor.Widget.widgetCount measuredWidget
            rs := { rs with renderCommandCount := commands.size, widgetCount := widgetCount, cacheHits := cacheHits, cacheMisses := cacheMisses, cacheSize := cacheSize }
            -- Timing: GPU phase (batching and draw calls)
            let tGpu0 ← IO.monoMsNow
            let (batchStats, c') ← CanvasM.run c do
              let stats ← Afferent.Widget.executeCommandsBatchedWithStats rs.assets.fontPack.registry commands
              Afferent.Widget.renderCustomWidgets measuredWidget layouts
              return stats
            c := c'
            let tGpu1 ← IO.monoMsNow
            rs := { rs with batchedCalls := batchStats.batchedCalls, individualCalls := batchStats.individualCalls, rectsBatched := batchStats.rectsBatched, circlesBatched := batchStats.circlesBatched, strokeRectsBatched := batchStats.strokeRectsBatched }
            -- Store timing stats
            rs := { rs with
              timeUpdateMs := (tUpdate1 - tUpdate0).toFloat
              timeBuildMs := (tBuild1 - tBuild0).toFloat
              timeLayoutMs := (tLayout1 - tLayout0).toFloat
              timeCollectMs := (tCollect1 - tCollect0).toFloat
              timeGpuMs := (tGpu1 - tGpu0).toFloat
            }

            if click.isSome then
              FFI.Window.clearClick c.ctx.window

            -- Update FPS counter (update display every 10 frames for stability)
            rs := { rs with frameCount := rs.frameCount + 1 }
            if dt > 0.0 then
              rs := { rs with fpsAccumulator := rs.fpsAccumulator + (1.0 / dt) }
            if rs.frameCount >= 10 then
              let displayFps := rs.fpsAccumulator / rs.frameCount.toFloat
              let usage ← Std.Internal.IO.Process.getResourceUsage
              rs := {
                rs with
                displayFps := displayFps
                fpsAccumulator := 0.0
                frameCount := 0
                peakRssKb := usage.peakResidentSetSizeKb
                minorFaults := usage.minorPageFaults
                majorFaults := usage.majorPageFaults
              }

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
