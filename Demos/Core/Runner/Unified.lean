/-
  Demo Runner - Unified visual demo loop.
-/
import Afferent
import Afferent.Arbor
import Afferent.Widget
import Demos.Overview.Text
import Demos.Core.Demo
import Demos.Core.DemoRegistry
import Demos.Core.Runner.Loading
import Demos.Core.Runner.Types
import Demos.Core.Runner.Widgets
import Std.Internal.Async.Process
import Std.Data.HashMap
import Wisp
import Init.Data.FloatArray

set_option maxRecDepth 1024

open Afferent CanvasM

namespace Demos

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
    let _ ← Afferent.Arbor.enableCollectMetrics
    let mut c := canvas
    let mut state : AppState := .loading {}
    let mut lastTime := startTime
    let mut lastWorkEndTime := startTime  -- When we finished all work last frame
    let mut lastPresentMs : Float := 0.0
    while !(← c.shouldClose) do
      let ok ← c.beginFrame Color.darkGray
      if ok then
        -- Measure idle/present time AFTER beginFrame returns (captures vsync wait)
        let now ← IO.monoMsNow
        lastPresentMs := (now - lastWorkEndTime).toFloat
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
                  cacheCapacity := 0
                  batchedCalls := 0
                  individualCalls := 0
                  rectsBatched := 0
                  circlesBatched := 0
                  strokeRectsBatched := 0
                  linesBatched := 0
                  textsBatched := 0
                  peakRssKb := 0
                  minorFaults := 0
                  majorFaults := 0
                  framesLeft := exitAfterFrames
                  tabBar := tabBar
                  frameCache := none
                }
            | none =>
                state := .loading ls
            c ← c.endFrame
            lastWorkEndTime ← IO.monoMsNow
        | .running rs =>
            let mut rs := rs
            let frameNameMap := match rs.frameCache with
              | some cache => cache.hitIndex.nameMap
              | none => ({} : Std.HashMap String Afferent.Arbor.WidgetId)
            setFrameNameMap frameNameMap
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

            -- Footer lines: split timing data across 4 lines for readability
            let fmt := fun (v : Float) => s!"{(v * 10).toUInt32.toFloat / 10}"  -- 1 decimal place
            let canopyStatsStr := match rs.canopyStats with
              | some stats => s!"  |  Subs: {stats.scopeSubscriptionCount}"
              | none => ""

            -- Line 1: FPS, Commands, Widgets, Memory
            let footerLine1 :=
              s!"FPS: {rs.displayFps.toUInt32}  |  Commands: {rs.renderCommandCount}  |  Widgets: {rs.widgetCount}  |  Mem: {memMb}MB  |  Cache: {cacheRate}%{canopyStatsStr}"

            -- Line 2: Draw Calls with batch breakdown
            let totalBatched := rs.rectsBatched + rs.circlesBatched + rs.strokeRectsBatched + rs.linesBatched + rs.textsBatched
            let avgBatchSize := if rs.batchedCalls > 0 then (totalBatched * 10 / rs.batchedCalls).toFloat / 10.0 else 0.0
            let avgBatchStr := s!"{(avgBatchSize * 10).toUInt32.toFloat / 10}"
            let batchBreakdown := s!"Rects:{rs.rectsBatched} Circles:{rs.circlesBatched} StrokeRects:{rs.strokeRectsBatched} Lines:{rs.linesBatched} Texts:{rs.textsBatched}"
            let footerLine2 :=
              s!"Draw Calls: {totalDrawCalls} (Batched: {rs.batchedCalls}, Avg: {avgBatchStr})  |  {batchBreakdown}"

            -- Line 3: Main timing phases (Present = time waiting in beginFrame for vsync/GPU)
            let footerLine3 :=
              s!"Timing: Update {fmt rs.timeUpdateMs}ms, Build {fmt rs.timeBuildMs}ms, Layout {fmt rs.timeLayoutMs}ms, Collect {fmt rs.timeCollectMs}ms, GPU {fmt rs.timeGpuMs}ms, Present {fmt lastPresentMs}ms"

            -- Line 4: GPU breakdown detail + Collect detail
            let gpuDetail := s!"GPU Detail: Flatten {fmt rs.timeFlattenMs}ms, Coalesce {fmt rs.timeCoalesceMs}ms, BatchLoop {fmt rs.timeBatchLoopMs}ms, DrawCalls {fmt rs.timeDrawCallsMs}ms"
            let collectDetailStr :=
              if rs.collectLookupCount == 0 && rs.collectTouchCount == 0 &&
                 rs.collectEmitAllCount == 0 && rs.collectSpecCount == 0 &&
                 rs.collectInsertCount == 0 then
                ""
              else
                s!"  |  Collect: L:{fmt rs.collectLookupMs}({rs.collectLookupCount})" ++
                s!" T:{fmt rs.collectTouchMs}({rs.collectTouchCount})" ++
                s!" E:{fmt rs.collectEmitAllMs}({rs.collectEmitAllCount})" ++
                s!" S:{fmt rs.collectSpecMs}({rs.collectSpecCount})" ++
                s!" I:{fmt rs.collectInsertMs}({rs.collectInsertCount})"
            let footerLine4 := s!"{gpuDetail}{collectDetailStr}"

            let buildDemoWidget := fun (tabBar : TabBarResult) (demo : AnyDemo)
                (envForView : DemoEnv) =>
              match AnyDemo.view? demo envForView with
              | some builder => Afferent.Arbor.buildFrom tabBar.finalId builder
              | none => Afferent.Arbor.buildFrom tabBar.finalId (Afferent.Arbor.spacer 0 0)

            let buildRoot := fun (tabBar : TabBarResult) (demo : AnyDemo)
                (envForView : DemoEnv) =>
              buildRootWidget tabBar (buildDemoWidget tabBar demo envForView)
                footerLine1 footerLine2 footerLine3 footerLine4 rs.assets.fontPack.smallId s

            let measureRoot := fun (root : Afferent.Arbor.Widget) => do
              let measureResult ← runWithFonts rs.assets.fontPack.registry
                (Afferent.Arbor.measureWidget root screenW screenH)
              let layouts := Trellis.layout measureResult.node screenW screenH
              pure (measureResult.widget, layouts)

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
            let contentLayoutForEnv := match rs.frameCache with
              | some cache =>
                  (cache.layouts.get cache.rootBuild.contentId).getD defaultLayout
              | none => defaultLayout
            let envForView := envFromLayout contentLayoutForEnv t dt keyCode c.clearKey
            -- Timing: Update phase (FRP propagation for Canopy demos)
            let tUpdate0 ← IO.monoMsNow
            let canopyStats ← match rs.demoRefs[rs.displayMode]? with
            | some demoRef => do
                let demo ← demoRef.get
                let demo' ← AnyDemo.update demo envForView
                demoRef.set demo'
                -- Extract canopy stats from updated demo
                pure (getCanopyStats demo')
            | none => pure none
            rs := { rs with canopyStats := canopyStats }
            let tUpdate1 ← IO.monoMsNow

            let buildRootBuild : IO RootBuild := do
              match rs.demoRefs[rs.displayMode]? with
              | some demoRef => do
                  let demo ← demoRef.get
                  pure (buildRoot rs.tabBar demo envForView)
              | none =>
                  pure (buildRootWidget rs.tabBar (.spacer rs.tabBar.finalId none 0 0)
                    footerLine1 footerLine2 footerLine3 footerLine4 rs.assets.fontPack.smallId s)
            let mut tabSwitched := false

            match rs.frameCache with
            | some cache =>
                let mut clickedTab : Option Nat := none
                let mut demoClickPath : Option (Array Afferent.Arbor.WidgetId) := none
                match click with
                | some ce =>
                    let hitPath := Afferent.Arbor.hitTestPathIndexed cache.hitIndex ce.x ce.y
                    let inTabBar := hitPath.any (· == cache.rootBuild.tabBar.rowId)
                    if inTabBar then
                      clickedTab := cache.rootBuild.tabBar.tabIds.findIdx? (fun tabId => hitPath.any (· == tabId))
                    else
                      demoClickPath := some hitPath
                | none => pure ()

                match clickedTab with
                | some newTabIdx =>
                    if newTabIdx != rs.displayMode then
                      let contentLayout :=
                        (cache.layouts.get cache.rootBuild.contentId).getD defaultLayout
                      let exitEnv := envFromLayout contentLayout 0.0 0.0 keyCode c.clearKey
                      match rs.demoRefs[rs.displayMode]? with
                      | some demoRef =>
                          let currentDemo ← demoRef.get
                          let nextDemo ← AnyDemo.onExit currentDemo c exitEnv
                          demoRef.set nextDemo
                      | none => pure ()
                      rs := { rs with displayMode := newTabIdx, lastHoverPath := #[] }
                      c := c.resetState
                      c.ctx.resetScissor
                      let newTabBar := rebuildTabBar rs.demos newTabIdx rs.assets.fontPack.smallId s
                      rs := { rs with tabBar := newTabBar }
                      IO.println s!"Switched to {AnyDemo.name (rs.demos[rs.displayMode]!)}"
                      tabSwitched := true
                | none => pure ()

                if !tabSwitched then
                  match demoClickPath with
                  | some hitPath =>
                      match click with
                      | some ce =>
                          let contentLayout :=
                            (cache.layouts.get cache.rootBuild.contentId).getD defaultLayout
                          let clickEnv := envFromLayout contentLayout t dt keyCode c.clearKey
                          match rs.demoRefs[rs.displayMode]? with
                          | some demoRef =>
                              let demo ← demoRef.get
                              let demo' ← AnyDemo.handleClickWithLayouts demo clickEnv cache.rootBuild.contentId hitPath ce cache.layouts cache.measuredWidget
                              demoRef.set demo'
                          | none => pure ()
                      | none => pure ()
                  | none => pure ()

                  let (mouseX, mouseY) ← FFI.Window.getMousePos c.ctx.window
                  let mut hoverPathOpt : Option (Array Afferent.Arbor.WidgetId) := none
                  let mouseMoved := mouseX != rs.lastMouseX || mouseY != rs.lastMouseY
                  if mouseMoved then
                    let hoverPath := Afferent.Arbor.hitTestPathIndexed cache.hitIndex mouseX mouseY
                    hoverPathOpt := some hoverPath
                    let inTabBar := hoverPath.any (· == cache.rootBuild.tabBar.rowId)
                    if !inTabBar then
                      let contentLayout :=
                        (cache.layouts.get cache.rootBuild.contentId).getD defaultLayout
                      let hoverEnv := envFromLayout contentLayout t dt keyCode c.clearKey
                      match rs.demoRefs[rs.displayMode]? with
                      | some demoRef =>
                          let demo ← demoRef.get
                          let demo' ← AnyDemo.handleHoverWithLayouts demo hoverEnv cache.rootBuild.contentId hoverPath mouseX mouseY cache.layouts cache.measuredWidget
                          demoRef.set demo'
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
                      (cache.layouts.get cache.rootBuild.contentId).getD defaultLayout
                    let keyEnv := envFromLayout contentLayout t dt keyCode c.clearKey
                    match rs.demoRefs[rs.displayMode]? with
                    | some demoRef =>
                        let demo ← demoRef.get
                        let demo' ← AnyDemo.handleKey demo keyEnv keyEvent
                        demoRef.set demo'
                        c.clearKey
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
                      (cache.layouts.get cache.rootBuild.contentId).getD defaultLayout
                    let scrollEnv := envFromLayout contentLayout t dt keyCode c.clearKey
                    match rs.demoRefs[rs.displayMode]? with
                    | some demoRef =>
                        let demo ← demoRef.get
                        let scrollPath := match hoverPathOpt with
                          | some path => path
                          | none => Afferent.Arbor.hitTestPathIndexed cache.hitIndex mouseX mouseY
                        let demo' ← AnyDemo.handleScrollWithLayouts demo scrollEnv scrollPath scrollEvent cache.layouts cache.measuredWidget
                        demoRef.set demo'
                    | none => pure ()
                    FFI.Window.clearScroll c.ctx.window

                  -- Handle mouse button release (for ending drag interactions)
                  let buttons ← FFI.Window.getMouseButtons c.ctx.window
                  let leftDown := (buttons &&& (1 : UInt8)) != (0 : UInt8)
                  if !leftDown && rs.prevLeftDown then
                    let contentLayout :=
                      (cache.layouts.get cache.rootBuild.contentId).getD defaultLayout
                    let mouseUpEnv := envFromLayout contentLayout t dt keyCode c.clearKey
                    match rs.demoRefs[rs.displayMode]? with
                    | some demoRef =>
                        let demo ← demoRef.get
                        let mouseUpPath := match hoverPathOpt with
                          | some path => path
                          | none => Afferent.Arbor.hitTestPathIndexed cache.hitIndex mouseX mouseY
                        let demo' ← AnyDemo.handleMouseUpWithLayouts demo mouseUpEnv mouseX mouseY mouseUpPath cache.layouts cache.measuredWidget
                        demoRef.set demo'
                    | none => pure ()
                  rs := { rs with prevLeftDown := leftDown }
            | none =>
                let buttons ← FFI.Window.getMouseButtons c.ctx.window
                let leftDown := (buttons &&& (1 : UInt8)) != (0 : UInt8)
                rs := { rs with prevLeftDown := leftDown }

            -- Timing: Build phase (widget tree construction)
            let tBuild0 ← IO.monoMsNow
            let rootBuild ← buildRootBuild
            let tBuild1 ← IO.monoMsNow
            -- Timing: Layout phase (measurement and layout calculation)
            let tLayout0 ← IO.monoMsNow
            let (measuredWidget, layouts) ← measureRoot rootBuild.widget
            let tLayout1 ← IO.monoMsNow
            let hitIndex := Afferent.Arbor.buildHitTestIndex measuredWidget layouts
            rs := { rs with
              frameCache := some {
                rootBuild := rootBuild
                measuredWidget := measuredWidget
                layouts := layouts
                hitIndex := hitIndex
              }
            }

            -- Timing: Collect phase (render command generation)
            let collectMetricsOpt ← Afferent.Arbor.getCollectMetrics
            match collectMetricsOpt with
            | some metrics => Afferent.Arbor.CollectMetrics.reset metrics
            | none => pure ()
            let tCollect0 ← IO.monoMsNow
            let (commands, cacheHits, cacheMisses) ← Afferent.Arbor.collectCommandsCachedWithStats c.renderCache measuredWidget layouts
            let tCollect1 ← IO.monoMsNow
            let collectSnapOpt ← match collectMetricsOpt with
            | some metrics => do
                let snap ← Afferent.Arbor.CollectMetrics.snapshot metrics
                pure (some snap)
            | none => pure none
            let renderCache ← c.renderCache.get
            let cacheSize := renderCache.size
            let cacheCapacity := renderCache.capacity
            let widgetCount := Afferent.Arbor.Widget.widgetCount measuredWidget
            rs := { rs with
              renderCommandCount := commands.size
              widgetCount := widgetCount
              cacheHits := cacheHits
              cacheMisses := cacheMisses
              cacheSize := cacheSize
              cacheCapacity := cacheCapacity
            }
            let nanosToMs := fun (n : Nat) => n.toFloat / 1000000.0
            let (lookupMs, lookupCount, touchMs, touchCount, emitAllMs, emitAllCount, specMs, specCount, insertMs, insertCount) :=
              match collectSnapOpt with
              | some snap =>
                  (nanosToMs snap.lookupNanos, snap.lookupCount,
                   nanosToMs snap.touchNanos, snap.touchCount,
                   nanosToMs snap.emitAllNanos, snap.emitAllCount,
                   nanosToMs snap.collectNanos, snap.collectCount,
                   nanosToMs snap.insertNanos, snap.insertCount)
              | none =>
                  (0.0, 0, 0.0, 0, 0.0, 0, 0.0, 0, 0.0, 0)
            rs := { rs with
              collectLookupMs := lookupMs
              collectLookupCount := lookupCount
              collectTouchMs := touchMs
              collectTouchCount := touchCount
              collectEmitAllMs := emitAllMs
              collectEmitAllCount := emitAllCount
              collectSpecMs := specMs
              collectSpecCount := specCount
              collectInsertMs := insertMs
              collectInsertCount := insertCount
            }
            -- Timing: GPU phase (batching and draw calls)
            let tGpu0 ← IO.monoMsNow
            let (batchStats, c') ← CanvasM.run c do
              let stats ← Afferent.Widget.executeCommandsBatchedWithStats rs.assets.fontPack.registry commands
              Afferent.Widget.renderCustomWidgets measuredWidget layouts
              return stats
            c := c'
            let tGpu1 ← IO.monoMsNow
            rs := { rs with batchedCalls := batchStats.batchedCalls, individualCalls := batchStats.individualCalls, rectsBatched := batchStats.rectsBatched, circlesBatched := batchStats.circlesBatched, strokeRectsBatched := batchStats.strokeRectsBatched, linesBatched := batchStats.linesBatched, textsBatched := batchStats.textsBatched, timeFlattenMs := batchStats.timeFlattenMs, timeCoalesceMs := batchStats.timeCoalesceMs, timeBatchLoopMs := batchStats.timeBatchLoopMs, timeDrawCallsMs := batchStats.timeDrawCallsMs }
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
            lastWorkEndTime ← IO.monoMsNow

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

end Demos
