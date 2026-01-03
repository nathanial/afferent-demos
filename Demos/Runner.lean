/-
  Demo Runner - Main orchestration for all demos
-/
import Afferent
import Demos.Shapes
import Demos.Transforms
import Demos.Strokes
import Demos.Gradients
import Demos.Text
import Demos.Animations
import Demos.Layout
import Demos.Grid
import Demos.Collimator
import Demos.GridPerf
import Demos.TrianglesPerf
import Demos.CirclesPerf
import Demos.SpritesPerf
import Demos.LinesPerf
import Demos.Widgets
import Demos.Interactive
import Demos.SpinningCubes
import Demos.DemoGrid
import Demos.Seascape
import Demos.PathFeatures
import Demos.ShapeGallery
import Demos.LineCaps
import Demos.DashedLines
import Worldmap
import Wisp

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

  IO.println "Loading fonts..."
  -- Font sizes scaled for physical resolution
  let fontSmall ← Font.load "/System/Library/Fonts/Monaco.ttf" (16 * screenScale).toUInt32
  let fontMedium ← Font.load "/System/Library/Fonts/Monaco.ttf" (24 * screenScale).toUInt32
  let fontLarge ← Font.load "/System/Library/Fonts/Monaco.ttf" (36 * screenScale).toUInt32
  let fontHuge ← Font.load "/System/Library/Fonts/Monaco.ttf" (48 * screenScale).toUInt32
  let fonts : Fonts := { small := fontSmall, medium := fontMedium, large := fontLarge, huge := fontHuge }

  IO.println "Loading sprite texture..."
  let spriteTexture ← FFI.Texture.load "nibble.png"
  let (texWidth, texHeight) ← FFI.Texture.getSize spriteTexture
  IO.println s!"Loaded nibble.png: {texWidth}x{texHeight}"

  IO.println "Rendering animated demo... (close window to exit)"
  IO.println "Press SPACE to toggle performance test mode (10000 spinning squares)"

  -- Pre-compute particle data ONCE at startup using unified Dynamic module
  -- Sizes scaled for physical resolution
  let halfSize := 1.5 * screenScale
  let circleRadius := 2.0 * screenScale
  let spriteHalfSize := 15.0 * screenScale  -- Size for sprite rendering

  -- Physical dimensions as floats
  let physWidthF := baseWidth * screenScale
  let physHeightF := baseHeight * screenScale

  -- Precompute layout-demo fit so mode 5 can scale uniformly and keep text readable.
  let layoutW : Float := 1000.0
  let layoutH : Float := 800.0
  let layoutPadTop : Float := 60.0 * screenScale
  let layoutAvailW : Float := physWidthF
  let layoutAvailH : Float := max 1.0 (physHeightF - layoutPadTop)
  let layoutScale : Float := min (layoutAvailW / layoutW) (layoutAvailH / layoutH)
  let layoutOffsetX : Float := (layoutAvailW - layoutW * layoutScale) / 2.0
  let layoutOffsetY : Float := layoutPadTop + (layoutAvailH - layoutH * layoutScale) / 2.0

  -- Grid particles (316x316 ≈ 100k grid of spinning squares/triangles)
  let gridCols := 316 * 3
  let gridRows := 316 * 2
  let gridSpacing := 2.0 * screenScale
  let gridStartX := (physWidthF - (gridCols.toFloat - 1) * gridSpacing) / 2.0
  let gridStartY := (physHeightF - (gridRows.toFloat - 1) * gridSpacing) / 2.0
  let gridParticles := Render.Dynamic.ParticleState.createGrid gridCols gridRows gridStartX gridStartY gridSpacing physWidthF physHeightF
  IO.println s!"Created {gridParticles.count} grid particles"

  -- Line segments for 100k-line GPU stroke perf
  let (lineSegments, lineCount) := Demos.buildLineSegments physWidthF physHeightF
  let lineWidth := 1.0 * screenScale
  let lineBuffer ← FFI.Buffer.createStrokeSegmentPersistent canvas.ctx.renderer lineSegments
  IO.println s!"Prepared {lineCount} line segments"

  -- Bouncing circles using Dynamic.ParticleState
  let bouncingParticles := Render.Dynamic.ParticleState.create 1000000 physWidthF physHeightF 42
  IO.println s!"Created {bouncingParticles.count} bouncing circles"

  -- Sprite particles for Bunnymark-style benchmark (Lean physics, FloatBuffer rendering)
  let spriteParticles := Render.Dynamic.ParticleState.create 1000000 physWidthF physHeightF 123
  let spriteBuffer ← FFI.FloatBuffer.create (spriteParticles.count.toUSize * 5)  -- 5 floats per sprite
  let circleBuffer ← FFI.FloatBuffer.create (bouncingParticles.count.toUSize * 4)  -- 4 floats per circle
  IO.println s!"Created {spriteParticles.count} bouncing sprites (Lean physics, FloatBuffer rendering)"

  -- No GPU upload needed! Dynamic module sends positions each frame.
  IO.println "Using unified Dynamic rendering - CPU positions, GPU color/NDC."

  -- Layout demo labels are drawn in screen pixels (not scaled with the demo), so size is stable.
  let layoutLabelPt : Float := 12.0
  let layoutFontPx : UInt32 := (max 8.0 (layoutLabelPt * screenScale)).toUInt32
  let layoutFont ← Font.load "/System/Library/Fonts/Monaco.ttf" layoutFontPx

  -- Create font registry for Arbor widget system
  let (fontReg1, fontSmallId) := FontRegistry.empty.register fontSmall "small"
  let (fontReg2, fontMediumId) := fontReg1.register fontMedium "medium"
  let fontRegistry := fontReg2.setDefault fontMedium

  -- Display modes: 0 = demo, 1 = grid squares, 2 = triangles, 3 = circles, 4 = sprites, 16 = lines
  let startTime ← IO.monoMsNow
  let mut c := canvas
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

  let mut displayMode : Nat := startMode % 17
  let mut msaaEnabled : Bool := true
  let mut lastTime := startTime
  let mut bouncingState := bouncingParticles
  let mut spriteState := spriteParticles
  -- FPS counter (smoothed over multiple frames)
  let mut frameCount : Nat := 0
  let mut fpsAccumulator : Float := 0.0
  let mut displayFps : Float := 0.0
  -- FPS camera for 3D demo (mode 9)
  let mut fpsCamera : Render.FPSCamera := default
  -- Seascape camera (mode 10)
  let mut seascapeCamera : Render.FPSCamera := Demos.seascapeCamera
  let mut framesLeft : Nat := exitAfterFrames
  -- Interactive counter demo state (mode 8)
  let mut counterState : Demos.CounterState := Demos.CounterState.initial
  -- Shape gallery state (mode 12)
  let mut shapeGalleryIndex : Nat := 0

  -- Worldmap state (mode 13)
  let diskConfig : Worldmap.TileDiskCacheConfig := {
    cacheDir := "./tile_cache"
    tilesetName := "carto-dark-2x"
    maxSizeBytes := Worldmap.defaultDiskCacheSizeBytes
  }
  let mut mapState ← Worldmap.MapState.init 37.7749 (-122.4194) 12 (physWidth.toNat : Int) (physHeight.toNat : Int) diskConfig

  while !(← c.shouldClose) do
    c.pollEvents

    -- Check for Space key to cycle through modes
    let keyCode ← c.getKeyCode
    if keyCode == FFI.Key.space then
      -- Release pointer lock when leaving mode 9 or 10
      if displayMode == 9 || displayMode == 10 then
        FFI.Window.setPointerLock c.ctx.window false
      displayMode := (displayMode + 1) % 17
      c.clearKey
      c := c.resetState
      c.ctx.resetScissor
      -- Disable MSAA for throughput-heavy benchmarks and the seascape demo.
      -- (Seascape is usually fill-rate bound; MSAA can be a big hit at Retina resolutions.)
      msaaEnabled := displayMode != 4 && displayMode != 10 && displayMode != 16
      FFI.Renderer.setMSAAEnabled c.ctx.renderer msaaEnabled
      match displayMode with
      | 0 => IO.println "Switched to DEMO mode"
      | 1 => IO.println "Switched to GRID (squares) performance test"
      | 2 => IO.println "Switched to TRIANGLES performance test"
      | 3 => IO.println "Switched to CIRCLES (bouncing) performance test"
      | 4 => IO.println "Switched to SPRITES (Bunnymark) performance test"
      | 5 => IO.println "Switched to LAYOUT demo (full-size)"
      | 6 => IO.println "Switched to CSS GRID demo (full-size)"
      | 7 => IO.println "Switched to WIDGET demo (full-size)"
      | 8 => IO.println "Switched to INTERACTIVE demo (click the buttons!)"
      | 9 => IO.println "Switched to 3D SPINNING CUBES demo"
      | 10 => IO.println "Switched to SEASCAPE demo (Gerstner waves)"
      | 11 => IO.println "Switched to PATH FEATURES demo (non-convex, arcTo, transforms)"
      | 12 => IO.println "Switched to SHAPE GALLERY (arrow keys to navigate)"
      | 13 => IO.println "Switched to WORLDMAP demo (drag to pan, scroll to zoom)"
      | 14 => IO.println "Switched to LINE CAPS & JOINS demo"
      | 15 => IO.println "Switched to DASHED LINES demo"
      | _ => IO.println "Switched to 100k LINES performance test"

    -- Arrow key navigation for shape gallery (mode 12)
    if displayMode == 12 then
      if keyCode == FFI.Key.right then
        shapeGalleryIndex := (shapeGalleryIndex + 1) % Demos.shapeGalleryCount
        c.clearKey
      else if keyCode == FFI.Key.left then
        shapeGalleryIndex := if shapeGalleryIndex == 0 then Demos.shapeGalleryCount - 1 else shapeGalleryIndex - 1
        c.clearKey

    let ok ← c.beginFrame Color.darkGray
    if ok then
      let now ← IO.monoMsNow
      let t := (now - startTime).toFloat / 1000.0  -- Elapsed seconds
      let dt := (now - lastTime).toFloat / 1000.0  -- Delta time
      lastTime := now

      -- Update FPS counter (update display every 10 frames for stability)
      frameCount := frameCount + 1
      if dt > 0.0 then
        fpsAccumulator := fpsAccumulator + (1.0 / dt)
      if frameCount >= 10 then
        displayFps := fpsAccumulator / frameCount.toFloat
        fpsAccumulator := 0.0
        frameCount := 0

      if displayMode == 1 then
        -- Grid performance test: squares spinning in a grid
        c ← run' c do
          resetTransform
          renderGridTestM t fontMedium gridParticles halfSize
      else if displayMode == 2 then
        -- Triangle performance test: triangles spinning in a grid
        c ← run' c do
          resetTransform
          renderTriangleTestM t fontMedium gridParticles halfSize
      else if displayMode == 3 then
        -- Circle performance test: bouncing circles
        bouncingState ← bouncingState.updateBouncingAndWriteCircles dt circleRadius circleBuffer
        c ← run' c do
          resetTransform
          setFillColor Color.white
          fillTextXY s!"Circles: {bouncingState.count} dynamic circles [fused] (Space to advance)" (20 * screenScale) (30 * screenScale) fontMedium
          let renderer ← getRenderer
          Render.Dynamic.drawCirclesFromBuffer renderer circleBuffer bouncingState.count.toUInt32 t bouncingState.screenWidth bouncingState.screenHeight
      else if displayMode == 4 then
        -- Sprite performance test: bouncing textured sprites (Bunnymark)
        -- Physics runs in Lean, rendering uses FloatBuffer for zero-copy GPU upload
        spriteState ← spriteState.updateBouncingAndWriteSprites dt spriteHalfSize spriteBuffer
        c ← run' c do
          resetTransform
          setFillColor Color.white
          fillTextXY s!"Sprites: {spriteState.count} textured sprites [fused] (Space to advance)" (20 * screenScale) (30 * screenScale) fontMedium
          let renderer ← getRenderer
          Render.Dynamic.drawSpritesFromBuffer renderer spriteTexture spriteBuffer spriteState.count.toUInt32 spriteHalfSize spriteState.screenWidth spriteState.screenHeight
      else if displayMode == 5 then
        -- Full-size Layout demo
        c ← run' c do
          resetTransform
          save
          translate layoutOffsetX layoutOffsetY
          scale layoutScale layoutScale
          renderLayoutShapesM
          restore
          -- Draw labels in screen space to avoid texture upscaling artifacts.
          renderLayoutLabelsMappedM layoutFont layoutOffsetX layoutOffsetY layoutScale
          setFillColor Color.white
          fillTextXY "CSS Flexbox Layout Demo (Space to advance)" (20 * screenScale) (30 * screenScale) fontMedium
      else if displayMode == 6 then
        -- Full-size CSS Grid demo
        c ← run' c do
          resetTransform
          save
          translate layoutOffsetX layoutOffsetY
          scale layoutScale layoutScale
          renderGridShapesM
          restore
          -- Draw labels in screen space to avoid texture upscaling artifacts.
          renderGridLabelsMappedM layoutFont layoutOffsetX layoutOffsetY layoutScale
          setFillColor Color.white
          fillTextXY "CSS Grid Layout Demo (Space to advance)" (20 * screenScale) (30 * screenScale) fontMedium
      else if displayMode == 7 then
        -- Widget system demo (using Arbor)
        c ← run' c do
          resetTransform
          renderWidgetShapesDebugM fontRegistry fontMediumId fontSmallId physWidthF physHeightF screenScale
          setFillColor Color.white
          fillTextXY "Widget System Demo (Space to advance)" (20 * screenScale) (30 * screenScale) fontMedium
      else if displayMode == 8 then
        -- Interactive counter demo with click handling
        -- Check for clicks
        let click ← FFI.Window.getClick c.ctx.window
        match click with
        | some ce =>
          FFI.Window.clearClick c.ctx.window
          -- Prepare widget for hit testing
          let (widget, layouts, ids, offsetX, offsetY) ←
            Demos.prepareCounterForHitTest fontRegistry fontMediumId fontSmallId physWidthF physHeightF counterState screenScale
          -- Hit test at click position
          let hitId := Demos.hitTestCounter widget layouts offsetX offsetY ce.x ce.y
          -- Update state with widget IDs and process click
          counterState := { counterState with widgetIds := some ids }
          counterState := Demos.processClick counterState hitId
        | none => pure ()

        -- Render
        c ← run' c do
          resetTransform
          renderInteractiveDebugM fontRegistry fontMediumId fontSmallId physWidthF physHeightF counterState screenScale
          setFillColor Color.white
          fillTextXY "Interactive Counter Demo - Click the buttons! (Space to advance)" (20 * screenScale) (30 * screenScale) fontMedium
      else if displayMode == 9 then
        -- 3D Spinning Cubes demo with FPS camera controls
        -- Pointer lock controls:
        -- - Escape toggles capture/release
        -- - Left click captures (when not captured)
        let mut locked ← FFI.Window.getPointerLock c.ctx.window
        if keyCode == FFI.Key.escape then
          FFI.Window.setPointerLock c.ctx.window (!locked)
          locked := !locked
          c.clearKey
        else if !locked then
          -- Convenience: click to capture mouse so users don't have to discover Escape first.
          let click ← FFI.Window.getClick c.ctx.window
          match click with
          | some ce =>
            FFI.Window.clearClick c.ctx.window
            if ce.button == 0 then
              FFI.Window.setPointerLock c.ctx.window true
              locked := true
          | none => pure ()

        -- Check movement keys (WASD + Q/E) using continuous key state.
        -- Movement works even when not pointer-locked; mouse-look only when locked.
        let wDown ← FFI.Window.isKeyDown c.ctx.window FFI.Key.w
        let aDown ← FFI.Window.isKeyDown c.ctx.window FFI.Key.a
        let sDown ← FFI.Window.isKeyDown c.ctx.window FFI.Key.s
        let dDown ← FFI.Window.isKeyDown c.ctx.window FFI.Key.d
        let qDown ← FFI.Window.isKeyDown c.ctx.window FFI.Key.q
        let eDown ← FFI.Window.isKeyDown c.ctx.window FFI.Key.e

        let (dx, dy) ←
          if locked then
            FFI.Window.getMouseDelta c.ctx.window
          else
            pure (0.0, 0.0)

        -- Update camera (forward, back, left, right, up, down)
        fpsCamera := fpsCamera.update dt wDown sDown aDown dDown eDown qDown dx dy

        -- Render with camera (use current drawable size for correct aspect ratio on resize).
        c ← run' c do
          let (currentW, currentH) ← getCurrentSize
          let renderer ← getRenderer
          renderSpinningCubesWithCamera renderer t currentW currentH fpsCamera
          resetTransform
          setFillColor Color.white
          if locked then
            fillTextXY "3D Spinning Cubes - WASD+Q/E to move, mouse to look, Escape to release (Space to advance)" (20 * screenScale) (30 * screenScale) fontMedium
          else
            fillTextXY "3D Spinning Cubes - WASD+Q/E to move, click or Escape to capture mouse (Space to advance)" (20 * screenScale) (30 * screenScale) fontMedium

          -- Debug line (helps confirm input is arriving)
          fillTextXY s!"lock={locked} dt={dt} w={wDown} a={aDown} s={sDown} d={dDown} q={qDown} e={eDown} dx={dx} dy={dy} pos=({fpsCamera.x},{fpsCamera.y},{fpsCamera.z}) yaw={fpsCamera.yaw} pitch={fpsCamera.pitch}" (20 * screenScale) (55 * screenScale) fontSmall
      else if displayMode == 10 then
        -- Seascape demo with Gerstner waves and FPS camera
        let mut locked ← FFI.Window.getPointerLock c.ctx.window
        if keyCode == FFI.Key.escape then
          FFI.Window.setPointerLock c.ctx.window (!locked)
          locked := !locked
          c.clearKey
        else if !locked then
          let click ← FFI.Window.getClick c.ctx.window
          match click with
          | some ce =>
            FFI.Window.clearClick c.ctx.window
            if ce.button == 0 then
              FFI.Window.setPointerLock c.ctx.window true
              locked := true
          | none => pure ()

        -- Check movement keys (WASD + Q/E)
        let wDown ← FFI.Window.isKeyDown c.ctx.window FFI.Key.w
        let aDown ← FFI.Window.isKeyDown c.ctx.window FFI.Key.a
        let sDown ← FFI.Window.isKeyDown c.ctx.window FFI.Key.s
        let dDown ← FFI.Window.isKeyDown c.ctx.window FFI.Key.d
        let qDown ← FFI.Window.isKeyDown c.ctx.window FFI.Key.q
        let eDown ← FFI.Window.isKeyDown c.ctx.window FFI.Key.e

        let (dx, dy) ←
          if locked then
            FFI.Window.getMouseDelta c.ctx.window
          else
            pure (0.0, 0.0)

        -- Update seascape camera
        seascapeCamera := seascapeCamera.update dt wDown sDown aDown dDown eDown qDown dx dy

        -- Render seascape (use current drawable size for correct aspect ratio on resize).
        c ← run' c do
          let (currentW, currentH) ← getCurrentSize
          let renderer ← getRenderer
          renderSeascape renderer t currentW currentH seascapeCamera
          resetTransform
          setFillColor Color.white
          if locked then
            fillTextXY "Seascape - WASD+Q/E to move, mouse to look, Escape to release (Space to advance)" (20 * screenScale) (30 * screenScale) fontMedium
          else
            fillTextXY "Seascape - WASD+Q/E to move, click or Escape to capture mouse (Space to advance)" (20 * screenScale) (30 * screenScale) fontMedium

          fillTextXY
            (s!"pos=({seascapeCamera.x},{seascapeCamera.y},{seascapeCamera.z}) yaw={seascapeCamera.yaw} pitch={seascapeCamera.pitch}")
            (20 * screenScale) (55 * screenScale) fontSmall
      else if displayMode == 11 then
        -- Path Features demo: non-convex polygons, arcTo, transformed arcs
        c ← run' c do
          resetTransform
          renderPathFeaturesM screenScale fontSmall
          setFillColor Color.white
          fillTextXY "Path Features Demo - Non-convex, arcTo, transforms (Space to advance)" (20 * screenScale) (30 * screenScale) fontMedium
      else if displayMode == 12 then
        -- Shape Gallery: flip through labeled shapes with arrow keys
        c ← run' c do
          resetTransform
          renderShapeGalleryM shapeGalleryIndex physWidthF physHeightF screenScale fontLarge fontSmall
          setFillColor Color.white
          fillTextXY "Shape Gallery (Space to advance)" (20 * screenScale) (30 * screenScale) fontMedium
      else if displayMode == 13 then
        -- Worldmap demo: tile-based map viewer
        -- Keep viewport size in sync with the drawable (handles Retina + resize).
        let (w, h) ← FFI.Window.getSize c.ctx.window
        mapState := mapState.updateScreenSize w.toNat h.toNat
        -- Handle input (pan/zoom)
        mapState ← Worldmap.handleInput c.ctx.window mapState
        mapState := Worldmap.updateZoomAnimation mapState
        Worldmap.cancelStaleTasks mapState
        mapState ← Worldmap.updateTileCache mapState

        -- Render tiles
        -- Reset any leftover scissor from other demos before drawing tiles.
        c.ctx.resetScissor
        Worldmap.render c.ctx.renderer mapState

        -- Render UI overlay
        c ← run' c do
          resetTransform
          setFillColor Color.white
          fillTextXY "Worldmap Demo - drag to pan, scroll to zoom (Space to advance)" (20 * screenScale) (30 * screenScale) fontMedium
          -- Show coordinates
          let lat := mapState.viewport.centerLat
          let lon := mapState.viewport.centerLon
          let zoom := mapState.displayZoom
          fillTextXY s!"lat={lat} lon={lon} zoom={zoom}" (20 * screenScale) (55 * screenScale) fontSmall
      else if displayMode == 14 then
        -- Line Caps and Joins demo
        c ← run' c do
          resetTransform
          scale screenScale screenScale
          renderLineCapsM fontSmall
          setFillColor Color.white
          fillTextXY "Line Caps & Joins (Space to advance)" 20 30 fontMedium
      else if displayMode == 15 then
        -- Dashed Lines demo
        c ← run' c do
          resetTransform
          scale screenScale screenScale
          renderDashedLinesM fontSmall
          setFillColor Color.white
          fillTextXY "Dashed Lines (Space to advance)" 20 30 fontMedium
      else if displayMode == 16 then
        -- 100k Lines performance test (single draw call)
        c ← run' c do
          resetTransform
          renderLinesPerfM t lineBuffer lineCount lineWidth fontMedium
      else
        -- Normal demo mode: grid of demos using Trellis layout
        c ← run' c do
          resetTransform
          let (currentW, currentH) ← getCurrentSize
          renderDemoGridM screenScale currentW currentH fontSmall fonts t

      -- Render FPS counter in top-right corner (after all other rendering)
      -- Use current drawable size for proper positioning after resize
      let fpsText := s!"{displayFps.toUInt32} FPS"
      let (textWidth, _) ← fontSmall.measureText fpsText
      c ← run' c do
        resetTransform
        let (fpsW, _) ← getCurrentSize
        setFillColor (Color.hsva 0.0 0.0 0.0 0.6)
        fillRectXYWH (fpsW - textWidth - 20 * screenScale) (5 * screenScale) (textWidth + 15 * screenScale) (25 * screenScale)
        setFillColor Color.white
        fillTextXY fpsText (fpsW - textWidth - 12 * screenScale) (22 * screenScale) fontSmall

      c ← c.endFrame
      if framesLeft != 0 then
        framesLeft := framesLeft - 1
        if framesLeft == 0 then
          break

  IO.println "Cleaning up..."
  fontSmall.destroy
  fontMedium.destroy
  fontLarge.destroy
  fontHuge.destroy
  layoutFont.destroy
  FFI.Buffer.destroy lineBuffer
  canvas.destroy
  Wisp.FFI.globalCleanup
  Wisp.HTTP.Client.shutdown

/-- Main entry point - runs all demos -/
def main : IO Unit := do
  IO.println "Afferent - 2D Vector Graphics Library"
  IO.println "======================================"
  IO.println ""

  -- Run collimator demo first
  collimatorDemo

  -- Run unified visual demo (single window with all demos)
  unifiedDemo

  IO.println ""
  IO.println "Done!"

end Demos
