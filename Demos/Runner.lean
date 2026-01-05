/-
  Demo Runner - Main orchestration for all demos
-/
import Afferent
import Demos.Overview.Text
import Demos.Demo
import Demos.DemoRegistry
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

  -- Kick an initial frame so the window isn't blank during heavy startup work.
  let kickInitialFrame (attempts : Nat) : IO Unit := do
    for _ in [:attempts] do
      canvas.pollEvents
      let ok ← canvas.beginFrame Color.darkGray
      if ok then
        let c' ← run' canvas do
          resetTransform
          let (w, h) ← getCurrentSize
          let barW := min (w * 0.6) (360.0 * screenScale)
          let barH := max (h * 0.02) (12.0 * screenScale)
          let x := (w - barW) / 2.0
          let y := (h - barH) / 2.0
          setFillColor (Color.gray 0.2)
          fillRectXYWH x y barW barH
          setFillColor (Color.gray 0.7)
          fillRectXYWH x y (barW * 0.35) barH
        discard (c'.endFrame)
        return
      else
        IO.sleep 10
  kickInitialFrame 50

  IO.println "Loading fonts..."
  -- Font sizes scaled for physical resolution
  let fontSmall ← Font.load "/System/Library/Fonts/Monaco.ttf" (16 * screenScale).toUInt32
  let fontMedium ← Font.load "/System/Library/Fonts/Monaco.ttf" (24 * screenScale).toUInt32
  let fontLarge ← Font.load "/System/Library/Fonts/Monaco.ttf" (36 * screenScale).toUInt32
  let fontHuge ← Font.load "/System/Library/Fonts/Monaco.ttf" (48 * screenScale).toUInt32

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

  -- No GPU upload needed! Dynamic module sends positions each frame.
  IO.println "Using unified Dynamic rendering - CPU positions, GPU color/NDC."

  -- Orbital instanced demo parameters (CPU orbit, GPU instancing)
  let orbitalCount : Nat := 50000
  let minRadius : Float := 20.0 * screenScale
  let maxRadius : Float := (min physWidthF physHeightF) * 0.45
  let speedMin : Float := 0.3
  let speedMax : Float := 1.6
  let sizeMin : Float := 1.0 * screenScale
  let sizeMax : Float := 3.5 * screenScale
  let twoPi : Float := 6.283185307
  let orbitalParams : FloatArray := Id.run do
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
  let orbitalBuffer ← FFI.FloatBuffer.create (orbitalCount.toUSize * 8)
  IO.println s!"Prepared {orbitalCount} orbital instances"

  -- Layout demo labels are drawn in screen pixels (not scaled with the demo), so size is stable.
  let layoutLabelPt : Float := 12.0
  let layoutFontPx : UInt32 := (max 8.0 (layoutLabelPt * screenScale)).toUInt32
  let layoutFont ← Font.load "/System/Library/Fonts/Monaco.ttf" layoutFontPx

  -- Create font registry for Arbor widget system
  let (fontReg1, fontSmallId) := FontRegistry.empty.register fontSmall "small"
  let (fontReg2, fontMediumId) := fontReg1.register fontMedium "medium"
  let (fontReg3, fontLargeId) := fontReg2.register fontLarge "large"
  let (fontReg4, fontHugeId) := fontReg3.register fontHuge "huge"
  let fontRegistry := fontReg4.setDefault fontMedium

  -- Display modes: 0 = demo, 1 = grid squares, 2 = triangles, 3 = circles, 4 = sprites, 16 = lines, 17 = texture matrix, 18 = orbital instanced
  let startTime ← IO.monoMsNow
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

  let mkEnv := fun (t dt : Float) (keyCode : UInt16) => {
    screenScale := screenScale
    t := t
    dt := dt
    keyCode := keyCode
    fontSmall := fontSmall
    fontMedium := fontMedium
    fontLarge := fontLarge
    fontHuge := fontHuge
    layoutFont := layoutFont
    fontRegistry := fontRegistry
    fontMediumId := fontMediumId
    fontSmallId := fontSmallId
    fontLargeId := fontLargeId
    fontHugeId := fontHugeId
    spriteTexture := spriteTexture
    halfSize := halfSize
    circleRadius := circleRadius
    spriteHalfSize := spriteHalfSize
    gridParticles := gridParticles
    lineBuffer := lineBuffer
    lineCount := lineCount
    lineWidth := lineWidth
    orbitalCount := orbitalCount
    orbitalParams := orbitalParams
    orbitalBuffer := orbitalBuffer
    physWidthF := physWidthF
    physHeightF := physHeightF
    physWidth := physWidth
    physHeight := physHeight
    layoutOffsetX := layoutOffsetX
    layoutOffsetY := layoutOffsetY
    layoutScale := layoutScale
  }

  let renderLoop : IO Canvas := do
    let initEnv := mkEnv 0.0 0.0 0
    let mut demos ← buildDemoList initEnv
    let mut displayMode : Nat := startMode % demos.size
    let mut c := canvas
    let mut msaaEnabled : Bool := AnyDemo.msaaEnabled (demos[displayMode]!)
    FFI.Renderer.setMSAAEnabled c.ctx.renderer msaaEnabled
    let mut lastTime := startTime
    -- FPS counter (smoothed over multiple frames)
    let mut frameCount : Nat := 0
    let mut fpsAccumulator : Float := 0.0
    let mut displayFps : Float := 0.0
    let mut framesLeft : Nat := exitAfterFrames

    while !(← c.shouldClose) do
      -- Check for Space key to cycle through modes
      let keyCode ← c.getKeyCode
      if keyCode == FFI.Key.space then
        let exitEnv := mkEnv 0.0 0.0 keyCode
        let currentDemo ← AnyDemo.onExit (demos[displayMode]!) c exitEnv
        demos := demos.set! displayMode currentDemo
        displayMode := (displayMode + 1) % demos.size
        c.clearKey
        c := c.resetState
        c.ctx.resetScissor
        msaaEnabled := AnyDemo.msaaEnabled (demos[displayMode]!)
        FFI.Renderer.setMSAAEnabled c.ctx.renderer msaaEnabled
        IO.println s!"Switched to {AnyDemo.name (demos[displayMode]!)}"

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

        let env := mkEnv t dt keyCode
        let demo := demos[displayMode]!
        let (c', nextDemo) ← AnyDemo.step demo c env
        c := c'
        demos := demos.set! displayMode nextDemo

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
    pure c

  let renderTask ← IO.asTask (prio := .dedicated) renderLoop
  canvas.ctx.window.runEventLoop
  let c ← match renderTask.get with
    | .ok c => pure c
    | .error err => throw err

  IO.println "Cleaning up..."
  fontSmall.destroy
  fontMedium.destroy
  fontLarge.destroy
  fontHuge.destroy
  layoutFont.destroy
  FFI.Buffer.destroy lineBuffer
  FFI.FloatBuffer.destroy orbitalBuffer
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
