/-
  Orbital Instanced Demo
  Demonstrates CPU orbit updates with GPU instancing.
-/
import Afferent
import Init.Data.FloatArray

open Afferent CanvasM

namespace Demos

def renderOrbitalInstancedDemoFrame (c : Canvas) (t : Float) (screenScale : Float)
    (fontMedium : Font) (orbitalCount : Nat) (orbitalParams : FloatArray)
    (orbitalBuffer : FFI.FloatBuffer) : IO Canvas := do
  run' c do
    resetTransform
    setFillColor Color.white
    fillTextXY s!"Orbital: {orbitalCount} instanced rects (Space to advance)"
      (20 * screenScale) (30 * screenScale) fontMedium
    let renderer ← getRenderer
    let (wF, hF) ← getCurrentSize
    let centerX := wF * 0.5
    let centerY := hF * 0.5
    let a := 2.0 / wF
    let d := -2.0 / hF
    let tx := -1.0
    let ty := 1.0
    let sizeModeScreen : UInt32 := 1
    let colorModeHSV : UInt32 := 1
    let hueSpeed : Float := 0.2
    for i in [:orbitalCount] do
      let base := i * 5
      let phase := orbitalParams.get! base
      let radius := orbitalParams.get! (base + 1)
      let speed := orbitalParams.get! (base + 2)
      let hue := orbitalParams.get! (base + 3)
      let size := orbitalParams.get! (base + 4)
      let angle := phase + t * speed
      let x := centerX + radius * Float.cos angle
      let y := centerY + radius * Float.sin angle
      let rot := angle
      let bufIndex : USize := (i * 8).toUSize
      FFI.FloatBuffer.setVec8 orbitalBuffer bufIndex x y rot size hue 0.0 0.0 1.0
    FFI.Renderer.drawInstancedRectsBuffer
      renderer
      orbitalBuffer
      orbitalCount.toUInt32
      a 0.0 0.0 d tx ty
      wF hF
      sizeModeScreen
      t hueSpeed
      colorModeHSV

end Demos
