/-
  Texture Matrix Demo
  Demonstrates scaling a sprite via the u_matrix transform.
-/
import Afferent

open Afferent CanvasM

namespace Demos

def renderTextureMatrixDemoFrame (c : Canvas) (t : Float) (screenScale : Float)
    (fontMedium fontSmall : Font) (texture : FFI.Texture) : IO Canvas := do
  run' c do
    resetTransform
    setFillColor Color.white
    fillTextXY "Texture Matrix Demo (u_matrix scaling) (Space to advance)"
      (20 * screenScale) (30 * screenScale) fontMedium
    let renderer ← getRenderer
    let (wF, hF) ← getCurrentSize
    let baseHalf := 48.0 * screenScale
    let pivotX := wF * 0.65
    let pivotY := hF * 0.55
    let scale := 1.0 + 0.5 * Float.sin t
    let a := 2.0 / wF
    let d := -2.0 / hF
    let tx := -1.0
    let ty := 1.0
    let a' := a * scale
    let d' := d * scale
    let tx' := tx + a * (1.0 - scale) * pivotX
    let ty' := ty + d * (1.0 - scale) * pivotY
    let refX := wF * 0.35
    let refY := pivotY
    let refData : Array Float := #[refX, refY, 0.0, baseHalf, 1.0]
    let scaledData : Array Float := #[pivotX, pivotY, 0.0, baseHalf, 1.0]
    FFI.Renderer.drawSprites renderer texture refData 1 wF hF
    FFI.Renderer.drawSpritesMatrix renderer texture scaledData 1 wF hF a' 0.0 0.0 d' tx' ty'
    setFillColor Color.white
    fillTextXY "left: screen-space  |  right: u_matrix scale"
      (20 * screenScale) (60 * screenScale) fontSmall

end Demos
